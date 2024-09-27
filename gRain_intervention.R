# Trying to understand intervention queries in gRain.

library(gRain)
library(doParallel)
library(foreach)


flat_eq_dag_chains <- flat_eq_dag_chains[seq(1, length(flat_eq_dag_chains$state), by = 100)]

cores <- 8
cl <- makeCluster(cores)  
registerDoParallel(cl)

# Finds intervention results with the option to change evidence state
n = 0
EU_results <- as.data.frame(matrix(ncol = length(after_nodes) + 1, nrow = length(flat_eq_dag_chains$state)))
colnames(EU_results) <- after_nodes 
colnames(EU_results)[[8]] <- "Nothing"
int_query_list <- as.data.frame(matrix(ncol = 21, nrow = length(flat_eq_dag_chains$state)))
obs_query_list <- as.data.frame(matrix(ncol = 21, nrow = length(flat_eq_dag_chains$state)))
colnames(int_query_list) <- names(unlist(int_query))
colnames(obs_query_list) <- names(unlist(obs_query))

for (g in flat_eq_dag_chains$state){
  n = n + 1
  print(n)
  gRain_obj <- togRain(x = g, data = data, smooth = 1)
  # Setup
  nodes <- gRain_obj$universe$nodes
  base_nodes <- nodes[grepl('.*_base', nodes)]
  after_nodes <- nodes[grepl('.*_after', nodes)]
  u <- c(1.0, 0.75, 0.0)
  
  evidence <- list(
    ACTIV_formula_base = "1",
    EMOT_formula_base = "1",
    NUTR_formula_base = "1", 
    PHYS_formula_base = "2", 
    SLEEP_formula_base = "1",
    SOCIAL_formula_base = "1",
    SUB_formula_base = "0"
  )
  
  gRain_evi <- setEvidence(gRain_obj, 
                           names(evidence), 
                             states = as.vector(unlist(evidence)))
  
  # Observational query prior to intervention.
  obs_query <- gRain::querygrain(gRain_evi, after_nodes, type = 'marginal')
  obs_eu <- obs_query |>
    lapply(function(x) sum(u*x)) |>
    unlist() |>
    sum()
  obs_query_list[n,] <- unlist(obs_query)
  EU_results[n, 8] <- obs_eu
  
  for (node in after_nodes) {
    # Query after intervention.
    intervention <- list()
    intervention[[node]] = c(1.0, 0.0, 0.0)
    
    mut_dag <- dagmc::MutilateGraph(gRain_obj, intervention)
    
    # Set evidence does not carry over to the mutilated graph. So it must be set again.
    mut_dag_evi <- setEvidence(mut_dag, 
                               names(evidence), 
                               states = as.vector(unlist(evidence)))
    
    # A query on the mutilated graph corresponds to an interventional query.
    int_query <- gRain::querygrain(mut_dag_evi, after_nodes, type = 'marginal')
    
    int_eu <- int_query |>
      lapply(function(x) sum(u*x)) |>
      unlist() |>
      sum()
    EU_results[n, node] <- int_eu
    int_query_list[n,] <- unlist(int_query)
  }
}

mean_row <- sapply(EU_results, mean)
sd_row <- sapply(EU_results, sd)

mean_df <- as.data.frame(t(mean_row))
sd_df <- as.data.frame(t(sd_row))

row.names(mean_df) <- "Mean"
row.names(sd_df) <- "sd"

evidence_string <- paste(unlist(evidence), collapse = "")

EU_results <- rbind(mean_df, sd_df)
write.csv(EU_results, file = paste0(evidence_string, "_results.csv"))

# Parallel loop using all evidences that have a count while ranking the domains. 

numCores <- detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)
results <- list()


results <- foreach(i = 1:1241, .combine = 'c', .packages = c('gRain', 'dagmc')) %dopar% {
  row_values <- all_combinations_df[i, -((ncol(all_combinations_df)-1))]
  string_values <- all_combinations_df[i, grepl("formula", colnames(all_combinations_df))]
  evidence <- as.list(string_values)
  evidence <- lapply(string_values, as.character)
  evidence_states <- data.frame(row_values, row.names = NULL)
  evidence_string <- paste(unlist(evidence[1:7]), collapse = "")
  rank <- list(
    ACTIV_formula_after = 2,
    EMOT_formula_after = 2,
    NUTR_formula_after = 1, 
    PHYS_formula_after = 1, 
    SLEEP_formula_after = 1,
    SOCIAL_formula_after = 1,
    SUB_formula_after = 1
  )
  unnorm_weight <- lapply(rank, function(x) sqrt(2)^x)
  normed_weight <- lapply(unnorm_weight, function(x) x / sum(unlist(unnorm_weight)))
  print(evidence_string)
  n = 0
  EU_results <- as.data.frame(matrix(ncol = length(after_nodes) + 1, nrow = length(flat_eq_dag_chains$state)))
  nothing_results <- as.data.frame(matrix(ncol = length(after_nodes) + 1, nrow = length(flat_eq_dag_chains$state)))
  colnames(EU_results) <- after_nodes 
  colnames(EU_results)[[8]] <- "Nothing"
  colnames(nothing_results) <- after_nodes
  colnames(nothing_results)[[8]] <- "Nothing"
  
  for (g in flat_eq_dag_chains$state) {
    n = n + 1
    gRain_obj <- togRain(x = g, data = data, smooth = 1)
    # Setup
    nodes <- gRain_obj$universe$nodes
    base_nodes <- nodes[grepl('.*_base', nodes)]
    after_nodes <- nodes[grepl('.*_after', nodes)]
    u <- c(1.0, 0.75, 0.0)
    
    gRain_evi <- setEvidence(gRain_obj, 
                             names(evidence), 
                             states = as.vector(unlist(evidence)))
    
    # Observational query prior to intervention.
    obs_query <- gRain::querygrain(gRain_evi, after_nodes, type = 'marginal')
    obs_eu <- obs_query |>
      lapply(function(x) sum(u*x)) 
    obs_eu <- Map(function(x,y) (x * y) * 7, obs_eu, normed_weight) |>
      unlist()
    obs_eu <- obs_eu[order(names(obs_eu))] 
    obs_eu <-  sum(obs_eu)
    EU_results[n, 8] <- obs_eu
    
    #Observational query prior to intervention without sum
    nothing_eu <- obs_query |>
      lapply(function(x) sum(u*x)) |>
      unlist()
    nothing_eu <- nothing_eu[order(names(nothing_eu))]
    nothing_eu[[8]] <- 0
    nothing_results[n, ] <- nothing_eu
    
    
    for (node in after_nodes) {
      # Query after intervention.
      intervention <- list()
      intervention[[node]] = c(1.0, 0.0, 0.0)
      
      mut_dag <- dagmc::MutilateGraph(gRain_obj, intervention)
      
      # Set evidence does not carry over to the mutilated graph. So it must be set again.
      mut_dag_evi <- setEvidence(mut_dag, 
                                 names(evidence), 
                                 states = as.vector(unlist(evidence)))
      
      # A query on the mutilated graph corresponds to an interventional query.
      int_query <- gRain::querygrain(mut_dag_evi, after_nodes, type = 'marginal')
      
      int_eu <- int_query |>
        lapply(function(x) sum(u*x))
      int_eu <- Map(function(x,y) (x * y) * 7, int_eu, normed_weight) |>
        unlist() 
      int_eu <- int_eu[order(names(int_eu))] 
      int_eu <- sum(int_eu)
      EU_results[n, node] <- int_eu
    }
    
  }
  
  mean_row <- sapply(EU_results, mean)
  sd_row <- sapply(EU_results, sd)
  nothing_mean <- sapply(nothing_results, mean)
  nothing_sd <- sapply(nothing_results, sd)
  results_table <- data.frame(Intervention = names(mean_row), Mean = mean_row, SD = sd_row, Nothing_Mean = nothing_mean, Nothing_SD = nothing_sd, row.names = NULL)
  evidence_states_repeated <- do.call(rbind, replicate(nrow(results_table), evidence_states, simplify = FALSE))
  results_table <- cbind(evidence_states_repeated, results_table)
  list(results_table)
}

stopCluster(cl)

# This collects the results of individual node interventions
results <- do.call(rbind, results)

n = 0
int_query_list <- as.data.frame(matrix(ncol = 7, nrow = length(flat_eq_dag_chains$state)))
obs_query_list <- as.data.frame(matrix(ncol = 7, nrow = length(flat_eq_dag_chains$state)))
colnames(int_query_list) <- names(after_nodes)
colnames(obs_query_list) <- names(after_nodes)

for (g in flat_eq_dag_chains$state){
  n = n + 1
  gRain_obj <- togRain(x = g, data = data, smooth = 1)
  # Setup
  nodes <- gRain_obj$universe$nodes
  base_nodes <- nodes[grepl('.*_base', nodes)]
  after_nodes <- nodes[grepl('.*_after', nodes)]
  u <- c(1.0, 0.75, 0.0)
  
  evidence <- list(
    ACTIV_formula_base = "2",
    EMOT_formula_base = "0",
    NUTR_formula_base = "0", 
    PHYS_formula_base = "0", 
    SLEEP_formula_base = "1",
    SOCIAL_formula_base = "2",
    SUB_formula_base = "0"
  )
  
  gRain_evi <- setEvidence(gRain_obj, 
                           names(evidence), 
                           states = as.vector(unlist(evidence)))
  
  # Observational query prior to intervention.
  obs_query <- gRain::querygrain(gRain_evi, after_nodes, type = 'marginal')
  obs_query[order(names(obs_query))]
  obs_eu <- obs_query |>
    lapply(function(x) sum(u*x)) |>
    unlist() 
  obs_eu <- obs_eu[order(names(obs_eu))]
  obs_query_list[n,] <- obs_eu
  
  
    # Query after intervention.
    intervention <- list()
    intervention[["SOCIAL_formula_after"]] = c(1.0, 0.0, 0.0)
    
    mut_dag <- cia::MutilateGraph(gRain_obj, intervention)
    
    # Set evidence does not carry over to the mutilated graph. So it must be set again.
    mut_dag_evi <- setEvidence(mut_dag, 
                               names(evidence), 
                               states = as.vector(unlist(evidence)))
    
    # A query on the mutilated graph corresponds to an interventional query.
    int_query <- gRain::querygrain(mut_dag_evi, after_nodes, type = 'marginal')
    int_query[order(names(int_query))]
    int_eu <- int_query |>
      lapply(function(x) sum(u*x)) |>
      unlist()
    int_eu <- int_eu[order(names(int_eu))]
    int_query_list[n,] <- int_eu
}

mean_obs_query <- sapply(obs_query_list, mean)
mean_int_query <- sapply(int_query_list, mean)


# Parallel loop using all evidences that have a count no ranking. 
numCores <- detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)
results <- list()


results <- foreach(i = 1:1241, .combine = 'c', .packages = c('gRain', 'dagmc')) %dopar% {
  row_values <- all_combinations_df[i, -((ncol(all_combinations_df)-1))]
  string_values <- all_combinations_df[i, grepl("formula", colnames(all_combinations_df))]
  evidence <- as.list(string_values)
  evidence <- lapply(string_values, as.character)
  evidence_states <- data.frame(row_values, row.names = NULL)
  evidence_string <- paste(unlist(evidence[1:7]), collapse = "")
  print(evidence_string)
  n = 0
  EU_results <- as.data.frame(matrix(ncol = length(after_nodes) + 1, nrow = length(flat_eq_dag_chains$state)))
  nothing_results <- as.data.frame(matrix(ncol = length(after_nodes) + 1, nrow = length(flat_eq_dag_chains$state)))
  colnames(EU_results) <- after_nodes 
  colnames(EU_results)[[8]] <- "Nothing"
  colnames(nothing_results) <- after_nodes
  colnames(nothing_results)[[8]] <- "Nothing"
  
  for (g in flat_eq_dag_chains$state) {
    n = n + 1
    gRain_obj <- togRain(x = g, data = data, smooth = 1)
    # Setup
    nodes <- gRain_obj$universe$nodes
    base_nodes <- nodes[grepl('.*_base', nodes)]
    after_nodes <- nodes[grepl('.*_after', nodes)]
    u <- c(1.0, 0.75, 0.0)
    
    gRain_evi <- setEvidence(gRain_obj, 
                             names(evidence), 
                             states = as.vector(unlist(evidence)))
    
    # Observational query prior to intervention.
    obs_query <- gRain::querygrain(gRain_evi, after_nodes, type = 'marginal')
    obs_query[order(names(obs_query))]
    obs_eu <- obs_query |>
      lapply(function(x) sum(u*x)) |>
      unlist() 
    obs_eu <- obs_eu[order(names(obs_eu))]
    obs_eu <-  sum(obs_eu)
    EU_results[n, 8] <- obs_eu
    
    #Observational query prior to intervention without sum
    nothing_eu <- obs_query |>
      lapply(function(x) sum(u*x)) |>
      unlist()
    nothing_eu <- nothing_eu[order(names(nothing_eu))]
    nothing_eu[[8]] <- 0
    nothing_results[n, ] <- nothing_eu
    
    
    for (node in after_nodes) {
      # Query after intervention.
      intervention <- list()
      intervention[[node]] = c(1.0, 0.0, 0.0)
      
      mut_dag <- dagmc::MutilateGraph(gRain_obj, intervention)
      
      # Set evidence does not carry over to the mutilated graph. So it must be set again.
      mut_dag_evi <- setEvidence(mut_dag, 
                                 names(evidence), 
                                 states = as.vector(unlist(evidence)))
      
      # A query on the mutilated graph corresponds to an interventional query.
      int_query <- gRain::querygrain(mut_dag_evi, after_nodes, type = 'marginal')
      int_eu <- int_query |>
        lapply(function(x) sum(u*x)) |>
        unlist()
      int_eu <- int_eu[order(names(int_eu))]
      int_eu <- sum(int_eu)
      EU_results[n, node] <- int_eu
    }
    
  }
  
  mean_row <- sapply(EU_results, mean)
  sd_row <- sapply(EU_results, sd)
  nothing_mean <- sapply(nothing_results, mean)
  nothing_sd <- sapply(nothing_results, sd)
  results_table <- data.frame(Intervention = names(mean_row), Mean = mean_row, SD = sd_row, Nothing_Mean = nothing_mean, Nothing_SD = nothing_sd, row.names = NULL)
  evidence_states_repeated <- do.call(rbind, replicate(nrow(results_table), evidence_states, simplify = FALSE))
  results_table <- cbind(evidence_states_repeated, results_table)
  list(results_table)
}

stopCluster(cl)

# Combine results
results <- do.call(rbind, results)
