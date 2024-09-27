# This will create a 

data <- as.data.frame(lapply(data, as.numeric))
type <- 'bge' 
scorer <- CreateScorer(scorer = BNLearnScorer,
                       data = data, 
                       type = type,
                       max_parents = 4,
                       blacklist = blacklist,
                       whitelist = NULL,
                       cache = TRUE)

n_results <- 400000
n_runs <- 8
init_partitions <- list()
for (i in 1:n_runs) {
  init_dag <- UniformlySampleDAG(names(data))
  #init_dag <- true_adj
  
  
  # Remove edges that disobey white/black listing. I think this breaks the 
  # uniformity from the prior but this will do for now.
  if (!is.null(scorer$blacklist))
    init_dag[which(scorer$blacklist)] <- 0
  
  if (!is.null(scorer$whitelist))
    init_dag[which(scorer$whitelist)] <- 1
  
  init_partitions[[i]] <- DAGtoPartition(init_dag)
}
t = 1 
system.time(
chains <- SampleChains(n_results, init_partitions,
                       transition = PartitionMCMC(temperature = t,
                                                  prerejection = FALSE),
                       scorer = scorer,
                       n_parallel_chains = n_runs))

n_burnin <- 1000
eq_chains <- chains[(1 + n_burnin):n_results]
for(i in seq_along(eq_chains)) {
  eq_chains[[i]] <- list(
    state = eq_chains[[i]]$state[seq(1, length(eq_chains[[i]]$state), by = 10)],
    log_score = eq_chains[[i]]$log_score[seq(1, length(eq_chains[[i]]$log_score), by = 10)],
    proposal_info = eq_chains[[i]]$proposal_info[seq(1, length(eq_chains[[i]]$proposal_info), by = 10)],
    mcmc_info = eq_chains[[i]]$mcmc_info[seq(1, length(eq_chains[[i]]$mcmc_info), by = 10)]
  )
}

eq_dag_chains <- PartitiontoDAG(eq_chains, scorer)
flat_eq_dag_chains <- FlattenChains(eq_dag_chains)
dag_collection <- CollectUniqueObjects(flat_eq_dag_chains)
map_dags <- GetMAP(dag_collection)

#Converts DAG chains to CP_DAG
eq_cpdag_chains <- cia::DAGtoCPDAG(eq_dag_chains)
flat_eq_cpdag_chains <- cia::FlattenChains(eq_cpdag_chains)
cpdag_collection <- cia::CollectUniqueObjects(flat_eq_cpdag_chains)
cpdag_map <- GetMAP(cpdag_collection)

log_scores <- list() 
for(i in 1:n_runs) {  
  log_scores[[i]] <- eq_dag_chains[[i]]$log_score |>    
    coda::mcmc() } 
log_scores |>   coda::mcmc.list() |>   coda::gelman.diag()
for (i in 1:n_runs)   eq_chains[[i]]$log_score |>     coda::mcmc() |>     coda::effectiveSize() |>     print()

total_accept <- CalculateAcceptanceRates(eq_chains, group_by = 'chain')
