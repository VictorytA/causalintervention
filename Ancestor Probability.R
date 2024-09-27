library(profvis)
library(doParallel)
library(cia)
library(dagitty)

cores <- 16
cl <- makeCluster(cores)  
registerDoParallel(cl)
ancestor_list <- list()
ancestor_list <- foreach(i = 1:8, .combine = 'c', .packages = c('dagitty', 'cia')) %dopar% {
causal_mat <- NULL
ancestor_mat <- matrix(0, nrow = 14, ncol = 14)
rownames(ancestor_mat) <- colnames(data)
colnames(ancestor_mat) <- colnames(data)
states <- eq_dag_chains[[i]]$state
for (c in 1:length(states)) { 
  print(c)
  state <- states[[c]]
  BNstate <- toBNLearn(state)
  dagittystate <- as.dagitty(BNstate)
  for (i in 1:14) {
    colname_i <- colnames(data)[i]
    # grab all possible ancestors
    ancestors <- ancestors(dagittystate, colname_i, proper = TRUE)
    row_indices <- match(ancestors, rownames(ancestor_mat))
    ancestor_mat[row_indices, colname_i] <- ancestor_mat[row_indices, colname_i] + 1
  }
}

list(ancestor_mat)
}

stopCluster(cl)

ancestor_mean <- ancestor_list |> 
  lapply(function(x) x/length(eq_dag_chains[[1]]$state)) |>
  (\(x) Reduce('+', x))() |>
  (\(x) x/8)()

#collects before ancestors from after nodes only

base <- c(2,4,6,8,10,12,14)
after <- c(1,3,5,7,9,11,13)
new_matrix <- matrix(NA, nrow = length(base), ncol = length(after))
rownames(new_matrix) <- rownames(ancestor_mean)[after]
colnames(new_matrix) <- colnames(ancestor_mean)[after]

for(i in seq_along(after)) {
  for(j in seq_along(after)) {
    x <- base[i]
    y <- after[j]
    row_name <- rownames(ancestor_mean)[x]
    col_name <- colnames(ancestor_mean)[y]
    new_matrix[i,j] <- ancestor_mean[x, y]
    
  }
}

#collects after ancestors from after nodes only

after <- c(1,3,5,7,9,11,13)
new_matrix <- matrix(NA, nrow = length(base), ncol = length(after))
rownames(new_matrix) <- rownames(ancestor_mean)[after]
colnames(new_matrix) <- colnames(ancestor_mean)[after]

for(i in seq_along(after)) {
  for(j in seq_along(after)) {
    x <- after[i]
    y <- after[j]
    row_name <- rownames(ancestor_mean)[x]
    col_name <- colnames(ancestor_mean)[y]
    new_matrix[i,j] <- ancestor_mean[x, y]
    
  }
}

cores <- 16
cl <- makeCluster(cores)  
registerDoParallel(cl)
parent_list <- list()
parent_list <- foreach(i = 1:8, .combine = 'c', .packages = c('dagitty', 'dagmc')) %dopar% {
  parent_mat <- matrix(0, nrow = 14, ncol = 14)
  rownames(parent_mat) <- colnames(data)
  colnames(parent_mat) <- colnames(data)
  states <- eq_dag_chains[[i]]$state
  for (c in 1:length(states)) { 
    print(c)
    state <- states[[c]]
    BNstate <- toBNLearn(state)
    dagittystate <- as.dagitty(BNstate)
    for (j in 1:14) {
      colname <- colnames(data)[j]
      # grab all possible ancestors
      ancestors <- ancestors(dagittystate, colname, proper = TRUE)
      for (a in ancestors) {
        #grab all direct parents of colname 
        if (state[a, colname] == 1) {
          print(parents)
          parent_mat[a, colname] <- parent_mat[a, colname] + 1
        }
      }
    }
  }
  list(parent_mat)
}

parent_mean <- parent_list |> 
  lapply(function(x) x/length(eq_dag_chains[[1]]$state)) |>
  (\(x) Reduce('+', x))() |>
  (\(x) x/8)()


base <- c(2,4,6,8,10,12,14)
after <- c(1,3,5,7,9,11,13)
new_matrix1 <- matrix(NA, nrow = length(base), ncol = length(after))
rownames(new_matrix1) <- rownames(p_edge)[after]
colnames(new_matrix1) <- colnames(p_edge)[after]

for(i in seq_along(after)) {
  for(j in seq_along(after)) {
    x <- after[i]
    y <- after[j]
    row_name <- rownames(p_edge)[x]
    col_name <- colnames(p_edge)[y]
    new_matrix1[i,j] <- p_edge[x, y]
    
  }
}

stopCluster()
write.csv(ancestor_mat_divided, file = "ancestor probability.csv")
