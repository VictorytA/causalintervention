#This will grab all states from dag_collection 
#and calculate the causal effects using lm
#load data, dag_collection

#This is a loop that runs in parallel
library(profvis)
library(doParallel)
library(cia)
library(dagitty)
profvis({
  cores <- 16
  cl <- makeCluster(cores)  
  registerDoParallel(cl)
  causal_mat <- NULL
causal_mat_list <- foreach(c = seq_along(flat_eq_dag_chains$state), .packages = c("stats", "cia", "dagitty")) %dopar% {
   state <- flat_eq_dag_chains$state[[c]]
   causal_mat <- matrix(0, nrow = 14, ncol = 14)
   diag(causal_mat) <- 1
   causal_sum <- matrix(0, nrow = 7, ncol = 2)
   rownames(causal_mat) <- colnames(data)
   colnames(causal_mat) <- colnames(data)
   BNstate <- toBNLearn(state)
   dagittystate <- as.dagitty(BNstate)
   for (i in 1:14) {
     colname_i <- colnames(data)[i]
     # grab all possible ancestors
     ancestors <- ancestors(dagittystate, colname_i, proper = TRUE)
     for (a in ancestors) {
       #grab all direct parents of a 
       parents <- row.names(state[state[,a] == 1, ])
       if (length(parents) > 0) {
         formula_text <- paste(colname_i, "~ ", 
                               a, " + ",
                               paste(parents, collapse = " + "))
       } else {
         formula_text <- paste(paste(colname_i), "~", a)
       }
       causal <- lm(as.formula(formula_text), data = data)
       causal_effect <- causal$coefficients[[2]]
       error <- summary(causal)$coefficients[[2, "Std. Error"]]
       random_sample <- rnorm(1, mean = causal_effect, sd = error)
       causal_mat[a, colname_i] <- random_sample
     }
   }
   causal_mat
 }
stopCluster(cl)
})


save(causal_mat_list, file = "causal_mat_list.RD")

causal_mat_list <- list()
causal_sum_list <- list()
states <- dag_collection$state
probability <- exp(dag_collection$log_norm_state_score)
profvis({
for (c in 1:10000) {
  print(c)
  state <- sample(states, 1, prob = probability)
  causal_mat <- matrix(0, nrow = 14, ncol = 14)
  error_mat <- matrix(0, nrow = 14, ncol = 14)
  
  diag(causal_mat) <- 1
  rownames(causal_mat) <- colnames(data)
  colnames(causal_mat) <- colnames(data)
  BNstate <- toBNLearn(state[[1]])
  dagittystate <- as.dagitty(BNstate)
  for (i in 1:14) {
    colname_i <- colnames(data)[i]
    # grab all possible ancestors
    ancestors <- ancestors(dagittystate, colname_i, proper = TRUE)
    for (a in ancestors) {
      #grab all direct parents of a 
      parents <- row.names(state[[1]][state[[1]][,a] == 1, ])
      if (length(parents) > 0) {
        formula_text <- paste(colname_i, "~ ", 
                              a, " + ",
                              paste(parents, collapse = " + "))
      } else {
        formula_text <- paste(paste(colname_i), "~", a)
      }
      causal <- lm(as.formula(formula_text), data = data)
      causal_effect <- causal$coefficients[[2]]
      error <- summary(causal)$coefficients[[2, "Std. Error"]]
      random_sample <- rnorm(1, mean = causal_effect, sd = error)
      causal_mat[a, colname_i] <- random_sample
      c_mat <- causal_mat[grep("base$", rownames(causal_mat)), grep("after$", rownames(causal_mat)) , drop = FALSE]
      causal_sum_mat <- matrix(rowSums(c_mat), nrow = 7, ncol = 1) 
      rownames(causal_sum_mat) <- rownames(c_mat)
      colnames(causal_sum_mat) <- "Sum"
    }
  }
  causal_mat_list[[c]] <- causal_mat
  causal_sum_list[[c]] <- causal_sum_mat
}
})
tictoc::toc()

#This is the same code but does not run in parallel

causal_mat_list <- list()
causal_sum_list <- list()
profvis({
  for (c in seq_along(flat_eq_dag_chains$state)) {
    print(c)
    state <- flat_eq_dag_chains$state[[c]]
    causal_mat <- matrix(0, nrow = 14, ncol = 14)
    error_mat <- matrix(0, nrow = 14, ncol = 14)
    diag(causal_mat) <- 1
    rownames(causal_mat) <- colnames(data)
    colnames(causal_mat) <- colnames(data)
    BNstate <- toBNLearn(state)
    dagittystate <- as.dagitty(BNstate)
    for (i in 1:14) {
      colname_i <- colnames(data)[i]
      # grab all possible ancestors
      ancestors <- ancestors(dagittystate, colname_i, proper = TRUE)
      for (a in ancestors) {
        #grab all direct parents of a 
        parents <- row.names(state[state[,a] == 1, ])
        if (length(parents) > 0) {
          formula_text <- paste(colname_i, "~ ", 
                                a, " + ",
                                paste(parents, collapse = " + "))
        } else {
          formula_text <- paste(paste(colname_i), "~", a)
        }
        causal <- lm(as.formula(formula_text), data = data)
        causal_effect <- causal$coefficients[[2]]
        error <- summary(causal)$coefficients[[2, "Std. Error"]]
        random_sample <- rnorm(1, mean = causal_effect, sd = error)
        causal_mat[a, colname_i] <- random_sample
        c_mat <- causal_mat[grep("base$", rownames(causal_mat)), grep("after$", rownames(causal_mat)) , drop = FALSE]
        causal_sum_mat <- matrix(rowSums(c_mat), nrow = 7, ncol = 1) 
        rownames(causal_sum_mat) <- rownames(c_mat)
        colnames(causal_sum_mat) <- "Sum"
      }
    }
    causal_mat_list[[c]] <- causal_mat
    causal_sum_list[[c]] <- causal_sum_mat
  }
})
