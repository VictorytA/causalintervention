#This will grab all states from dag_collection 
#and calculate the causal effects using lm
#load data, dag_collection

#This is a loop that runs in parallel
library(doParallel)
library(cia)
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
