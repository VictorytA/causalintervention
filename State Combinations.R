# This collects the baseline states of the patients
# Needs Fitness data 

library(dplyr)

baseline_fitness_wide_data <- fitness_wide_data %>%
  filter(timepoint == 1)

replacements <- c("Healthy" = 0, "Fair" = 1, "Focus area" = 2)
columns_to_edit <- c("ACTIV_formula", 
                     "EMOT_formula",
                     "NUTR_formula",
                     "PHYS_formula",
                     "SLEEP_formula",
                     "SOCIAL_formula",
                     "SUB_formula")

baseline_data <- baseline_fitness_wide_data %>%
  mutate_at(vars(all_of(columns_to_edit)), ~replacements[.]) %>%
  filter(PHYS_1 >= 0 & PHYS_2 >= 0 & PHYS_3 >= 0) %>%
  select(matches("formula")) %>%
  na.omit()

colnames(baseline_data) <- paste0(colnames(baseline_data), "_base")
baseline_data <- baseline_data[,sort(names(baseline_data))]

#This is collects all possible combinations that exist in the baseline data
for (i in c(1,2,3,4,5,6,7)) {
  combination_data <- baseline_data 
  all_combinations <- list()
  
  for (col_name in colnames(combination_data)) {
    col_combinations <- unique(combination_data[[col_name]])
    all_combinations[[col_name]] <- col_combinations
  }
  
  all_combinations_df <- as.data.frame(do.call(expand.grid, all_combinations))
  
  combination_counts <- table(do.call(paste, c(combination_data, sep = "-")))
  all_combinations_df$count <- combination_counts[do.call(paste, c(all_combinations_df, sep = "-"))]
  all_combinations_df[is.na(all_combinations_df)] <- 0
  all_combinations_df <- all_combinations_df[order(-all_combinations_df$count), ]
  all_combinations_df$probability <- all_combinations_df$count / sum(all_combinations_df$count)
}  

