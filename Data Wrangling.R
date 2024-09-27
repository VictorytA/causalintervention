library(dplyr)
# This will filter out all the observations that followed up 7 to 180 days after first timepoint
# Fitness data is needed 
fitness_wide_filtered <- fitness_wide_data %>%
  group_by(userId) %>%
  summarize(TimepointCount = n_distinct(timepoint))

fitness_wide_filtered <- fitness_wide_data %>%
  left_join(fitness_wide_filtered, by = "userId") %>%
  filter(TimepointCount >= 2)

first_timepoints <- fitness_wide_filtered %>%
  group_by(userId) %>%
  summarize(FirstTimepoint = min(assessmentSubmissionTime))

fitness_wide_tp <- fitness_wide_filtered %>%
  left_join(first_timepoints, by = 'userId') %>%
  mutate(DaysAfterFirst = as.numeric(difftime(assessmentSubmissionTime, FirstTimepoint, units = "days")))

unique_userId <- unique(fitness_wide_tp$userId)
replacements <- c("Healthy" = 0, "Fair" = 1, "Focus area" = 2)
columns_to_edit <- c("SLEEP_formula", 
                     "PHYS_formula", 
                     "SOCIAL_formula", 
                     "ACTIV_formula", 
                     "EMOT_formula", 
                     "SUB_formula", 
                     "NUTR_formula")
fitness_wide_tp <- fitness_wide_tp %>%
  mutate_at(vars(all_of(columns_to_edit)), ~replacements[.])
filtered_data <- fitness_wide_tp %>%
  filter(PHYS_1 >= 0 & PHYS_2 >= 0 & PHYS_3 >= 0)
filtered_data <- filtered_data %>%
  filter(timepoint == 2) %>%
  filter(DaysAfterFirst >= 7 & DaysAfterFirst <= 180)
first_timepoint_data <- fitness_wide_tp %>%
  filter(timepoint == 1) %>%
  select(userId, contains("formula"))
filtered_data <- left_join(filtered_data, first_timepoint_data, suffix = c("_after", "_base"), by = "userId")
filtered_data <- filtered_data %>%
  select(matches("formula"))
data <- filtered_data[,sort(names(filtered_data))]

