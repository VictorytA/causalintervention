library(dplyr)
# This will filter out all the observations that followed up 7 to 180 days after first timepoint
# Fitness data is needed 
fitness_wide_tp <- fitness_wide_data %>%
  group_by(userId) %>%
  summarize(
    TimepointCount = n_distinct(timepoint),
    FirstTimepoint = min(assessmentSubmissionTime)
  ) %>%
  left_join(fitness_wide_data, by = "userId") %>%
  filter(TimepointCount >= 2) %>%
  mutate(DaysAfterFirst = as.numeric(difftime(assessmentSubmissionTime, FirstTimepoint, units = "days")))
unique_userId <- unique(fitness_wide_tp$userId)
replacements <- c("Healthy" = 0, "Fair" = 1, "Focus area" = 2)
filtered_data <- fitness_wide_tp %>%
  filter(PHYS_1 >= 0, PHYS_2 >= 0, PHYS_3 >= 0) %>%
  filter(timepoint == 2, DaysAfterFirst >= 7, DaysAfterFirst <= 180) %>%
  left_join(
    fitness_wide_tp %>%
      filter(timepoint == 1) %>%
      select(userId, contains("formula")),
    by = "userId",
    suffix = c("_after", "_base")
  ) %>%
  select(matches("formula")) %>%
  mutate_at(vars(contains("formula")), ~replacements[.])
data <- filtered_data[,sort(names(filtered_data))]



