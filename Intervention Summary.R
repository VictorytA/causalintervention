library(tidyverse)
library(ggplot2)

#This creates the combination and the find the domain with the highest mean for each combination
new_table <- results |>
mutate(combination = paste(ACTIV_formula_base, EMOT_formula_base, NUTR_formula_base, PHYS_formula_base, SLEEP_formula_base,  SOCIAL_formula_base, 
                            SUB_formula_base,  sep = ""))

counts_table <- new_table |>
  group_by(combination) |>
  filter(Mean == max(Mean)) |>
  select(combination, Mean, Intervention, probability)
 
state_probabilities <- new_table |>
  distinct (combination, probability)

new_table <- new_table |>
  left_join(new_table |> 
              filter(Intervention == "Nothing") |>
              select(combination, nothing_mean = Mean),
            by = "combination")

plot_label <- c(
  "ACTIV_formula_after" = "Functioning",
  "EMOT_formula_after" = "Distress",
  "NUTR_formula_after" = "Nutrition",
  "PHYS_formula_after" = "Physical\nActivity", 
  "SLEEP_formula_after" = "Sleep",
  "SOCIAL_formula_after" = "Social",
  "SUB_formula_after" = "Substance"
)

plot_table <- new_table %>%
  mutate(EU_X_minus_Nothing = Mean - nothing_mean) %>%
  filter(Intervention != "Nothing") %>%
  select(Intervention, EU_X_minus_Nothing, probability) %>%
  mutate(Intervention = recode(Intervention, !!!plot_label))

treatment_plot <- ggplot(plot_table, aes(x = Intervention, y = EU_X_minus_Nothing, size = probability)) +
  geom_point(alpha = 0.1) +
  theme_minimal() +
  labs(x = "Intervention Target",
       y = "Average Treatment Effect (ATE)",
       size = "Probability") +
  scale_y_continuous(limits = c(0, 1.5), breaks = c(0, 0.5, 1, 1.5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 18, margin = margin(t = 20)),
        axis.title.y = element_text(size = 18, margin = margin(r = 20)),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
ggsave("Treatment Effect Plot Prejection.pdf", treatment_plot, width = 13,
       height = 7, device = pdf)

#This creates a ranking plot 
ranking_table <- new_table %>% 
  group_by(combination) %>%
  mutate(Rank = rank(desc(Mean), ties.method = "first")) %>%
  select(combination, Intervention, Rank)

combinations <- unique(ranking_table$combination)

weighted_matrices <- list()

load("domain_names.rd")

for (combo in combinations) {
  subset_table <- ranking_table %>% filter(combination == combo)
  probability <- new_table %>% filter(combination == combo) %>%
    select(probability) %>%
    distinct() %>%
    pull(probability)
  binary_matrix <- matrix(0, nrow = nrow(subset_table), ncol = nrow(subset_table))
  rownames(binary_matrix) <- subset_table$Intervention
  colnames(binary_matrix) <- c("1", "2", "3", "4", "5", "6", "7", "8")
  
  for (i in 1:nrow(subset_table)) {
    current_rank <- subset_table$Rank[i]
    indices <- current_rank:8
    binary_matrix[i, indices] <- 1
  }
  binary_matrix <- as.data.frame(binary_matrix)
  weighted_matrix <- binary_matrix * probability
  weighted_matrices[[combo]] <- weighted_matrix
}

ranking_plot_table <- Reduce("+", weighted_matrices)
ranking_plot_table <- ranking_plot_table %>%
  filter(row_number() != 8) %>%
  select(-"8")
ranking_plot_table <- ranking_plot_table * 100
rownames(ranking_plot_table) <- domain_names

long_table <- long_format <- ranking_plot_table %>%
  rownames_to_column(var = "formula") %>%
  pivot_longer(
    cols = colnames(ranking_plot_table),
    names_to = "ranking",
    values_to = "value"
  ) %>%
  mutate(ranking = as.integer(ranking))

ranking_plot <- ggplot(long_table, aes(x = ranking, y = value, color = formula, group = formula)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(size = 5, x = "Ranking of Intervention Target", y = 'Probability of Ranking or Better (%)') +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 18, margin = margin(t = 20)),
        axis.title.y = element_text(size = 18, margin = margin(r = 20)),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_blank())

ggsave("Ranking Plot Prejection.pdf",  ranking_plot,  width = 14,
       height = 7, device = pdf)

pdf("Treatment and Ranking Plot.pdf",
    width = 14,
    height = 14)
  (treatment_plot/ranking_plot) + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 20, face = "bold"))
dev.off()


# Redundant may take it out
# Function to find the unique highest value column for each row

formula_columns <- c('ACTIV_formula_base', 'EMOT_formula_base', 'NUTR_formula_base', 
                     'PHYS_formula_base', 'SLEEP_formula_base',  'SOCIAL_formula_base', 
                     'SUB_formula_base')

find_unique_highest <- function(row) {
  # Extract the values from the formula columns
  values <- row[formula_columns]
  
  # Find the highest value
  max_value <- max(values)
  
  # Check if the highest value is unique
  if(sum(values == max_value) == 1) {
    # Return the column name with the unique highest value
    return(names(values)[which.max(values)])
  } else {
    # Return NA if the highest value is not unique
    return(NA)
  }
}

new_table$unique_highest_domain <- apply(new_table, 1, find_unique_highest)

unique_highest_table <- new_table %>% filter(!is.na(unique_highest_domain))

new_table <- unique_highest_table %>%
  mutate(combination = paste(SLEEP_formula_base, PHYS_formula_base, SOCIAL_formula_base, 
                             ACTIV_formula_base, EMOT_formula_base, SUB_formula_base, NUTR_formula_base, sep = ""))

counts_table <- new_table %>% 
  group_by(combination) %>%
  filter(Mean == max(Mean)) %>%
  select(combination, Mean, Intervention, probability, unique_highest_domain)

counts_table <- counts_table %>%
  mutate(domain = case_when(
    str_detect(Intervention, "ACTIV") ~ "ACTIV_formula_base",
    str_detect(Intervention, "EMOT") ~ "EMOT_formula_base",
    str_detect(Intervention, "NUTR") ~ "NUTR_formula_base",
    str_detect(Intervention, "PHYS") ~ "PHYS_formula_base",
    str_detect(Intervention, "SLEEP") ~ "SLEEP_formula_base",
    str_detect(Intervention, "SOCIAL") ~ "SOCIAL_formula_base",
    str_detect(Intervention, "SUB") ~ "SUB_formula_base",
    TRUE ~ NA_character_
  ))

non_matching_data_table <- counts_table %>%
  filter(unique_highest_domain != domain)

summary_table <- counts_table %>%
  group_by(unique_highest_domain) %>%
  summarise(
    total_rows = n(),
    match_count = sum(unique_highest_domain == domain),
    percentage_match = (match_count / total_rows) * 100
  )

ranking_table <- new_table %>% 
  group_by(combination) %>%
  mutate(Rank = rank(desc(Mean), ties.method = "first")) %>%
  select(combination, Intervention, Rank)

final_label <- c(
  "ACTIV" = "Functioning",
  "EMOT" = "Distress",
  "NUTR" = "Nutrition",
  "PHYS" = "Physical\nActivity",
  "SLEEP" = "Sleep",
  "SOCIAL" = "Social",
  "SUB" = "Substance"
)

count_ranking_table <- ranking_table %>%
  group_by(Intervention, Rank) %>%
  summarise(count = n(), percentage = n()/1241, .groups = 'drop') %>%
  filter(Intervention != "Nothing") %>%
  mutate(Intervention = sub("_formula_after", "", Intervention)) %>%
  mutate(Intervention = recode(Intervention, !!!final_label))

blues <- RColorBrewer::brewer.pal(9, "Blues")

ggplot(count_ranking_table, aes(x = Intervention, y = Rank, fill = log(percentage))) +
  geom_tile() + 
  scale_fill_gradientn(colors = colorRampPalette(c("#ffffff", "dodgerblue3"))(6),
                       values = scales::rescale(c(0, 0.1, 0.2, 0.3, 0.4, 
                                                  0.5))) + 
  scale_y_continuous(breaks = 1:8) +
  theme(panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),) + 
  labs(title = "Heatmap of Intervention Counts by Rank",
       x = "Intervention",
       y = "Rank") +
  geom_text(aes(label = paste0(count, "\n(", sprintf('%.02f', percentage), ")")), color = "black", size = 3) +
  guides(fill = "none")



domains <- c("SLEEP_formula_base", "PHYS_formula_base", "SOCIAL_formula_base", "ACTIV_formula_base", "EMOT_formula_base", "SUB_formula_base", "NUTR_formula_base")
filtered_results <- results[apply(results[, domains], 1, function(row) row["EMOT_formula_base"] == max(row)), ]

filtered_results <- filtered_results %>%
  mutate(combination = paste(SLEEP_formula_base, PHYS_formula_base, SOCIAL_formula_base, 
                             ACTIV_formula_base, EMOT_formula_base, SUB_formula_base, NUTR_formula_base, sep = ""))
filtered_results <- filtered_results %>% 
  group_by(combination) %>%
  filter(Mean == max(Mean)) %>%
  select(combination, Mean, Intervention, probability)

sum((filtered_results$Intervention == "EMOT_formula_after")/nrow(filtered_results))
non_emot_interventions <- filtered_results[filtered_results$Intervention != "EMOT_formula_after", ]


