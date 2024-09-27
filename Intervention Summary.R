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


