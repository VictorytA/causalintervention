#This finds the edge probabilities 
p_edge <- CalculateEdgeProbabilities(flat_eq_dag_chains)

custom_labels <- c(
  "ACTIV_formula_after" = "Functioning",
  "ACTIV_formula_base" = "Functioning",
  "EMOT_formula_after" = "Distress",
  "EMOT_formula_base" = "Distress",
  "NUTR_formula_after" = "Nutrition",
  "NUTR_formula_base" = "Nutrition",
  "PHYS_formula_after" = "Physical\nActivity",
  "PHYS_formula_base" = "Physical\nActivity",
  "SLEEP_formula_after" = "Sleep",
  "SLEEP_formula_base" = "Sleep",
  "SOCIAL_formula_after" = "Social",
  "SOCIAL_formula_base" = "Social",
  "SUB_formula_after" = "Substance",
  "SUB_formula_base" = "Substance"
)

groups <- list("baseline" = which(endsWith(colnames(p_edge), 'base')),   
              "follow-up" = which(endsWith(colnames(p_edge), 'after')))

colour <- RColorBrewer::brewer.pal(n = 3, name = "Set1")
edge_colours_matrix <- matrix("black", ncol = ncol(p_edge), nrow = nrow(p_edge),
                             dimnames = list(rownames(p_edge), colnames(p_edge)))
for (i in 1:14) {
  if (grepl("_base", colnames(edge_colours_matrix)[i])) {
    for (j in 1:14) {
      if (grepl("_after", colnames(edge_colours_matrix)[j])) {
        edge_colours_matrix[i, j] <- "orange"
      }
    }
  }
}


pdf("DAG.pdf",
    width = 9,
    height = 7)
qgraph::qgraph(p_edge,
       layout = "groups",
       groups = groups,
       color = colour,
       minimum = 0.1, 
       threshold = 0.1,
       edge.color = edge_colours_matrix,
       edge.width = 0.5,
       label.cex =  0.7,
       legend.cex = 0.4,
       label.scale = FALSE,
       layoutScale = c(1.2, 1),
       GLratio = 3.0,
       mar = c(5, 5, 5, 5),
       legend = TRUE,
       labels = custom_labels
)
dev.off()

#CPDAG Map

groups <- list(baseline = which(endsWith(colnames(cpdag_map$state[[1]]), 'base')),   
            "follow-up" = which(endsWith(colnames(cpdag_map$state[[1]]), 'after')))

colour <- RColorBrewer::brewer.pal(n = 3, name = "Set1")
edge_colours_matrix <- matrix("black", ncol = ncol(cpdag_map$state[[1]]), nrow = nrow(cpdag_map$state[[1]]),
                              dimnames = list(rownames(cpdag_map$state[[1]]), colnames(cpdag_map$state[[1]])))
for (i in 1:14) {
  if (grepl("_base", colnames(edge_colours_matrix)[i])) {
    for (j in 1:14) {
      if (grepl("_after", colnames(edge_colours_matrix)[j])) {
        edge_colours_matrix[i, j] <- "orange"
      }
    }
  }
}

for (i in 1:14) {
  if (grepl("_after", colnames(edge_colours_matrix)[i])) {
    for (j in 1:14) {
      if (grepl("_base", colnames(edge_colours_matrix)[j])) {
        edge_colours_matrix[i, j] <- "orange"
      }
    }
  }
}


pdf("CPDAG_MAP.pdf",
    width = 9,
    height = 7)
qgraph::qgraph(cpdag_map$state[[1]],
               layout = "groups",
               groups = groups,
               color = colour,
               minimum = 0,
               edge.color = edge_colours_matrix,
               edge.width = 0.5,
               label.cex =  0.7,
               legend.cex = 0.4,
               label.scale = FALSE,
               layoutScale = c(1.2, 1),
               GLratio = 3.0,
               mar = c(5, 5, 5, 5),
               legend = TRUE,
               labels = custom_labels)
dev.off()
