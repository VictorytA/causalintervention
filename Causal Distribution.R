mean_lm_mat <- apply(simplify2array(causal_mat_list), c(1,2), mean)
mean_lm_mat <- round(mean_lm_mat, 4)
#This part to for creating distribution plot for before to after

plot_list <- list()
base <- c(2,4,6,8,10,12,14)
after <- c(1,3,5,7,9,11,13)

ci_table <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  CI_Max = numeric(),
  CI_Min = numeric(),
  stringsAsFactors = FALSE
)

for(x in base){
  for(y in after) {
    row_name <- rownames(causal_mat_list[[1]])[x]
    col_name <- colnames(causal_mat_list[[1]])[y]
    variable_name <- paste0(row_name, " vs ", col_name)
    print(variable_name)
    result_list <- lapply(causal_mat_list, function(mat) {
      cell_value <- mat[x,y]
      data.frame(probability = cell_value)
    }) 
    lm <- dplyr::bind_rows(result_list)
    lm_numeric <- as.numeric(lm$probability)
    ci_lm_numeric <- lm_numeric[lm_numeric >= quantile(lm_numeric, 0.025) & lm_numeric <= quantile(lm_numeric, 0.975)]
    mean_lm_value <- sprintf('%.02f', mean_lm_mat[x,y])
    
    if(x == 14){
      density_plot <- ggplot2::ggplot(data = data.frame(x = ci_lm_numeric), ggplot2::aes(x)) +
      ggplot2::geom_density(fill = "#0099CC", color = "#0099CC", linewidth = 0,  alpha = 0.7) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      ggplot2:: xlim(-0.1, 1) +
      ggplot2:: ylim(0, max(density(ci_lm_numeric)$y)) +
      ggplot2:: annotate("text", x = 0.4, y = max(density(ci_lm_numeric)$y)/2, label = paste(mean_lm_value), vjust = 1.5 ) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )}
    else{
      density_plot <- ggplot2::ggplot(data = data.frame(x = ci_lm_numeric), ggplot2::aes(x)) +
        ggplot2::geom_density(fill = "#0099CC", color = "#0099CC", linewidth = 0,  alpha = 0.7) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        ggplot2:: xlim(-0.1, 1) +
        ggplot2:: ylim(0, max(density(ci_lm_numeric)$y)) +
        ggplot2:: annotate("text", x = 0.4, y = max(density(ci_lm_numeric)$y)/2, label = paste(mean_lm_value), vjust = 1.5 ) +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "white"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        ) 
      }
    
    ggplot2::ggsave(path = "Density Plots", 
           paste(variable_name,".pdf"), width = 8, height = 5)
    
    plot_list[[length(plot_list) + 1]] <- density_plot
  }
}
combined_row <- c("ACTIV", "EMOT", "NUTR", "PHYS", "SLEEP", "SOCIAL", "SUB")

gap1 <- grid::rectGrob(gp = grid::gpar(col = "black", fill = "white"))

plot <- ggplot2::ggsave("Before Causal Distribution Plot.jpg",
       gridExtra::arrangeGrob(grobs = plot_list, gap1, ncol = 7), width = 10, height = 10)

#after to after plot

library(ggplot2)
library(dplyr)
library(gridExtra)

after_plot_list <- list()
base <- c(2, 4, 6, 8, 10, 12, 14)
after <- c(1, 3, 5, 7, 9, 11, 13)

ci_table <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  CI_Max = numeric(),
  CI_Min = numeric(),
  CI_Mean = numeric(),
  stringsAsFactors = FALSE
)

for (x in after) {
  for (y in after) {
    row_name <- rownames(causal_mat_list[[1]])[x]
    col_name <- colnames(causal_mat_list[[1]])[y]
    variable_name <- paste0(row_name, " vs ", col_name)
    print(variable_name)
    result_list <- lapply(causal_mat_list, function(mat) {
      cell_value <- mat[x,y]
      data.frame(probability = cell_value)
    }) 
    lm <- dplyr::bind_rows(result_list)
    lm_numeric <- as.numeric(lm$probability)
    ci_lm_numeric <- lm_numeric[lm_numeric >= quantile(lm_numeric, 0.025) & lm_numeric <= quantile(lm_numeric, 0.975)]
    mean_lm_value <- sprintf('%.02f', mean(ci_lm_numeric))
    ci_max <-sprintf('%.02f', max(ci_lm_numeric))
    ci_min <- sprintf('%.02f', min(ci_lm_numeric))
    print(mean_lm_value)
    ci_table <- rbind(ci_table, data.frame(
      Variable1 = row_name,
      Variable2 = col_name,
      CI_Max = ci_max,
      CI_Min = ci_min,
      CI_Mean = mean_lm_value))
    
    if (max(ci_lm_numeric) == min(ci_lm_numeric)) {
      adjust_value <- 0.01
    } else {
      adjust_value <- 0.5
    }
    if(x == 13){
      density_plot <- ggplot2::ggplot(data = data.frame(x = ci_lm_numeric), ggplot2::aes(x)) +
        ggplot2::geom_density(adjust = adjust_value, fill = "#0099CC", color = "#0099CC", linewidth = 0,  alpha = 0.7) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        ggplot2:: xlim(-0.1, 0.8) +
        ggplot2:: ylim(0, (max(density(ci_lm_numeric)$y) + 2)) +
        ggplot2:: annotate("text", x = 0.4, y = (max(density(ci_lm_numeric)$y) + 2)/2, label = paste(mean_lm_value), vjust = 1.5 ) +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "white"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )}
    else{
      density_plot <- ggplot2::ggplot(data = data.frame(x = ci_lm_numeric), ggplot2::aes(x)) +
        ggplot2::geom_density(adjust = adjust_value, fill = "#0099CC", color = "#0099CC", linewidth = 0,  alpha = 0.7) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        ggplot2:: xlim(-0.1, 0.8) +
        ggplot2:: ylim(0, (max(density(ci_lm_numeric)$y) + 2)) +
        ggplot2:: annotate("text", x = 0.4, y = (max(density(ci_lm_numeric)$y) + 2)/2, label = paste(mean_lm_value), vjust = 1.5 ) +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "white"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        ) 
    }
    
    ggplot2::ggsave(path = "Density Plots", 
                    paste(variable_name,".pdf"), width = 8, height = 5)
    
    after_plot_list[[length(after_plot_list) + 1]] <- density_plot
  }
}

combined_row <- c("ACTIV", "EMOT", "NUTR", "PHYS", "SLEEP", "SOCIAL", "SUB")

gap1 <- grid::rectGrob(gp = grid::gpar(col = "black", fill = "white"))


ggplot2::ggsave("After Causal Distribution Plot.jpg",
                gridExtra::arrangeGrob(grobs = after_plot_list, gap1, ncol = 7), width = 10, height = 10)




#sum causal plot

sum_plot_list <- list()
mean_sum_mat <- apply(simplify2array(causal_sum_list), c(1,2), mean)
sum_ci_table <- data.frame(
  Variable1 = character(),
  CI_Max = numeric(),
  CI_Min = numeric(),
  stringsAsFactors = FALSE
)
for(x in 1:7){
row_name <- rownames(causal_sum_list[[1]])[x]
result_list <- lapply(causal_sum_list, function(mat) {
  cell_value <- mat[x, 1]
  data.frame(sum = cell_value)
}) 
c_sum <- dplyr::bind_rows(result_list)
sum_numeric <- as.numeric(c_sum$sum)
mean_sum_value <- round(mean_sum_mat[x, 1], 2)
ci_sum_numeric <- sum_numeric[sum_numeric >= quantile(sum_numeric, 0.025) & sum_numeric <= quantile(sum_numeric, 0.975)]
ci_max <- round(max(ci_sum_numeric), 2)
ci_min <-round(min(ci_sum_numeric), 2)

sum_ci_table <- rbind(sum_ci_table, data.frame(
  Variable1 = row_name,
  CI_Max = ci_max,
  CI_Min = ci_min))

sum_density_plot <- ggplot2::ggplot(data = data.frame(x = sum_numeric), ggplot2::aes(x)) +
  ggplot2::geom_density(fill = "#0099CC", color = "#0099CC", linewidth = 0,  alpha = 0.7) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  ggplot2::xlab("Total Causal Score") +
  ggplot2::xlim(-0.1, 2.5) +
  ggplot2::ylim(0, max(density(sum_numeric)$y)) +
  ggplot2::annotate("text", x = 2.0, y = max(density(sum_numeric)$y)/2, label = paste(mean_sum_value), vjust = 1.5 ) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )


ggplot2::ggsave(path = "Sum Plots", 
                paste(row_name,".pdf"), width = 20, height = 5)

sum_plot_list[[length(sum_plot_list) + 1]] <- sum_density_plot
}

combined_row <- c("ACTIV", "EMOT", "NUTR", "PHYS", "SLEEP", "SOCIAL", "SUB")

gap1 <- grid::rectGrob(gp = grid::gpar(col = "black", fill = "white"))

plot <- ggplot2::ggsave("Causal Sum Distribution Plot.jpg",
       gridExtra::arrangeGrob(grobs = sum_plot_list, gap1, ncol = 7), width = 14, height = 2)

