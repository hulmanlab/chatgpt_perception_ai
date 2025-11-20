# Load dependencies and coefficient tables ---------------------------------------------

source(here::here("R", "load_packages.R"))

coefs_table <- source("analysis_outputs/all_coefs_weighted_background.txt")[[1]]
coefs_table[, estimates_label := fifelse(estimates_label == "(ref)", "Reference", estimates_label)]


unweighted_table <- source("C:/repos/sdca/perception_ai/hjemsendt/2024_nov/coef_tables_unweighted.txt")[[1]]
unweighted_table[, estimates_label := fifelse(estimates_label == "(ref)", "Reference", estimates_label)]


invitees_weighted_table <- source(
  "C:/repos/sdca/perception_ai/hjemsendt/2024_nov/coef_tables_weighted_invitees.txt"
)[[1]]
invitees_weighted_table[, estimates_label := fifelse(estimates_label == "(ref)", "Reference", estimates_label)]



# Define function for forest plotting the estimates -----------------------


generate_plots <- function(input, text_size) {

  output_dt <- input
  # Reorder outcome levels to match alluvial plot (color) order
  output_dt$y.level <- factor(output_dt$y.level,
                              levels = c("Don't know", "Benefits", "Equal", "Risks"))

  outcome_levels <- levels(output_dt$y.level)
  plot_list <- list()
  final_plot_list <- list()

  for (strata in outcome_levels) {
    figure <-
      output_dt[grepl("^chatgpt", term) &
                  baseline_perception == strata] |> ggplot(
                    aes(
                      y = forcats::fct_rev(term),
                      x = estimate,
                      xmin = conf.low,
                      xmax = conf.high,
                      color = y.level
                    )
                  ) +
      geom_vline(xintercept = 1, color = "grey70") +
      geom_pointrange(
        position = position_dodge(width = 0.75),
        linewidth = 1.2,
        fatten = 1.8
      ) +
      MetBrewer::scale_color_met_d("Hiroshige", direction = "-1") +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = text_size),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0, 0.1, 0, 0.3), "cm")
      ) +
      scale_x_log10(
        breaks = c(0.1, 0.3, 1, 3, 10),
        # Major grid lines at 1
        minor_breaks = NULL,
        # Minor grid lines at 0.1 and 10
        labels = c("0.1", "0.3", "1", "3", "10")
      ) +
      labs(x = "", y = "") +
      coord_cartesian(xlim = c(0.1, 10)) +
      scale_y_discrete(labels = rev(c("ChatGPT use"))) +
      theme(
        legend.position = "right",
        legend.key.size = unit(0.45, "cm"),
        legend.key.spacing.y = unit(0.0, "cm"),
        legend.title = element_text(
          size = text_size - 1,
          face = "bold",
          margin = margin(b = 0.1, unit = "cm")
        ),
        legend.text = element_text(size = text_size - 2)
      ) +
      guides(color = guide_legend(reverse = TRUE, title = "Perception at follow-up"))


    plot_list[["figures"]][[strata]] <- figure

    plot_list[["text"]][[strata]] <-
      output_dt[grepl("^chatgpt", term) &
                  baseline_perception == strata] |> ggplot(
                    aes(
                      y = forcats::fct_rev(term),
                      x = estimate,
                      xmin = conf.low,
                      xmax = conf.high,
                      color = y.level
                    )
                  ) +
      geom_text(
        aes(label = estimates_label, x = 0),
        position = position_dodge(width = 0.75),
        size = text_size - 5.5
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
      ) +
      scale_color_manual(values = rep("black", length(
        unique(output_dt[term != "(Intercept)"]$baseline_perception)
      ))) +
      theme(legend.position = "none") +
      labs(x = "", y = "") +
      scale_x_continuous(breaks = 1, labels = " ") +
      coord_cartesian(xlim = c(-0.1, 0.1))


  }

  input_name <- deparse(substitute(input))

  # Use switch() to select the correct N-vector for each analysis
  n_values <- switch(
    input_name,
    "unweighted_table" = c("2,236", "2,384", "1,083", "196"),
    "coefs_table" = c("2,230", "2,325", "1,128", "216"),
    "invitees_weighted_table" = c("2,387", "2,196", "1,110", "206"),
    # Default case if no name matches
    rep(NA_character_, 4)
  )

  # Create the data.table with the correctly selected vector
  baseline_counts <- data.table(
    ai_perception_2022 = c("Don't know", "Benefits", "Equal", "Risks"),
    N = n_values
  )


  for (strata in outcome_levels) {
    left_plot_col <- plot_list[["figures"]][[strata]] + ggtitle(paste0("Baseline: ", strata)) + theme(plot.title = element_text(
      hjust = 0.5,
      size = text_size - 2,
      face = "bold"
    ))
    right_plot_col <- plot_list[["text"]][[strata]] + ggtitle(paste0("N = ", baseline_counts[ai_perception_2022 == strata]$N)) + theme(plot.title = element_text(hjust = 0.5, size = text_size - 2))
    combined_plots <- left_plot_col +  plot_spacer() + right_plot_col + plot_layout(widths = c(4, -1.4, 3.6))


    final_plot_list[[strata]] <- combined_plots
  }

  return(final_plot_list)
}



# Generate the plots -------------------------------------------------

text_size <- 8

plot_panels_overall <- generate_plots(coefs_table, text_size = text_size)
fig_3 <- wrap_plots(plot_panels_overall, ncol = 2, guides = "collect") +
  plot_annotation(caption = "Odds Ratios",
                  theme = theme(plot.caption = element_text(hjust = 0.36, size = text_size - 1)))

plot_panels_unweighted <- generate_plots(unweighted_table, text_size = text_size)
fig_s3 <- wrap_plots(plot_panels_unweighted, ncol = 2, guides = "collect") +
  plot_annotation(caption = "Odds Ratios",
                  theme = theme(plot.caption = element_text(hjust = 0.36, size = text_size - 1)))

plot_panels_invitees <- generate_plots(invitees_weighted_table, text_size = text_size)
fig_s4 <- wrap_plots(plot_panels_invitees, ncol = 2, guides = "collect") +
  plot_annotation(caption = "Odds Ratios",
                  theme = theme(plot.caption = element_text(hjust = 0.36, size = text_size - 1)))


# Write images ------------------------------------------------------------------

# For 2x2 plots:
ggsave(
  here::here("plots", "fig_3.png"),
  fig_3,
  dpi = 600,
  create.dir = TRUE,
  device = "png",
  width = 16,
  height = 12,
  units = "cm"
)

ggsave(
  here::here("plots", "fig_s3.png"),
  fig_s3,
  dpi = 600,
  create.dir = TRUE,
  device = "png",
  width = 16,
  height = 12,
  units = "cm"
)

ggsave(
  here::here("plots", "fig_s4.png"),
  fig_s4,
  dpi = 600,
  create.dir = TRUE,
  device = "png",
  width = 16,
  height = 12,
  units = "cm"
)
