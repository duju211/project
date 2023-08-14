vis_total_time <- function(df_total_time, df_total_time_summary) {
  df_total_time |>
    mutate(desc = fct_rev(factor(desc, df_total_time_summary$desc))) |>
    ggplot(aes(y = desc, x = diff_winner_perc)) +
    geom_beeswarm(alpha = 0.2) +
    geom_boxplot() +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_continuous(labels = label_percent()) +
    labs(y = "Stage", x = "Difference to Winner Time [%]")
}