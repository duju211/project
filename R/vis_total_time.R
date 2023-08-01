vis_total_time <- function(df_total_time) {
  df_total_time_pro <- df_total_time |>
    filter(diff_winner_perc != 0, diff_winner_perc != max(diff_winner_perc))
  
  df_summary <- df_total_time_pro |>
    group_by(desc) |>
    summarise(median_diff_winner_perc = median(diff_winner_perc)) |>
    arrange(desc(median_diff_winner_perc)) |>
    mutate(desc = fct_inorder(desc))
  
  df_vis <- df_total_time_pro |>
    mutate(desc = fct_rev(factor(desc, df_summary$desc)))
  
  df_vis |>
    ggplot(aes(y = desc, x = diff_winner_perc)) +
    ggbeeswarm::geom_beeswarm(alpha = 0.2) +
    geom_boxplot() +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_continuous(labels = label_percent()) +
    labs(y = "Stage", x = "Difference to Winner Time [%]")
}