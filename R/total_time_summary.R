total_time_summary <- function(df_total_time) {
  df_total_time |>
    filter(diff_winner_perc != 0) |>
    group_by(desc) |>
    summarise(median_diff_winner_perc = median(diff_winner_perc)) |>
    arrange(desc(median_diff_winner_perc)) |>
    mutate(desc = fct_inorder(desc))
}