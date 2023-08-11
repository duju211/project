winner_time <- function(df_stage) {
  df_stage |>
    group_by(url_stage) |>
    summarise(winner_time = time[rnk == 1]) |>
    mutate(winner_time = hms(winner_time))
}