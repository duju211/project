total_time <- function(df_time_delta, df_winner_time, df_stages_itt) {
  df_time_delta |>
    inner_join(df_winner_time, by = "url_stage") |>
    transmute(
      url_stage, rider,
      time = winner_time + time_delta,
      diff_winner_perc = (time - winner_time) / winner_time) |>
    left_join(df_stages_itt, by = "url_stage")
}