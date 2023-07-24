stage <- function(df_stage_raw) {
  df_stage_raw |>
    mutate(rnk = row_number()) |>
    filter(!is.na(rnk))
}