stages_overview <- function(df_stages_overview_raw, cycling_stats_url) {
  df_stages_overview_raw |>
    filter(desc != "") |>
    rename(url_stages_overview = url) |>
    mutate(
      year = parse_integer(
        map_chr(str_split(url_stages_overview, "/"), \(x) x[length(x)])),
      url_stage = str_glue("{cycling_stats_url}{href}")) |>
    filter(year <= 2023)
}