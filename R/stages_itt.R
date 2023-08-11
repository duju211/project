stages_itt <- function(df_stages_overview, time_trial_regex) {
  df_stages_overview |>
    filter(str_detect(desc, time_trial_regex)) |>
    distinct(href, .keep_all = TRUE) |>
    mutate(
      desc = str_remove_all(desc, str_glue("{time_trial_regex}|Stage|\\|")),
      desc = str_squish(desc),
      desc = str_glue("{desc} ({year})"))
}