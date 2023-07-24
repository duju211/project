stages_itt <- function(df_stages_overview_combined_raw, time_trial_regex) {
  df_stages_overview_combined_raw |>
    filter(str_detect(desc, time_trial_regex)) |>
    mutate(
      url = str_glue("https://www.procyclingstats.com/{href}"),
      year = map_chr(str_split(year, "_"), ~ .x[length(.x)]),
      desc = str_remove(desc, time_trial_regex),
      desc = str_remove(desc, "Stage "),
      desc = str_remove(desc, "\\|"),
      desc = str_squish(desc),
      desc = str_glue("{desc} ({year})"))
}