source("libraries.R")

walk(dir_ls("R"), source)

df_tdf_editions <- tibble(
  year = 2000:year(today()),
  url_edition = str_glue(
    "https://www.procyclingstats.com/race/tour-de-france/{year}"))

mapped_stages_overview <- tar_map(
  df_tdf_editions, names = "year", unlist = FALSE,
  tar_target(
    df_stages_overview_raw,
    stages_overview_raw(url_edition, stages_urls_css)))

list(
  tar_target(
    ex_url,
    "https://www.procyclingstats.com/race/tour-de-france/2023/stage-16"),
  tar_target(
    ex_stages_overview_url,
    "https://www.procyclingstats.com/race/tour-de-france/2023"),
  tar_target(
    stage_tbl_css,
    "div.result-cont:nth-child(5) > div:nth-child(1) > table:nth-child(1)"),
  tar_target(stages_overview_tbl_css, ".basic"),
  tar_target(stages_urls_css, ".basic a:nth-child(1)"),
  tar_target(time_trial_regex, regex("\\(ITT\\)", ignore_case = TRUE)),
  
  mapped_stages_overview,
  tar_combine(
    df_stages_overview_combined_raw,
    mapped_stages_overview[["df_stages_overview_raw"]],
    command = dplyr::bind_rows(!!!.x, .id = "year")),
  tar_target(
    df_stages_itt,
    stages_itt(df_stages_overview_combined_raw, time_trial_regex)),
  tar_target(
    urls_stages, pull(df_stages_itt, url)),
  tar_target(
    df_stage_raw,
    stage_raw(urls_stages, stage_tbl_css),
    pattern = map(urls_stages)),
  tar_target(df_stage, stage(df_stage_raw), pattern = map(df_stage_raw)),
  tar_target(df_time_delta, time_delta(df_stage)),
  tar_target(df_winner_time, winner_time(df_stage)),
  tar_target(
    df_total_time, total_time(df_time_delta, df_winner_time, df_stages_itt)),
  tar_target(gg_total_time, vis_total_time(df_total_time))
)