source("libraries.R")

walk(dir_ls("R"), source)

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
  tar_target(cycling_stats_url, "https://www.procyclingstats.com/"),
  tar_target(stages_overview_path, "race/tour-de-france/"),
  tar_target(start_year, 2000),
  tar_target(
    rvest_url, "https://rvest.tidyverse.org/articles/selectorgadget.html"),
  
  tar_target(
    df_tdf_editions, tdf_editions(start_year, stages_overview_path)),
  tar_target(so_paths, pull(df_tdf_editions, so_path)),
  tar_target(
    df_stages_overview_raw,
    stages_overview_raw(cycling_stats_url, so_paths, stages_urls_css)),
  tar_target(
    df_stages_overview,
    stages_overview(df_stages_overview_raw, cycling_stats_url)),
  tar_target(
    df_stages_itt, stages_itt(df_stages_overview, time_trial_regex)),
  tar_target(stages_paths, pull(df_stages_itt, href)),
  tar_target(
    df_stage, stage(cycling_stats_url, stages_paths, stage_tbl_css)),
  tar_target(df_time_delta, time_delta(df_stage)),
  tar_target(df_winner_time, winner_time(df_stage)),
  tar_target(
    df_total_time, total_time(df_time_delta, df_winner_time, df_stages_itt)),
  tar_target(df_total_time_summary, total_time_summary(df_total_time)),
  tar_target(gg_total_time, vis_total_time(df_total_time_summary)),
  tar_target(
    png_total_time,
    ggsave("total_time.png", plot = gg_total_time),
    format = "file"),
  
  tar_render(tdf_post, "tdf_itt.Rmd"),
  tar_render(
    tdf_readme, "tdf_itt.Rmd", output_format = "md_document",
    output_file = "README.md")
)