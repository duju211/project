stage <- function(cycling_stats_url, stages_paths, stage_tbl_css) {
  session_bow <- bow(cycling_stats_url)
  
  map_df(stages_paths, \(x) scrape_stage(session_bow, x, stage_tbl_css))
}