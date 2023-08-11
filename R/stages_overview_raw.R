stages_overview_raw <- function(cycling_stats_url, so_paths, stages_urls_css) {
  session_bow <- bow(cycling_stats_url)
  
  map_df(so_paths, \(x) scrape_overview(session_bow, x, stages_urls_css))
}