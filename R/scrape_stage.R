scrape_stage <- function(cycling_stats_host, stage_path, stage_tbl_css) {
  stage_url <- nod(cycling_stats_host, path = stage_path)
  
  stage_html <- scrape(stage_url)
  
  html_nodes(stage_html, stage_tbl_css) |>
    html_table() |>
    first() |>
    clean_names() |>
    mutate(rnk = row_number(), url_stage = stage_url$url)
}