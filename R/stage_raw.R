stage_raw <- function(stage_url, stage_tbl_css) {
  stage_url_bow <- bow(stage_url)
  
  stage_html <- scrape(stage_url_bow)
  
  html_nodes(stage_html, stage_tbl_css) |>
    html_table() |>
    first() |>
    clean_names() |>
    mutate(url = stage_url)
}