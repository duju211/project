stage_raw <- function(stage_url, stage_tbl_css) {
  stage_html <- read_html(stage_url)
  
  html_nodes(stage_html, stage_tbl_css) |>
    html_table() |>
    first() |>
    clean_names() |>
    mutate(url = stage_url)
}