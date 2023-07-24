stages_overview_raw <- function(stages_overview_url, stages_urls_css) {
  stages_overview_html <- read_html(stages_overview_url)
  
  stages_overview_nodes <- stages_overview_html |>
    html_nodes(stages_urls_css)
  
  tibble(
    href = html_attr(stages_overview_nodes, "href"),
    desc = html_text(stages_overview_nodes)) |>
    filter(str_detect(href, "stage-\\d+$")) |>
    distinct(href, .keep_all = TRUE)
}