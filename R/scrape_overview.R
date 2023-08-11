scrape_overview <- function(session_bow, so_path, stages_urls_css) {
  scrape_url <- nod(session_bow, path = so_path)
  
  stages_overview_html <- scrape(scrape_url)
  
  stages_overview_nodes <- stages_overview_html |>
    html_elements(stages_urls_css)
  
  tibble(
    href = html_attr(stages_overview_nodes, "href"),
    desc = html_text(stages_overview_nodes)) |>
    mutate(url = scrape_url$url)
}