tdf_editions <- function(start_year, stages_overview_path) {
  years <- start_year:year(today())
  
  tibble(
    year = start_year:year(today()),
    so_path = str_glue("{stages_overview_path}{year}"))
}