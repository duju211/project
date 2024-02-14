time_delta <- function(df_stage) {
  df_stage |>
    transmute(
      url_stage, rider,
      time_delta = case_when(
        rnk == 1 ~ seconds(0),
        str_detect(time, ",") ~ ms(str_remove_all(time, ",")),
        TRUE ~ ms(str_sub(time, start = (str_length(time) / 2) + 1)))) |>
    filter(!is.na(time_delta), time_delta >= 0)
}