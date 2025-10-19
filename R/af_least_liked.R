#' Create percentage table of least liked groups by wave
#'
#' @param data A dataframe containing survey data
#' @param political_orientation Optional character string to filter by political orientation
#' @param title Character string for table title
#' @return A gt table object
af_create_leastliked_table <- function(data, political_orientation = NULL, title = "") {
  
  if (!is.null(political_orientation)) {
    data <- data[data$pe_left_center_right == political_orientation, ]
  }
  
  percentage_table <- data %>%
    count(leastliked, Wave) %>%
    group_by(Wave) %>%
    mutate(percentage = round(n / sum(n) * 100, 1)) %>%
    select(leastliked, Wave, percentage) %>%
    pivot_wider(names_from = Wave, values_from = percentage, values_fill = 0) %>%
    gt() %>%
    tab_header(title = title) %>%
    fmt_number(columns = -1, decimals = 1, suffixing = TRUE) %>%
    cols_label(leastliked = "Least Liked Group")
  
  return(percentage_table)
}