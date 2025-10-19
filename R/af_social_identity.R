#' Generates a horizontal bar chart showing counts for combinations of specified variables.
#' Allows filtering based on a percentage threshold and exclusion of specific strings
#' in combination labels. Displays percentage on each bar and uses "Respondents" for the x-axis label.
#' The subtitle includes filtering information and the count/percentage of respondents remaining after filtering.
#' A multi-line caption is added to show the number of combinations included and the included values for each variable.
#'
#' @param df The input dataframe.
#' @param var_names A character vector containing the names of the variables to combine.
#'                  These variables should ideally be factors or convertible to factors.
#' @param threshold A numeric value (0 to 100). If not NULL, combinations with a
#'                  percentage of total rows below this threshold will not be displayed.
#'                  Defaults to NULL (no threshold filtering).
#' @param exclude_strings A character vector of strings. If not NULL, combinations
#'                        whose label contains any of these strings will not be included.
#'                        Defaults to NULL (no string exclusion filtering).
#' @return A ggplot object representing the horizontal bar chart.
#' @examples
#' # Assuming you have a dataframe 'my_data' with columns 'A', 'B', 'C'
#' # my_data <- data.frame(A = sample(letters[1:2], 100, replace = TRUE),
#' #                       B = sample(1:3, 100, replace = TRUE),
#' #                       C = sample(c("X", "Y"), 100, replace = TRUE))
#' #
#' # # Example 1: Basic usage
#' # chart1 <- af_plot_combinations_chart(my_data, c("A", "B", "C"))
#' # print(chart1)
#'
#' # # Example 2: Using a threshold (e.g., exclude combinations less than 5%)
#' # chart2 <- af_plot_combinations_chart(my_data, c("A", "B", "C"), threshold = 5)
#' # print(chart2)
#'
#' # # Example 3: Excluding combinations containing "Low" or "Group A"
#' # chart3 <- af_plot_combinations_chart(my_data, c("pe_left_center_right", "religiosity", "leastliked"), exclude_strings = c("Low", "Group A"))
#' # print(chart3)
#'
#' # # Example 4: Using both threshold and exclusion
#' # chart4 <- af_plot_combinations_chart(my_data, c("pe_left_center_right", "religiosity", "leastliked"), threshold = 10, exclude_strings = c("Right_High"))
#' # print(chart4)
#'
#' # # Example 5: Demonstrating error handling (uncomment to test)
#' # try(af_plot_combinations_chart(dummy_df, c("pe_left_center_right", "non_existent_var")))
#' # try(af_plot_combinations_chart(dummy_df, c("pe_left_center_right"), threshold = 101))
#' # try(af_plot_combinations_chart(dummy_df, c("pe_left_center_right"), exclude_strings = 123))
af_plot_combinations_chart <- function(df, var_names, threshold = NULL, exclude_strings = NULL) {
  
  # --- Input Validation ---
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a dataframe.")
  }
  if (!is.character(var_names) || length(var_names) == 0) {
    stop("Input 'var_names' must be a character vector of at least one variable name.")
  }
  if (!all(var_names %in% names(df))) {
    missing_vars <- var_names[!var_names %in% names(df)]
    stop(paste("The following variables are not found in the dataframe:", paste(missing_vars, collapse = ", ")))
  }
  if (!is.null(threshold) && (!is.numeric(threshold) || threshold < 0 || threshold > 100)) {
    stop("Input 'threshold' must be a numeric value between 0 and 100.")
  }
  if (!is.null(exclude_strings) && !is.character(exclude_strings)) {
    stop("Input 'exclude_strings' must be a character vector.")
  }
  
  
  # --- Data Preparation ---
  # Get total number of rows in the original dataframe for percentage calculation
  total_rows <- nrow(df)
  
  # Select only the specified columns and ensure they are factors
  df_subset <- df %>%
    select(all_of(var_names)) %>%
    mutate(across(everything(), factor)) # Convert all selected columns to factors
  
  # Calculate the counts for each unique combination of the specified factors
  combination_counts <- df_subset %>%
    count(!!!syms(var_names), name = "n") # Use !!!syms() to count by multiple variables
  
  # Create a single interaction term for the y-axis label (since it will be horizontal)
  # Encapsulate each part in square brackets
  combination_counts$combination_label <- apply(combination_counts[, var_names], 1, function(x) paste0("[", x, "]", collapse = "_"))
  
  # Calculate percentage of total rows for each combination
  combination_counts <- combination_counts %>%
    mutate(percentage = (n / total_rows) * 100) %>%
    mutate(percentage_label = paste0(round(percentage, 1), "%")) # Create a formatted percentage label
  
  # Store initial counts before filtering for subtitle
  initial_combination_counts <- combination_counts
  
  # --- Filtering ---
  # Apply threshold filtering if threshold is specified
  if (!is.null(threshold)) {
    combination_counts <- combination_counts %>%
      filter(percentage >= threshold)
  }
  
  # Apply string exclusion filtering if exclude_strings is specified
  if (!is.null(exclude_strings)) {
    # Create a regex pattern to match any of the exclude_strings
    exclude_pattern <- paste(exclude_strings, collapse = "|")
    combination_counts <- combination_counts %>%
      filter(!grepl(exclude_pattern, combination_label))
  }
  
  # --- Calculate Post-Filtering Stats ---
  # Calculate total respondents and percentage after filtering
  total_respondents_after_filtering <- sum(combination_counts$n)
  percentage_after_filtering <- (total_respondents_after_filtering / total_rows) * 100
  
  # Calculate the number of combinations included
  num_combinations_included <- nrow(combination_counts)
  
  # Identify the unique values included for each variable after filtering
  included_values_list <- lapply(var_names, function(var) {
    unique_vals <- unique(combination_counts[[var]])
    paste0(var, ": [", paste(unique_vals, collapse = ", "), "]")
  })
  # Combine included values with newline characters for multi-line caption
  included_values_text <- paste(included_values_list, collapse = "\n")
  
  
  # --- Plotting ---
  # Create the ggplot object
  # Map combination_label to the y-axis and n to the x-axis for a horizontal bar chart
  p <- ggplot(combination_counts, aes(y = combination_label, x = n)) +
    geom_bar(stat = "identity", fill = "skyblue") + # Use stat="identity" and a default fill color
    geom_text(aes(label = percentage_label), # Add percentage labels to the bars
              hjust = -0.1, # Adjust horizontal position of the label (outside the bar)
              size = 3) + # Adjust label size
    labs(
      title = paste("Counts per Combination of:", paste(var_names, collapse = ", ")),
      y = paste("Combination (", paste(var_names, collapse = "_"), ")", sep = ""), # Y-axis label for horizontal chart
      x = "Respondents" # X-axis label changed to "Respondents"
    ) +
    theme(
      # No need to rotate y-axis labels usually, but adjust if needed
      # axis.text.y = element_text(angle = 0, hjust = 1),
      plot.title = element_text(hjust = 0.5), # Center the plot title
      plot.caption = element_text(hjust = 0) # Left-align the caption
    ) +
    # Ensure the x-axis includes enough space for the labels
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)))
  
  
  # Add subtitle indicating if filtering was applied and showing post-filtering stats
  subtitle_text <- NULL
  filter_details <- c()
  
  if (!is.null(threshold)) {
    filter_details <- c(filter_details, paste("Combinations below", threshold, "% excluded"))
  }
  if (!is.null(exclude_strings)) {
    filter_details <- c(filter_details, paste("those containing:", paste(exclude_strings, collapse = ", "), "excluded"))
  }
  
  if (length(filter_details) > 0) {
    subtitle_text <- paste("Filtered:", paste(filter_details, collapse = " and "),
                           "| Displaying", total_respondents_after_filtering,
                           "Respondents (", round(percentage_after_filtering, 1), "% of total)")
  }
  
  
  if (!is.null(subtitle_text)) {
    p <- p + labs(subtitle = subtitle_text)
  }
  
  # Add multi-line caption with number of combinations and included values
  caption_text <- paste0("Included Combinations: ", num_combinations_included, "\n",
                         "Included Values:\n", included_values_text)
  
  p <- p + labs(caption = caption_text)
  
  
  # Return the plot object
  return(p)
}

