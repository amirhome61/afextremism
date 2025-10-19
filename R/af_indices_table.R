#' Format and Print Political Extremism Gauge Indices Table
#'
#' @param indices_table A data frame containing the indices table created by af_gauge_indices
#' @param print_type What to print: NULL for all, "Population" for population only,
#'                  "Community" for community only, or a character vector of specific indices
#'
#' @return A gt table object
#'
#' @export
af_indices_table <- function(indices_table, print_type = NULL) {
  # Input validation
  if (!is.data.frame(indices_table)) {
    stop("indices_table must be a data frame")
  }
  
  # Check if print_type is valid
  valid_types <- c("Population", "Community")
  if (!is.null(print_type) && !is.character(print_type) && 
      !(length(print_type) == 1 && print_type %in% valid_types)) {
    # If print_type is not NULL and not one of the valid types, 
    # it should be a character vector of indices names
    if (!is.character(print_type)) {
      stop("print_type must be NULL, 'Population', 'Community', or a character vector of indices names")
    }
    # Check if all elements in print_type are valid column names in indices_table
    invalid_indices <- print_type[!print_type %in% names(indices_table)]
    if (length(invalid_indices) > 0) {
      stop(paste("The following indices are not present in the indices_table:", 
                 paste(invalid_indices, collapse = ", ")))
    }
  }
  
  # Make a copy of the table to avoid modifying the original
  table_to_print <- indices_table
  
  # Identify population and community indices columns
  pop_indices_cols <- grep("_p$", names(table_to_print), value = TRUE)
  comm_indices_cols <- grep("_c$", names(table_to_print), value = TRUE)
  
  # Identify count columns
  count_cols <- c("np", "nc")
  count_cols <- count_cols[count_cols %in% names(table_to_print)]
  
  # Identify population and community variable columns (non-indices, non-count columns)
  all_special_cols <- c(pop_indices_cols, comm_indices_cols, count_cols)
  all_cols <- names(table_to_print)
  var_cols <- setdiff(all_cols, all_special_cols)
  
  # Identify population and community variable columns
  # Population variables should not end with _p or _c
  pop_var_cols <- var_cols[!grepl("_c$", var_cols)]
  comm_var_cols <- setdiff(var_cols, pop_var_cols)
  
  # Process according to print_type
  if (identical(print_type, "Population")) {
    # Keep only population indices and variable columns
    cols_to_keep <- c(pop_var_cols, count_cols[count_cols == "np"], pop_indices_cols)
    cols_to_keep <- cols_to_keep[cols_to_keep %in% names(table_to_print)]
    table_to_print <- table_to_print[, cols_to_keep, drop = FALSE]
    
    # Remove community rows (rows with nc > 0)
    if ("nc" %in% names(indices_table)) {
      table_to_print <- table_to_print[indices_table$nc == 0, , drop = FALSE]
    }
  } else if (identical(print_type, "Community")) {
    # Keep all rows but remove population indices columns
    cols_to_keep <- c(pop_var_cols, comm_var_cols, count_cols, comm_indices_cols)
    cols_to_keep <- cols_to_keep[cols_to_keep %in% names(table_to_print)]
    table_to_print <- table_to_print[, cols_to_keep, drop = FALSE]
  } else if (is.character(print_type) && length(print_type) > 0) {
    # Identify which columns from print_type are population vs community indices
    print_pop_indices <- print_type[print_type %in% pop_indices_cols]
    print_comm_indices <- print_type[print_type %in% comm_indices_cols]
    
    # Keep only the specified indices and variable columns
    cols_to_keep <- c(pop_var_cols, comm_var_cols, count_cols, print_pop_indices, print_comm_indices)
    cols_to_keep <- cols_to_keep[cols_to_keep %in% names(table_to_print)]
    table_to_print <- table_to_print[, cols_to_keep, drop = FALSE]
    
    # Check if we have only population indices (no community indices)
    has_comm_indices <- length(print_comm_indices) > 0
    
    # If we don't have community indices, remove community rows
    if (!has_comm_indices && "nc" %in% names(table_to_print)) {
      table_to_print <- table_to_print[table_to_print$nc == 0, , drop = FALSE]
    }
  }
  
  # If the table is empty after filtering, return a message
  if (nrow(table_to_print) == 0) {
    stop("No data to display after applying the specified print_type filter")
  }
  
  # Reorder columns to match required order:
  # 1. Population variables
  # 2. Community variables
  # 3. np and nc counts
  # 4. Population indices
  # 5. Community indices
  ordered_cols <- c()
  
  # 1. Add population variables if present
  pop_vars_present <- pop_var_cols[pop_var_cols %in% names(table_to_print)]
  if (length(pop_vars_present) > 0) {
    ordered_cols <- c(ordered_cols, pop_vars_present)
  }
  
  # 2. Add community variables if present
  comm_vars_present <- comm_var_cols[comm_var_cols %in% names(table_to_print)]
  if (length(comm_vars_present) > 0) {
    ordered_cols <- c(ordered_cols, comm_vars_present)
  }
  
  # 3. Add count columns if present
  count_cols_present <- count_cols[count_cols %in% names(table_to_print)]
  if (length(count_cols_present) > 0) {
    ordered_cols <- c(ordered_cols, count_cols_present)
  }
  
  # 4. Add population indices if present
  pop_indices_present <- pop_indices_cols[pop_indices_cols %in% names(table_to_print)]
  if (length(pop_indices_present) > 0) {
    ordered_cols <- c(ordered_cols, pop_indices_present)
  }
  
  # 5. Add community indices if present
  comm_indices_present <- comm_indices_cols[comm_indices_cols %in% names(table_to_print)]
  if (length(comm_indices_present) > 0) {
    ordered_cols <- c(ordered_cols, comm_indices_present)
  }
  
  # Reorder the table columns
  table_to_print <- table_to_print[, ordered_cols, drop = FALSE]
  
  # Require gt package
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("The gt package is required for this function. Please install it with install.packages('gt')")
  }
  
  # Create gt table
  gt_table <- gt::gt(table_to_print)
  
  # Format indices according to requirements
  
  # Identify NP and EP indices (should have 1 decimal place)
  np_ep_cols <- grep("(np|ep)_[pc]$", names(table_to_print), value = TRUE)
  if (length(np_ep_cols) > 0) {
    gt_table <- gt::fmt_number(gt_table, columns = np_ep_cols, decimals = 1)
  }
  
  # Identify EL and ER indices (should be percentages with 1 decimal place)
  el_er_cols <- grep("(el|er[1-3])_[pc]$", names(table_to_print), value = TRUE)
  if (length(el_er_cols) > 0) {
    gt_table <- gt::fmt_percent(gt_table, columns = el_er_cols, 
                                decimals = 1, scale_values = FALSE)
  }
  
  # Format np and nc as integers if they are present
  count_cols_present <- count_cols[count_cols %in% names(table_to_print)]
  if (length(count_cols_present) > 0) {
    gt_table <- gt::fmt_number(gt_table, columns = count_cols_present, decimals = 0)
  }
  
  # Add a title
  title_text <- "Political Extremism Gauge Indices"
  if (identical(print_type, "Population")) {
    title_text <- paste(title_text, "- Population Level")
  } else if (identical(print_type, "Community")) {
    title_text <- paste(title_text, "- Community Level")
  } else if (is.character(print_type) && length(print_type) > 0) {
    title_text <- paste(title_text, "- Selected Indices")
  }
  
  gt_table <- gt::tab_header(gt_table, title = title_text)
  
  # Return the formatted table
  return(gt_table)
}