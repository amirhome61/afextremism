#' Plot Political Extremism Gauge Indices
#'
#' @param indices_table A data frame containing the indices table from af_gauge_indices
#' @param selected_indices Character vector of indices to plot, if NULL all indices will be selected
#' @param pop_var1 Optional first population variable name
#' @param pop_levels1 Optional list of levels for pop_var1
#' @param pop_var2 Optional second population variable name
#' @param pop_levels2 Optional list of levels for pop_var2
#' @param pop_var3 Optional third population variable name
#' @param pop_levels3 Optional list of levels for pop_var3
#' @param comm_var1 Optional first community variable name
#' @param comm_levels1 Optional list of levels for comm_var1
#' @param comm_var2 Optional second community variable name
#' @param comm_levels2 Optional list of levels for comm_var2
#' @param type Type of plot to generate: "by_index" (default) or "by_group"
#' @param bar_type Orientation of the bars: "Vertical" (default) or "Horizontal"
#' @param ncol Number of plots per row in the combined plot
#'
#' @return A ggplot object with the combined plots
#'
#' @export
af_plot_indices <- function(indices_table, 
                            selected_indices = NULL,
                            pop_var1 = NULL, pop_levels1 = NULL,
                            pop_var2 = NULL, pop_levels2 = NULL,
                            pop_var3 = NULL, pop_levels3 = NULL,
                            comm_var1 = NULL, comm_levels1 = NULL,
                            comm_var2 = NULL, comm_levels2 = NULL,
                            type = "by_index",
                            bar_type = "Vertical",
                            ncol = 2) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required. Please install it.")
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required. Please install it.")
  }
  if (!requireNamespace("viridis", quietly = TRUE)) {
    stop("Package 'viridis' is required. Please install it.")
  }
  
  # Import required packages
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
  library(viridis)
  
  # Input validation
  if (!is.data.frame(indices_table)) {
    stop("indices_table must be a data frame")
  }
  
  # Identify all indices columns in the table (those ending with _p or _c)
  all_indices_in_table <- grep("_(p|c)$", names(indices_table), value = TRUE)
  
  # Remove np and nc columns if they exist in the indices list
  all_indices_in_table <- all_indices_in_table[!all_indices_in_table %in% c("np", "nc")]
  
  if (length(all_indices_in_table) == 0) {
    stop("No indices columns found in the indices_table")
  }
  
  # Check if community variables are specified
  comm_vars_not_null <- c(comm_var1, comm_var2)[!sapply(c(comm_var1, comm_var2), is.null)]
  has_comm_vars <- length(comm_vars_not_null) > 0
  
  # Determine if we should use population or community indices
  should_use_comm_indices <- has_comm_vars
  
  # If selected_indices is NULL, select all indices columns in the specified order
  if (is.null(selected_indices)) {
    # First, separate population and community indices
    pop_indices <- all_indices_in_table[grepl("_p$", all_indices_in_table)]
    comm_indices <- all_indices_in_table[grepl("_c$", all_indices_in_table)]
    
    # Define the exact order for indices
    indices_order <- c(
      "cnp", "bnp", "snp", "onp", 
      "cep", "bep", "sep", "oep", 
      "cel", "bel", "sel", "oel", 
      "er1", "er2", "er3"
    )
    
    # Initialize empty vectors to store ordered indices
    ordered_pop_indices <- character(0)
    ordered_comm_indices <- character(0)
    
    # Create ordered lists of indices
    for (prefix in indices_order) {
      # Add population indices in specified order
      pop_match <- grep(paste0("^", prefix, "_p$"), pop_indices, value = TRUE)
      if (length(pop_match) > 0) {
        ordered_pop_indices <- c(ordered_pop_indices, pop_match)
      }
      
      # Add community indices in specified order
      comm_match <- grep(paste0("^", prefix, "_c$"), comm_indices, value = TRUE)
      if (length(comm_match) > 0) {
        ordered_comm_indices <- c(ordered_comm_indices, comm_match)
      }
    }
    
    # If community variables are specified, use community indices; otherwise use population indices
    if (should_use_comm_indices) {
      selected_indices <- ordered_comm_indices
    } else {
      selected_indices <- ordered_pop_indices
    }
  } else if (length(selected_indices) == 0) {
    stop("selected_indices cannot be an empty vector")
  }
  
  # Check if all selected indices exist in the table
  if (!all(selected_indices %in% names(indices_table))) {
    missing_indices <- selected_indices[!selected_indices %in% names(indices_table)]
    stop(paste("The following indices are not present in the indices_table:", 
               paste(missing_indices, collapse = ", ")))
  }
  
  # Check if all selected indices are actually indices (end with _p or _c)
  non_indices <- selected_indices[!selected_indices %in% all_indices_in_table]
  if (length(non_indices) > 0) {
    stop(paste("The following selected columns are not valid indices (should end with _p or _c):", 
               paste(non_indices, collapse = ", ")))
  }
  
  if (!type %in% c("by_index", "by_group")) {
    stop("type must be either 'by_index' or 'by_group'")
  }
  
  if (!bar_type %in% c("Vertical", "Horizontal")) {
    stop("bar_type must be either 'Vertical' or 'Horizontal'")
  }
  
  # Define population and community variables and levels
  pop_vars <- list(pop_var1, pop_var2, pop_var3)
  pop_levels <- list(pop_levels1, pop_levels2, pop_levels3)
  comm_vars <- list(comm_var1, comm_var2)
  comm_levels <- list(comm_levels1, comm_levels2)
  
  # Check if specified population and community variables exist in the table
  all_vars <- c(
    pop_vars[!sapply(pop_vars, is.null)], 
    comm_vars[!sapply(comm_vars, is.null)]
  )
  
  for (var in all_vars) {
    if (!var %in% names(indices_table)) {
      stop(paste("Variable", var, "does not exist in the indices_table"))
    }
  }
  
  # Filter indices_table based on population and community variables and levels
  filtered_table <- indices_table
  
  # Filter by population variables and levels
  for (i in 1:3) {
    var <- pop_vars[[i]]
    levels <- pop_levels[[i]]
    
    if (!is.null(var)) {
      if (is.null(levels)) {
        # If levels are not specified, use all available levels
        levels <- unique(indices_table[[var]])
      }
      
      # Check if the specified levels exist in the table
      if (!all(levels %in% unique(indices_table[[var]]))) {
        missing_levels <- levels[!levels %in% unique(indices_table[[var]])]
        stop(paste("The following levels for variable", var, "do not exist in the indices_table:", 
                   paste(missing_levels, collapse = ", ")))
      }
      
      # Filter the table
      filtered_table <- filtered_table %>%
        filter(.data[[var]] %in% levels)
    }
  }
  
  # Filter by community variables and levels
  for (i in 1:2) {
    var <- comm_vars[[i]]
    levels <- comm_levels[[i]]
    
    if (!is.null(var)) {
      if (is.null(levels)) {
        # If levels are not specified, use all available levels
        levels <- unique(filtered_table[[var]])
      }
      
      # Check if the specified levels exist in the table
      if (!all(levels %in% unique(filtered_table[[var]]))) {
        missing_levels <- levels[!levels %in% unique(filtered_table[[var]])]
        stop(paste("The following levels for variable", var, "do not exist in the indices_table:", 
                   paste(missing_levels, collapse = ", ")))
      }
      
      # Filter the table
      filtered_table <- filtered_table %>%
        filter(.data[[var]] %in% levels)
    }
  }
  
  # Check if there are any rows after filtering
  if (nrow(filtered_table) == 0) {
    stop("No data available after filtering by the specified population and community variables and levels")
  }
  
  # If community variables are specified, filter to only include community rows
  if (should_use_comm_indices) {
    community_rows <- filtered_table %>% filter(nc > 0)
    
    if (nrow(community_rows) > 0) {
      filtered_table <- community_rows
    } else {
      stop("No community rows found after filtering. Cannot proceed because community variables were specified.")
    }
    
    # Check if selected indices have _c suffix if community variables are specified
    if (!all(grepl("_c$", selected_indices))) {
      pop_indices_selected <- selected_indices[grepl("_p$", selected_indices)]
      if (length(pop_indices_selected) > 0) {
        # Convert population indices to community indices if possible
        comm_equivalents <- sub("_p$", "_c", pop_indices_selected)
        valid_comm_indices <- comm_equivalents[comm_equivalents %in% names(filtered_table)]
        
        if (length(valid_comm_indices) > 0) {
          message("Converting population indices to community indices because community variables were specified:")
          for (i in seq_along(pop_indices_selected)) {
            if (comm_equivalents[i] %in% names(filtered_table)) {
              message(paste(" -", pop_indices_selected[i], "->", comm_equivalents[i]))
              selected_indices <- c(setdiff(selected_indices, pop_indices_selected[i]), comm_equivalents[i])
            }
          }
        } else {
          stop("Population indices were selected but community variables were specified. Please select community indices instead.")
        }
      }
    }
  } else {
    # If no community variables are specified, only include population rows
    filtered_table <- filtered_table %>% filter(nc == 0)
  }
  
  # Check if all selected indices have data after filtering
  for (index in selected_indices) {
    if (all(is.na(filtered_table[[index]]))) {
      stop(paste("The index", index, "contains only NA values after filtering"))
    }
  }
  
  # Create a list to store the plots
  plots_list <- list()
  
  # Prepare a group identifier column based on population and community variables
  group_vars <- c(
    unlist(pop_vars[!sapply(pop_vars, is.null)]),
    unlist(comm_vars[!sapply(comm_vars, is.null)])
  )
  
  if (length(group_vars) == 0) {
    # If no grouping variables specified, create a dummy group
    filtered_table$group_id <- "All Data"
    group_vars <- "group_id"
  }
  
  # Create a group label column by concatenating the values of the group variables
  filtered_table <- filtered_table %>%
    mutate(group_label = apply(select(., all_of(group_vars)), 1, function(x) paste(x, collapse = "-")))
  
  # Generate plots based on the specified type
  if (type == "by_index") {
    # Generate a plot for each selected index
    for (index in selected_indices) {
      # Extract index data
      index_data <- filtered_table %>%
        select(group_label, !!index) %>%
        rename(value = !!index) %>%
        filter(!is.na(value))  # Remove NA values
      
      # Skip if no data for this index
      if (nrow(index_data) == 0) {
        warning(paste("No data available for index", index, "after filtering"))
        next
      }
      
      # Determine if this is a percentage index (el or er)
      is_percent <- grepl("(el|er[1-3])_[pc]$", index)
      
      # Create the plot
      if (bar_type == "Vertical") {
        p <- ggplot(index_data, aes(x = group_label, y = value, fill = group_label)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = if (is_percent) sprintf("%.1f%%", value) else sprintf("%.1f", value),
                        y = value + max(value) * 0.02), # Position at top of bar
                    size = 3, # Smaller font size
                    vjust = 0) + # Align to bottom of text
          scale_fill_viridis_d() + # Use viridis color palette
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none") +
          labs(title = index, x = "Group", y = "Value")
      } else {
        p <- ggplot(index_data, aes(x = value, y = group_label, fill = group_label)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = if (is_percent) sprintf("%.1f%%", value) else sprintf("%.1f", value),
                        x = value + max(value) * 0.02), # Position at end of bar
                    size = 3, # Smaller font size
                    hjust = 0) + # Align to left of text
          scale_fill_viridis_d() + # Use viridis color palette
          theme_minimal() +
          theme(legend.position = "none") +
          labs(title = index, x = "Value", y = "Group")
      }
      
      plots_list[[length(plots_list) + 1]] <- p
    }
  } else if (type == "by_group") {
    # Generate a plot for each unique group
    unique_groups <- unique(filtered_table$group_label)
    
    for (group in unique_groups) {
      # Extract group data
      group_data <- filtered_table %>%
        filter(group_label == group) %>%
        select(all_of(selected_indices)) %>%
        pivot_longer(cols = everything(), names_to = "index", values_to = "value") %>%
        filter(!is.na(value))  # Remove NA values
      
      # Skip if no data for this group
      if (nrow(group_data) == 0) {
        warning(paste("No data available for group", group, "after filtering"))
        next
      }
      
      # Order the indices in the specified order
      group_data$index <- factor(group_data$index, levels = selected_indices)
      
      # Determine which indices are percentages
      group_data$is_percent <- grepl("(el|er[1-3])_[pc]$", group_data$index)
      
      # Create the plot
      if (bar_type == "Vertical") {
        p <- ggplot(group_data, aes(x = index, y = value, fill = index)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = ifelse(is_percent, sprintf("%.1f%%", value), sprintf("%.1f", value)),
                        y = value + max(value) * 0.02), # Position at top of bar
                    size = 3, # Smaller font size
                    vjust = 0) + # Align to bottom of text
          scale_fill_viridis_d() + # Use viridis color palette
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none") +
          labs(title = group, x = "Index", y = "Value")
      } else {
        p <- ggplot(group_data, aes(x = value, y = index, fill = index)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = ifelse(is_percent, sprintf("%.1f%%", value), sprintf("%.1f", value)),
                        x = value + max(value) * 0.02), # Position at end of bar
                    size = 3, # Smaller font size
                    hjust = 0) + # Align to left of text
          scale_fill_viridis_d() + # Use viridis color palette
          theme_minimal() +
          theme(legend.position = "none") +
          labs(title = group, x = "Value", y = "Index")
      }
      
      plots_list[[length(plots_list) + 1]] <- p
    }
  }
  
  # Combine the plots using patchwork
  if (length(plots_list) == 0) {
    stop("No plots were generated")
  } else if (length(plots_list) == 1) {
    combined_plot <- plots_list[[1]]
  } else {
    # Combine plots with specified number of columns
    combined_plot <- wrap_plots(plots_list, ncol = ncol)
  }
  
  return(combined_plot)
}