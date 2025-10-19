#' Create 3D Political Extremism Gauge Plot
#'
#' @param indices_table Data frame containing the indices table from af_gauge_indices
#' @param indices_group Type of indices to use: "NP" (Normative Points), "EP" (Extremism Points), or "EL" (Extremism Levels)
#' @param rank_index Extremism Rank index to use for coloring: "ER1", "ER2", or "ER3"
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
#'
#' @return A plotly figure object representing a 3D scatter plot
#'
#' @export
af_plot_gauge <- function(indices_table,
                          indices_group = "NP",
                          rank_index = "ER1",
                          pop_var1 = NULL, pop_levels1 = NULL,
                          pop_var2 = NULL, pop_levels2 = NULL,
                          pop_var3 = NULL, pop_levels3 = NULL,
                          comm_var1 = NULL, comm_levels1 = NULL,
                          comm_var2 = NULL, comm_levels2 = NULL) {
  
  # Check required packages
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it.")
  }
  
  # Import required packages
  library(plotly)
  library(dplyr)
  
  # Input validation
  if (!is.data.frame(indices_table)) {
    stop("indices_table must be a data frame")
  }
  
  # Validate indices_group
  valid_groups <- c("NP", "EP", "EL")
  if (!indices_group %in% valid_groups) {
    stop(paste("indices_group must be one of:", paste(valid_groups, collapse = ", ")))
  }
  
  # Validate rank_index
  valid_ranks <- c("ER1", "ER2", "ER3")
  if (!rank_index %in% valid_ranks) {
    stop(paste("rank_index must be one of:", paste(valid_ranks, collapse = ", ")))
  }
  
  # Check if community variables are specified
  comm_vars_not_null <- c(comm_var1, comm_var2)[!sapply(c(comm_var1, comm_var2), is.null)]
  has_comm_vars <- length(comm_vars_not_null) > 0
  
  # Define column names based on indices_group
  prefix_map <- list(
    NP = "np",
    EP = "ep",
    EL = "el"
  )
  
  # Set columns for x, y, z axes based on indices_group and whether community variables are specified
  prefix <- prefix_map[[indices_group]]
  suffix <- if (has_comm_vars) "_c" else "_p"
  
  x_col <- paste0("c", prefix, suffix) # cognitive dimension
  y_col <- paste0("b", prefix, suffix) # behavioral dimension
  z_col <- paste0("s", prefix, suffix) # social dimension
  o_col <- paste0("o", prefix, suffix) # overall
  
  # Set color column based on rank_index
  color_col <- paste0(tolower(rank_index), suffix)
  
  # Check if columns exist in the dataframe
  required_columns <- c(x_col, y_col, z_col, o_col, color_col)
  missing_columns <- required_columns[!required_columns %in% names(indices_table)]
  
  if (length(missing_columns) > 0) {
    stop(paste("The following required columns are missing from indices_table:", 
               paste(missing_columns, collapse = ", ")))
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
  if (has_comm_vars) {
    community_rows <- filtered_table %>% filter(nc > 0)
    
    if (nrow(community_rows) > 0) {
      filtered_table <- community_rows
    } else {
      stop("No community rows found after filtering. Cannot proceed because community variables were specified.")
    }
  } else {
    # If no community variables are specified, only include population rows
    filtered_table <- filtered_table %>% filter(nc == 0)
  }
  
  # Create a group label column by concatenating the values of the group variables
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
  
  # Create hover text for each point
  filtered_table <- filtered_table %>%
    mutate(hover_text = paste0(
      "Group: ", group_label, "<br>",
      "Cognitive ", indices_group, ": ", round(get(x_col), 1), "<br>",
      "Behavioral ", indices_group, ": ", round(get(y_col), 1), "<br>",
      "Social ", indices_group, ": ", round(get(z_col), 1), "<br>",
      "Overall ", indices_group, ": ", round(get(o_col), 1), "<br>",
      rank_index, ": ", round(get(color_col), 1), "%"
    ))
  
  # Define axis titles based on the selected indices_group
  axis_titles <- list(
    NP = c('Cognitive Normative Point', 'Behavioral Normative Point', 'Social Normative Point'),
    EP = c('Cognitive Extremism Point', 'Behavioral Extremism Point', 'Social Extremism Point'),
    EL = c('Cognitive Extremism Level', 'Behavioral Extremism Level', 'Social Extremism Level')
  )
  
  # Create the 3D scatter plot
  fig <- plot_ly(filtered_table,
                 x = ~get(x_col),
                 y = ~get(y_col),
                 z = ~get(z_col),
                 text = ~hover_text,
                 hoverinfo = "text",
                 hoverlabel = list(namelength = -1),
                 type = 'scatter3d',
                 mode = 'markers',
                 marker = list(
                   color = ~get(color_col),
                   colorscale = 'Viridis',
                   colorbar = list(title = rank_index),
                   showscale = TRUE,
                   size = 8
                 )
  )
  
  # Set layout
  fig <- fig %>% layout(
    scene = list(
      xaxis = list(title = axis_titles[[indices_group]][1]),
      yaxis = list(title = axis_titles[[indices_group]][2]),
      zaxis = list(title = axis_titles[[indices_group]][3]),
      aspectmode = 'cube'
    )
  )
  
  return(fig)
}