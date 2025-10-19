#' Plot Characteristics of Extremist Groups in Populations or Communities
#'
#' @param df A data frame containing the survey dataset with extremist indicators (output from af_gauge_indices)
#' @param extremism_level Integer indicating which extremist definition to use (1, 2, or 3)
#' @param characteristics Character vector of variable names to plot as characteristics
#' @param pop_var1 Optional first population variable name
#' @param pop_levels1 Optional list of levels for pop_var1, if NULL all levels are used
#' @param pop_var2 Optional second population variable name
#' @param pop_levels2 Optional list of levels for pop_var2, if NULL all levels are used
#' @param pop_var3 Optional third population variable name
#' @param pop_levels3 Optional list of levels for pop_var3, if NULL all levels are used
#' @param comm_var1 Optional first community variable name
#' @param comm_levels1 Optional list of levels for comm_var1, if NULL all levels are used
#' @param comm_var2 Optional second community variable name
#' @param comm_levels2 Optional list of levels for comm_var2, if NULL all levels are used
#' @param type Character string indicating plot organization, either "by_characteristic" or "by_group"
#' @param bar_type Character string specifying bar orientation, either "Vertical" or "Horizontal"
#' @param ncol Integer specifying the number of columns in the combined plot
#'
#' @return A ggplot object containing all created plots combined
#'
#' @export
af_extremists <- function(df,
                          extremism_level,
                          characteristics,
                          pop_var1 = NULL, pop_levels1 = NULL,
                          pop_var2 = NULL, pop_levels2 = NULL,
                          pop_var3 = NULL, pop_levels3 = NULL,
                          comm_var1 = NULL, comm_levels1 = NULL,
                          comm_var2 = NULL, comm_levels2 = NULL,
                          type = "by_characteristic",
                          bar_type = "Vertical",
                          ncol = 2) {
  
  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is needed for this function to work. Please install it.")
  }
  if (!requireNamespace("viridis", quietly = TRUE)) {
    stop("Package 'viridis' is needed for this function to work. Please install it.")
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is needed for this function to work. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is needed for this function to work. Please install it.")
  }
  
  # Import libraries
  library(ggplot2)
  library(viridis)
  library(patchwork)
  library(dplyr)
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  if (!is.numeric(extremism_level) || !(extremism_level %in% c(1, 2, 3))) {
    stop("extremism_level must be 1, 2, or 3")
  }
  
  if (!is.character(characteristics) || length(characteristics) < 1) {
    stop("characteristics must be a character vector with at least one element")
  }
  
  for (char in characteristics) {
    if (!char %in% names(df)) {
      stop(paste("Characteristic variable", char, "not found in the data frame"))
    }
  }
  
  if (!type %in% c("by_characteristic", "by_group")) {
    stop("type must be either 'by_characteristic' or 'by_group'")
  }
  
  if (!bar_type %in% c("Vertical", "Horizontal")) {
    stop("bar_type must be either 'Vertical' or 'Horizontal'")
  }
  
  if (!is.numeric(ncol) || ncol < 1) {
    stop("ncol must be a positive integer")
  }
  
  # Check if we're working with populations or communities
  use_communities <- !is.null(comm_var1) || !is.null(comm_var2)
  
  # Define extremist variable name based on type and community/population focus
  extremist_var <- paste0("extremist_", extremism_level, ifelse(use_communities, "_c", "_p"))
  
  # Check if extremist variable exists
  if (!extremist_var %in% names(df)) {
    stop(paste("Extremist variable", extremist_var, "not found in the data frame"))
  }
  
  # Validate population variables if specified
  pop_vars <- c(pop_var1, pop_var2, pop_var3)
  pop_vars <- pop_vars[!sapply(pop_vars, is.null)]
  
  if (length(pop_vars) > 0) {
    for (var in pop_vars) {
      if (!var %in% names(df)) {
        stop(paste("Population variable", var, "not found in the data frame"))
      }
    }
  }
  
  # Validate community variables if specified
  comm_vars <- c(comm_var1, comm_var2)
  comm_vars <- comm_vars[!sapply(comm_vars, is.null)]
  
  if (length(comm_vars) > 0) {
    if (length(pop_vars) == 0) {
      stop("Communities cannot be defined without at least one population variable")
    }
    
    for (var in comm_vars) {
      if (!var %in% names(df)) {
        stop(paste("Community variable", var, "not found in the data frame"))
      }
    }
  }
  
  # Set default population levels if none specified
  if (!is.null(pop_var1) && is.null(pop_levels1)) {
    pop_levels1 <- unique(df[[pop_var1]])
  }
  if (!is.null(pop_var2) && is.null(pop_levels2)) {
    pop_levels2 <- unique(df[[pop_var2]])
  }
  if (!is.null(pop_var3) && is.null(pop_levels3)) {
    pop_levels3 <- unique(df[[pop_var3]])
  }
  
  # Set default community levels if none specified
  if (!is.null(comm_var1) && is.null(comm_levels1)) {
    comm_levels1 <- unique(df[[comm_var1]])
  }
  if (!is.null(comm_var2) && is.null(comm_levels2)) {
    comm_levels2 <- unique(df[[comm_var2]])
  }
  
  # Define population combinations
  pop_combinations <- list()
  
  if (is.null(pop_var1)) {
    # If no population variables, use the entire dataset as one population
    pop_combinations[[1]] <- list(
      vars = NULL,
      filter = rep(TRUE, nrow(df)),
      name = "All"
    )
  } else {
    # Create combinations based on specified population variables and levels
    pop_vars <- c(pop_var1, pop_var2, pop_var3)
    pop_vars <- pop_vars[!sapply(pop_vars, is.null)]
    
    pop_levels <- list(pop_levels1, pop_levels2, pop_levels3)
    pop_levels <- pop_levels[!sapply(pop_vars, is.null)]
    
    # Generate all possible combinations
    pop_combo_idx <- 1
    
    # Function to recursively build population filter combinations
    build_pop_combinations <- function(curr_combo = list(), curr_depth = 1) {
      if (curr_depth > length(pop_vars)) {
        if (length(curr_combo) > 0) {
          # Build filter expression
          filter_expr <- rep(TRUE, nrow(df))
          vars_used <- names(curr_combo)
          
          for (var in vars_used) {
            filter_expr <- filter_expr & (df[[var]] %in% curr_combo[[var]])
          }
          
          # Create name for this combination
          combo_name <- ""
          for (var in vars_used) {
            if (combo_name != "") combo_name <- paste0(combo_name, ", ")
            combo_name <- paste0(combo_name, var, ": ", paste(curr_combo[[var]], collapse = ","))
          }
          
          # Save this combination
          pop_combinations[[pop_combo_idx]] <<- list(
            vars = curr_combo,
            filter = filter_expr,
            name = combo_name
          )
          pop_combo_idx <<- pop_combo_idx + 1
        }
        return()
      }
      
      # Current variable and its levels
      curr_var <- pop_vars[curr_depth]
      curr_levels <- pop_levels[[curr_depth]]
      
      # For each level of current variable
      for (level in curr_levels) {
        new_combo <- curr_combo
        new_combo[[curr_var]] <- level
        build_pop_combinations(new_combo, curr_depth + 1)
      }
    }
    
    # Start building combinations
    build_pop_combinations()
  }
  
  # Define community combinations
  comm_combinations <- list()
  
  if (length(comm_vars) > 0) {
    # Function to build community combinations for a given population
    build_community_combinations <- function(pop_df, pop_combo) {
      comm_combo_list <- list()
      comm_combo_idx <- 1
      
      # Function to recursively build community filter combinations
      build_comm_combinations <- function(curr_combo = list(), curr_depth = 1) {
        if (curr_depth > length(comm_vars)) {
          if (length(curr_combo) > 0) {
            # Build filter expression
            filter_expr <- rep(TRUE, nrow(pop_df))
            vars_used <- names(curr_combo)
            
            for (var in vars_used) {
              filter_expr <- filter_expr & (pop_df[[var]] %in% curr_combo[[var]])
            }
            
            # Create name for this combination
            combo_name <- ""
            for (var in vars_used) {
              if (combo_name != "") combo_name <- paste0(combo_name, ", ")
              combo_name <- paste0(combo_name, var, ": ", paste(curr_combo[[var]], collapse = ","))
            }
            
            # Save this combination
            comm_combo_list[[comm_combo_idx]] <<- list(
              vars = curr_combo,
              filter = filter_expr,
              name = combo_name
            )
            comm_combo_idx <<- comm_combo_idx + 1
          }
          return()
        }
        
        # Current variable and its levels
        curr_var <- comm_vars[curr_depth]
        
        # Use specified levels or fallback to what's available in the population
        if (curr_depth == 1 && !is.null(comm_levels1)) {
          available_levels <- intersect(comm_levels1, unique(pop_df[[curr_var]]))
          if (length(available_levels) == 0) {
            available_levels <- unique(pop_df[[curr_var]])
          }
        } else if (curr_depth == 2 && !is.null(comm_levels2)) {
          available_levels <- intersect(comm_levels2, unique(pop_df[[curr_var]]))
          if (length(available_levels) == 0) {
            available_levels <- unique(pop_df[[curr_var]])
          }
        } else {
          available_levels <- unique(pop_df[[curr_var]])
        }
        
        # For each level of current variable
        for (level in available_levels) {
          new_combo <- curr_combo
          new_combo[[curr_var]] <- level
          build_comm_combinations(new_combo, curr_depth + 1)
        }
      }
      
      # Start building community combinations
      build_comm_combinations()
      
      return(comm_combo_list)
    }
  }
  
  # Store all plots
  all_plots <- list()
  plot_idx <- 1
  
  # Generate plots based on type
  if (type == "by_characteristic") {
    # For each characteristic
    for (char in characteristics) {
      # Determine variable type
      is_numeric <- is.numeric(df[[char]])
      is_ordered <- is.ordered(df[[char]])
      
      # For each population combination
      for (pop_idx in seq_along(pop_combinations)) {
        pop_combo <- pop_combinations[[pop_idx]]
        pop_filter <- pop_combo$filter
        pop_df <- df[pop_filter, ]
        
        if (nrow(pop_df) == 0) {
          next  # Skip empty populations
        }
        
        if (use_communities) {
          # Process communities for this population
          comm_combo_list <- build_community_combinations(pop_df, pop_combo)
          
          # For each community in this population
          for (comm_idx in seq_along(comm_combo_list)) {
            comm_combo <- comm_combo_list[[comm_idx]]
            comm_filter_local <- comm_combo$filter
            
            # Convert local filter to global filter
            pop_indices_in_df <- which(pop_filter)
            comm_indices_in_pop <- which(comm_filter_local)
            
            if (length(comm_indices_in_pop) == 0) {
              next  # Skip empty communities
            }
            
            comm_indices_in_df <- pop_indices_in_df[comm_indices_in_pop]
            comm_filter_global <- rep(FALSE, nrow(df))
            comm_filter_global[comm_indices_in_df] <- TRUE
            
            comm_df <- df[comm_filter_global, ]
            
            # Filter extremists only
            extremist_df <- comm_df[comm_df[[extremist_var]] == 1, ]
            
            if (nrow(extremist_df) == 0) {
              next  # Skip if no extremists in this community
            }
            
            # Generate plot for this community and characteristic
            plot_title <- paste0("Characteristic: ", char, "\n", 
                                 pop_combo$name, "\n", 
                                 comm_combo$name, " (N=", nrow(extremist_df), ")")
            
            p <- create_plot(extremist_df, char, is_numeric, is_ordered, plot_title, bar_type)
            all_plots[[plot_idx]] <- p
            plot_idx <- plot_idx + 1
          }
        } else {
          # Process for population only (no communities)
          # Filter extremists only
          extremist_df <- pop_df[pop_df[[extremist_var]] == 1, ]
          
          if (nrow(extremist_df) == 0) {
            next  # Skip if no extremists in this population
          }
          
          # Generate plot for this population and characteristic
          plot_title <- paste0("Characteristic: ", char, "\n", 
                               pop_combo$name, " (N=", nrow(extremist_df), ")")
          
          p <- create_plot(extremist_df, char, is_numeric, is_ordered, plot_title, bar_type)
          all_plots[[plot_idx]] <- p
          plot_idx <- plot_idx + 1
        }
      }
    }
  } else {  # type == "by_group"
    # For each population combination
    for (pop_idx in seq_along(pop_combinations)) {
      pop_combo <- pop_combinations[[pop_idx]]
      pop_filter <- pop_combo$filter
      pop_df <- df[pop_filter, ]
      
      if (nrow(pop_df) == 0) {
        next  # Skip empty populations
      }
      
      if (use_communities) {
        # Process communities for this population
        comm_combo_list <- build_community_combinations(pop_df, pop_combo)
        
        # For each community in this population
        for (comm_idx in seq_along(comm_combo_list)) {
          comm_combo <- comm_combo_list[[comm_idx]]
          comm_filter_local <- comm_combo$filter
          
          # Convert local filter to global filter
          pop_indices_in_df <- which(pop_filter)
          comm_indices_in_pop <- which(comm_filter_local)
          
          if (length(comm_indices_in_pop) == 0) {
            next  # Skip empty communities
          }
          
          comm_indices_in_df <- pop_indices_in_df[comm_indices_in_pop]
          comm_filter_global <- rep(FALSE, nrow(df))
          comm_filter_global[comm_indices_in_df] <- TRUE
          
          comm_df <- df[comm_filter_global, ]
          
          # Filter extremists only
          extremist_df <- comm_df[comm_df[[extremist_var]] == 1, ]
          
          if (nrow(extremist_df) == 0) {
            next  # Skip if no extremists in this community
          }
          
          # For each characteristic
          for (char in characteristics) {
            # Determine variable type
            is_numeric <- is.numeric(df[[char]])
            is_ordered <- is.ordered(df[[char]])
            
            # Generate plot for this community and characteristic
            plot_title <- paste0("Population: ", pop_combo$name, "\n", 
                                 "Community: ", comm_combo$name, " (N=", nrow(extremist_df), ")\n",
                                 "Characteristic: ", char)
            
            p <- create_plot(extremist_df, char, is_numeric, is_ordered, plot_title, bar_type)
            all_plots[[plot_idx]] <- p
            plot_idx <- plot_idx + 1
          }
        }
      } else {
        # Process for population only (no communities)
        # Filter extremists only
        extremist_df <- pop_df[pop_df[[extremist_var]] == 1, ]
        
        if (nrow(extremist_df) == 0) {
          next  # Skip if no extremists in this population
        }
        
        # For each characteristic
        for (char in characteristics) {
          # Determine variable type
          is_numeric <- is.numeric(df[[char]])
          is_ordered <- is.ordered(df[[char]])
          
          # Generate plot for this population and characteristic
          plot_title <- paste0("Population: ", pop_combo$name, " (N=", nrow(extremist_df), ")\n",
                               "Characteristic: ", char)
          
          p <- create_plot(extremist_df, char, is_numeric, is_ordered, plot_title, bar_type)
          all_plots[[plot_idx]] <- p
          plot_idx <- plot_idx + 1
        }
      }
    }
  }
  
  # Check if any plots were created
  if (length(all_plots) == 0) {
    stop("No valid plots could be created. Check your extremism_level, population, and community settings.")
  }
  
  # Combine plots using patchwork
  combined_plot <- wrap_plots(all_plots, ncol = ncol)
  
  return(combined_plot)
}

# Helper function to create appropriate plot based on variable type
create_plot <- function(data, var_name, is_numeric, is_ordered, plot_title, bar_type) {
  if (is_numeric) {
    # Create density plot for numeric variables
    p <- ggplot(data, aes(x = .data[[var_name]])) +
      geom_density(fill = viridis(1), alpha = 0.7) +
      labs(title = plot_title,
           x = var_name,
           y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8))
  } else {
    # Prepare data for bar chart
    if (bar_type == "Vertical") {
      # For vertical bars
      freq_data <- data %>%
        group_by_at(var_name) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(freq = count / sum(count) * 100)
      
      # Create bar chart
      p <- ggplot(freq_data, aes(x = .data[[var_name]], y = freq, fill = .data[[var_name]])) +
        geom_bar(stat = "identity") +
        scale_fill_viridis_d() +
        geom_text(aes(label = sprintf("%.1f%%", freq)), vjust = -0.5, size = 3) +
        labs(title = plot_title,
             x = var_name,
             y = "Frequency (%)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(size = 9),
              axis.text = element_text(size = 8),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
    } else {
      # For horizontal bars
      freq_data <- data %>%
        group_by_at(var_name) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(freq = count / sum(count) * 100)
      
      # Create horizontal bar chart
      p <- ggplot(freq_data, aes(x = freq, y = .data[[var_name]], fill = .data[[var_name]])) +
        geom_bar(stat = "identity") +
        scale_fill_viridis_d() +
        geom_text(aes(label = sprintf("%.1f%%", freq)), hjust = -0.1, size = 3) +
        labs(title = plot_title,
             x = "Frequency (%)",
             y = var_name) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 10),
              axis.title = element_text(size = 9),
              axis.text = element_text(size = 8),
              legend.position = "none")
    }
  }
  
  return(p)
}

#' Create Extremist Tracking Table Across Survey Waves
#'
#' @param df Dataframe containing survey data with wave, extremist flag, and characteristic variables
#' @param wave_var Character string naming the wave variable in the dataframe
#' @param wave_order Character vector defining the expected order of waves (e.g., c("First", "Second", "Third", "Fourth", "Fifth", "Sixth"))
#' @param extremist_flag_var Character string naming the extremist flag variable (0/1, where 1 = extremist)
#' @param table_title Character string for the main table title
#' @param characteristics List of lists defining characteristics to display. Each inner list should contain:
#'   \itemize{
#'     \item var: variable name in the dataframe
#'     \item type: "categorical" or "numerical"
#'     \item category: category value to analyze (required if type is "categorical"), or NULL to include all categories
#'     \item title: display title for the characteristic
#'     \item subtitle: display subtitle (should include scale information)
#'   }
#' @param notes Character vector of notes to add to the table footer
#'
#' @return A gt table object showing extremist characteristics across survey waves
#'
#' @details 
#' For categorical variables, displays percentage of extremists with the specified category,
#' with overall population percentage in parentheses. If category = NULL for categorical
#' variables, all categories of that variable will be included as separate characteristics.
#' For numerical variables, displays median value for extremists with overall population 
#' median in parentheses.
#' 
#' Records with missing data in relevant variables are removed and reported. Wave columns 
#' are ordered according to wave_order parameter with sample sizes displayed.
#'
af_create_extremist_tracking_table <- function(df, wave_var, wave_order, extremist_flag_var, table_title, characteristics, notes) {
  
  # Expand characteristics with category = NULL to include all categories
  expanded_characteristics <- list()
  
  for (char in characteristics) {
    if (char$type == "categorical" && is.null(char$category)) {
      # Get all unique categories for this variable
      all_categories <- unique(df[[char$var]])
      all_categories <- all_categories[!is.na(all_categories)]
      
      # Create separate characteristic for each category
      for (cat in all_categories) {
        expanded_char <- char
        expanded_char$category <- cat
        expanded_char$title <- paste(char$title, "-", cat)
        expanded_characteristics <- append(expanded_characteristics, list(expanded_char))
      }
    } else {
      expanded_characteristics <- append(expanded_characteristics, list(char))
    }
  }
  
  # Use expanded characteristics for the rest of the function
  characteristics <- expanded_characteristics
  
  # Identify variables used by the function
  char_vars <- sapply(characteristics, function(x) x$var)
  relevant_vars <- c(wave_var, extremist_flag_var, char_vars)
  
  # Check for missing data only in relevant variables and remove
  initial_nrow <- nrow(df)
  df_clean <- df[complete.cases(df[, relevant_vars, drop = FALSE]), ]
  removed_count <- initial_nrow - nrow(df_clean)
  
  if (removed_count > 0) {
    missing_vars <- relevant_vars[sapply(df[, relevant_vars, drop = FALSE], function(x) any(is.na(x)))]
    cat("Removed", removed_count, "records with missing data.\n")
    cat("Variables with missing data:", paste(missing_vars, collapse = ", "), "\n")
  }
  
  # Get actual waves in data and order them according to wave_order
  actual_waves <- intersect(wave_order, unique(df_clean[[wave_var]]))
  
  # Initialize results dataframe
  char_names <- sapply(characteristics, function(x) paste(x$title, x$subtitle, sep = "<br>"))
  
  results_df <- data.frame(
    Characteristic = char_names,
    stringsAsFactors = FALSE
  )
  
  # Add columns for each wave
  for (wave in actual_waves) {
    # Calculate sample size for this wave
    n_extremists <- sum(df_clean[[wave_var]] == wave & df_clean[[extremist_flag_var]] == 1)
    col_name <- paste0(wave, " (N=", n_extremists, ")")
    
    # Calculate values for each characteristic
    wave_values <- character(length(characteristics))
    
    for (i in seq_along(characteristics)) {
      char <- characteristics[[i]]
      var_name <- char$var
      char_type <- char$type
      
      # Filter to current wave
      wave_data <- df_clean[df_clean[[wave_var]] == wave, ]
      extremist_data <- wave_data[wave_data[[extremist_flag_var]] == 1, ]
      
      if (char_type == "categorical") {
        category_val <- char$category
        
        # Calculate extremist percentage
        extremist_pct <- mean(extremist_data[[var_name]] == category_val, na.rm = TRUE) * 100
        
        # Calculate overall population percentage
        overall_pct <- mean(wave_data[[var_name]] == category_val, na.rm = TRUE) * 100
        
        wave_values[i] <- sprintf("%.2f%%<br>(%.2f%%)", extremist_pct, overall_pct)
        
      } else if (char_type == "numerical") {
        # Calculate extremist median
        extremist_median <- median(extremist_data[[var_name]], na.rm = TRUE)
        
        # Calculate overall population median
        overall_median <- median(wave_data[[var_name]], na.rm = TRUE)
        
        wave_values[i] <- sprintf("%.2f<br>(%.2f)", extremist_median, overall_median)
      }
    }
    
    results_df[[col_name]] <- wave_values
  }
  
  # Create gt table
  gt_table <- results_df %>%
    gt() %>%
    tab_header(
      title = table_title
    ) %>%
    cols_label(
      Characteristic = ""
    ) %>%
    fmt_markdown(columns = everything())
  
  # Add notes
  for (note in notes) {
    gt_table <- gt_table %>%
      tab_footnote(footnote = note)
  }
  
  return(gt_table)
}

#' Create Extremist Tracking Plot Across Survey Waves
#'
#' @param df Dataframe containing survey data with wave, extremist flag, and characteristic variables
#' @param wave_var Character string naming the wave variable in the dataframe
#' @param wave_order Character vector defining the expected order of waves (e.g., c("First", "Second", "Third", "Fourth", "Fifth", "Sixth"))
#' @param extremist_flag_var Character string naming the extremist flag variable (0/1, where 1 = extremist)
#' @param characteristics List of lists defining characteristics to display. All must be same type. Each inner list should contain:
#'   \itemize{
#'     \item var: variable name in the dataframe
#'     \item type: "categorical" or "numerical" (all characteristics must have same type)
#'     \item category: category value to analyze (required if type is "categorical"), or NULL to include all categories
#'     \item title: display title for the characteristic
#'     \item subtitle: display subtitle (should include scale information)
#'   }
#' @param line_labels Character vector of labels for each line. If NULL, uses category names for categorical variables
#' @param BW Logical indicating whether to use black and white (TRUE) or color (FALSE, uses viridis colors)
#' @param plot_title Character string for the plot title
#'
#' @return A ggplot object showing extremist characteristics across survey waves
#'
#' @details 
#' Creates a line plot with up to 6 lines, each representing a characteristic or category.
#' For categorical variables, plots percentage of extremists with each category.
#' For numerical variables, plots median values for extremists.
#' Uses different symbols and line types for each line.
#'
af_create_extremist_tracking_plot <- function(df, wave_var, wave_order, extremist_flag_var, characteristics, line_labels = NULL, BW = FALSE, plot_title) {
  
  # Validate that all characteristics have the same type
  char_types <- sapply(characteristics, function(x) x$type)
  if (length(unique(char_types)) > 1) {
    stop("All characteristics must be of the same type (either all 'categorical' or all 'numerical')")
  }
  
  char_type <- char_types[1]
  
  # Expand characteristics with category = NULL to include all categories
  expanded_characteristics <- list()
  category_names <- character()
  
  for (char in characteristics) {
    if (char$type == "categorical" && is.null(char$category)) {
      # Get all unique categories for this variable
      all_categories <- unique(df[[char$var]])
      all_categories <- all_categories[!is.na(all_categories)]
      
      # Create separate characteristic for each category
      for (cat in all_categories) {
        expanded_char <- char
        expanded_char$category <- cat
        expanded_characteristics <- append(expanded_characteristics, list(expanded_char))
        category_names <- c(category_names, as.character(cat))
      }
    } else if (char$type == "categorical") {
      expanded_characteristics <- append(expanded_characteristics, list(char))
      category_names <- c(category_names, as.character(char$category))
    } else {
      expanded_characteristics <- append(expanded_characteristics, list(char))
      category_names <- c(category_names, char$title)
    }
  }
  
  # Use expanded characteristics
  characteristics <- expanded_characteristics
  
  # Set line labels
  if (is.null(line_labels)) {
    line_labels <- category_names
  }
  
  # Identify variables used by the function
  char_vars <- sapply(characteristics, function(x) x$var)
  relevant_vars <- c(wave_var, extremist_flag_var, char_vars)
  
  # Check for missing data only in relevant variables and remove
  initial_nrow <- nrow(df)
  df_clean <- df[complete.cases(df[, relevant_vars, drop = FALSE]), ]
  removed_count <- initial_nrow - nrow(df_clean)
  
  if (removed_count > 0) {
    missing_vars <- relevant_vars[sapply(df[, relevant_vars, drop = FALSE], function(x) any(is.na(x)))]
    cat("Removed", removed_count, "records with missing data.\n")
    cat("Variables with missing data:", paste(missing_vars, collapse = ", "), "\n")
  }
  
  # Create plotting data
  plot_data <- data.frame()
  
  for (i in seq_along(characteristics)) {
    char <- characteristics[[i]]
    var_name <- char$var
    line_label <- line_labels[i]
    
    for (wave in wave_order) {
      # Filter to current wave
      wave_data <- df_clean[df_clean[[wave_var]] == wave, ]
      extremist_data <- wave_data[wave_data[[extremist_flag_var]] == 1, ]
      
      if (char_type == "categorical") {
        category_val <- char$category
        # Calculate extremist percentage
        value <- mean(extremist_data[[var_name]] == category_val, na.rm = TRUE) * 100
      } else if (char_type == "numerical") {
        # Calculate extremist median
        value <- median(extremist_data[[var_name]], na.rm = TRUE)
      }
      
      plot_data <- rbind(plot_data, data.frame(
        Wave = wave,
        Value = value,
        Characteristic = line_label,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Ensure Wave is ordered correctly
  plot_data$Wave <- factor(plot_data$Wave, levels = wave_order)
  plot_data$Characteristic <- factor(plot_data$Characteristic, levels = line_labels)
  
  # Define symbols and line types (up to 6)
  symbols <- c(16, 17, 15, 18, 8, 11)  # circle, triangle, square, diamond, asterisk, hexagon
  line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  
  # Set Y-axis label
  y_label <- if (char_type == "categorical") "Percentage %" else "Median Score"
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = Wave, y = Value, group = Characteristic)) +
    geom_line(aes(linetype = Characteristic), linewidth = 1) +
    geom_point(aes(shape = Characteristic), size = 3) +
    scale_shape_manual(values = rep(symbols, length.out = length(line_labels))) +
    scale_linetype_manual(values = rep(line_types, length.out = length(line_labels))) +
    labs(
      title = plot_title,
      x = "Survey Wave",
      y = y_label
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank()
    )
  
  # Add color scale
  if (BW) {
    p <- p + scale_color_grey(start = 0.2, end = 0.8) +
      aes(color = Characteristic)
  } else {
    p <- p + scale_color_viridis_d() +
      aes(color = Characteristic)
  }
  
  return(p)
}