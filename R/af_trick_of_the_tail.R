#' Get Method Information for Tail Threshold Calculations
#'
#' This function returns the parameters and label for a specific tail threshold method.
#'
#' @param method Character string specifying the method. Must be one of:
#'   "Quantile", "MAD", "Qn", "Sn", "Tau", "ZScore".
#' @param method_params Named list containing parameters for the method.
#'
#' @return List containing:
#'   \item{params}{Named list of parameters to pass to af_tail_threshold}
#'   \item{label}{Character string describing the method and its parameters}
#'
#' @export
af_get_method_info <- function(method, method_params) {
  # Input validation
  valid_methods <- c("Quantile", "MAD", "Qn", "Sn", "Tau", "ZScore")
  if (!method %in% valid_methods) {
    stop(paste("method must be one of:", paste(valid_methods, collapse = ", ")))
  }
  
  if (!is.list(method_params)) {
    stop("method_params must be a list")
  }
  
  switch(method,
         "MAD" = list(
           params = list(type = "MAD", k_factor = method_params$k_factor),
           label = paste0("Median + ", method_params$k_factor, " * MAD")
         ),
         "Quantile" = list(
           params = list(type = "Quantile", q_pct = method_params$q_pct),
           label = paste0("Top ", 100 - 100 * method_params$q_pct, "%")
         ),
         "ZScore" = list(
           params = list(type = "ZScore", z_pct = method_params$z_pct),
           label = paste0("Z >= ", round(qnorm(method_params$z_pct), 2), " (≈ Top ", 100 - 100 * method_params$z_pct, "%)")
         ),
         "Qn" = list(
           params = list(type = "Qn", k_factor = method_params$k_factor),
           label = paste0("Median + ", method_params$k_factor, " * Qn")
         ),
         "Sn" = list(
           params = list(type = "Sn", k_factor = method_params$k_factor),
           label = paste0("Median + ", method_params$k_factor, " * Sn")
         ),
         "Tau" = list(
           params = list(type = "Tau", k_factor = method_params$k_factor),
           label = paste0("Median + ", method_params$k_factor, " * Tau")
         )
  )
}

#' Get Method-Specific Label for Legend
#'
#' This function returns a short label for a specific tail threshold method
#' suitable for use in legends.
#'
#' @param method Character string specifying the method. Must be one of:
#'   "Quantile", "MAD", "Qn", "Sn", "Tau", "ZScore".
#'
#' @return Character string with the method label
#'
#' @export
af_get_method_label <- function(method) {
  # Input validation
  valid_methods <- c("Quantile", "MAD", "Qn", "Sn", "Tau", "ZScore")
  if (!method %in% valid_methods) {
    stop(paste("method must be one of:", paste(valid_methods, collapse = ", ")))
  }
  
  switch(method,
         "MAD" = "Median+MAD",
         "Quantile" = "Quantile", 
         "ZScore" = "Z-Score",
         "Qn" = "Median+Qn",
         "Sn" = "Median+Sn",
         "Tau" = "Median+Tau"
  )
}

#' Compare Different Tail Threshold Methods
#'
#' This function compares different methods for calculating tail thresholds across
#' population groups, using the af_tail_threshold function for consistent calculations.
#'
#' @param df Data frame containing the survey dataset
#' @param group_selections Named list where names are variable names and values are 
#'   vectors of group values to analyze
#' @param measure_var Character string specifying the column name of the measure variable
#' @param methods Character vector of exactly 3 methods to compare. Must be from:
#'   "Quantile", "MAD", "Qn", "Sn", "Tau", "ZScore". Default is c("MAD", "Quantile", "ZScore").
#' @param method_params Named list containing parameters for each method. Should include:
#'   \itemize{
#'     \item k_factor: Factor for MAD method (default: 1.5)
#'     \item q_pct: Percentile for Quantile method (default: 0.85)
#'     \item z_pct: Percentile for ZScore method (default: 0.85)
#'   }
#'
#' @return List containing:
#'   \item{results}{Data frame with threshold comparison results for each group}
#'   \item{plots}{List of ggplot objects showing distributions and thresholds}
#'
#' @details
#' The function processes all combinations of group values specified in group_selections
#' and compares the three selected threshold methods using af_tail_threshold.
#' For each combination, it creates a plot showing the distribution with threshold lines.
#'
#' @examples
#' # Example with different methods
#' group_sel <- list(country = c("USA", "UK"), wave = c("W1", "W2"))
#' results <- af_compare_tail_methods(df, group_sel, "extremism_score",
#'                                    methods = c("MAD", "Quantile", "ZScore"))
#'
#' @export
af_compare_tail_methods <- function(df, 
                                    group_selections, 
                                    measure_var,
                                    methods = c("Quantile", "MAD", "Sn", "Qn", "Tau", "ZScore"),
                                    method_params = list(
                                      k_factor = 1.5,
                                      q_pct = 0.85,
                                      z_pct = 0.85
                                    )) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  if (!is.list(group_selections) || is.null(names(group_selections))) {
    stop("group_selections must be a named list")
  }
  
  if (!is.character(measure_var) || length(measure_var) != 1) {
    stop("measure_var must be a single character string")
  }
  
  if (!measure_var %in% names(df)) {
    stop("measure_var must be a column name in df")
  }
  
  if (!is.numeric(df[[measure_var]])) {
    stop("measure_var must contain numeric values")
  }
  
  valid_methods <- c("Quantile", "MAD", "Qn", "Sn", "Tau", "ZScore")
  if (!is.character(methods) || length(methods) != 3) {
    stop("methods must be a character vector of exactly 3 elements")
  }
  
  if (!all(methods %in% valid_methods)) {
    stop(paste("All methods must be one of:", paste(valid_methods, collapse = ", ")))
  }
  
  if (!is.list(method_params)) {
    stop("method_params must be a list")
  }
  
  # Validate group selections
  for (var in names(group_selections)) {
    if (!var %in% names(df)) {
      stop(paste("Group variable", var, "not found in df"))
    }
  }
  
  # Set default parameters if missing
  default_params <- list(k_factor = 1.5, q_pct = 0.85, z_pct = 0.85)
  for (param in names(default_params)) {
    if (!param %in% names(method_params)) {
      method_params[[param]] <- default_params[[param]]
    }
  }
  
  # Initialize empty results dataframe
  results <- data.frame(
    Method = character(),
    Parameter = character(),
    Threshold = numeric(),
    Tail_Size = integer(),
    Tail_Uniques = integer(),
    Avg_Tail_Score = numeric(),
    Median_Tail_Score = numeric(),
    Max_Tail_Score = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Add columns for group variables
  for (var in names(group_selections)) {
    results[[paste0("Group_", var)]] <- character()
  }
  
  # Initialize list for storing plots
  score_plots <- list()
  plot_counter <- 1
  
  # Function to process one combination of group values
  process_group_combination <- function(group_filters, current_combination) {
    # Apply all filters
    selected_population <- df
    for (i in seq_along(group_filters)) {
      selected_population <- selected_population[selected_population[[names(group_filters)[i]]] == group_filters[[i]], ]
    }
    
    # Extract the survey measure scores
    scores <- selected_population[[measure_var]]
    
    # Skip if no data after filtering
    if (length(scores) == 0) return(NULL)
    
    # Overall population size
    Np <- nrow(selected_population)
    
    # Calculate thresholds for all three methods
    thresholds <- numeric(3)
    method_labels <- character(3)
    
    for (i in 1:3) {
      method <- methods[i]
      method_info <- af_get_method_info(method, method_params)
      
      # Calculate threshold using af_tail_threshold
      tryCatch({
        thresholds[i] <- do.call(af_tail_threshold, c(list(scores = scores), method_info$params))
        method_labels[i] <- method_info$label
      }, error = function(e) {
        warning(paste("Error calculating", method, "threshold:", e$message))
        thresholds[i] <- NA
        method_labels[i] <- paste(method, "(Error)")
      })
      
      # Calculate tail statistics
      if (!is.na(thresholds[i])) {
        tail_values <- scores[scores >= thresholds[i]]
        
        current_result <- data.frame(
          Method = method,
          Parameter = method_labels[i],
          Threshold = thresholds[i],
          Tail_Size = length(tail_values),
          Tail_Uniques = length(unique(tail_values)),
          Avg_Tail_Score = if(length(tail_values) > 0) mean(tail_values) else NA,
          Median_Tail_Score = if(length(tail_values) > 0) median(tail_values) else NA,
          Max_Tail_Score = if(length(tail_values) > 0) max(tail_values) else NA,
          stringsAsFactors = FALSE
        )
        
        # Add group variable columns
        for (j in seq_along(group_filters)) {
          current_result[[paste0("Group_", names(group_filters)[j])]] <- group_filters[[j]]
        }
        
        results <<- rbind(results, current_result)
      }
    }
    
    # Create plot title with current combination
    plot_title <- paste(paste(names(group_filters), group_filters, sep = ": ", collapse = ", "))
    
    # Load ggplot2 if available
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      library(ggplot2)
      
      # Define colors for the three methods
      method_colors <- c("red", "blue", "darkgreen")
      
      # Create plot
      score_plot <- ggplot(selected_population, aes(x = .data[[measure_var]])) +
        geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black", alpha = 0.7, aes(y = after_stat(density))) +
        geom_density(color = "darkorange", fill = "darkorange", alpha = 0.4) +
        labs(title = plot_title,
             x = measure_var, 
             y = "Density") +
        annotate("text", x = Inf, y = -Inf, label = paste("Np =", Np), hjust = 1.1, vjust = -0.2)
      
      # Add threshold lines for each method
      for (i in 1:3) {
        if (!is.na(thresholds[i])) {
          score_plot <- score_plot +
            geom_vline(xintercept = thresholds[i], color = method_colors[i], linetype = "dashed", linewidth = 1)
        }
      }
      
      score_plots[[plot_counter]] <<- score_plot
      plot_counter <<- plot_counter + 1
    } else {
      warning("ggplot2 package not available - plots will not be generated")
    }
  }
  
  # Generate all combinations of group values
  process_combinations <- function(current_combination = list(), depth = 1) {
    if (depth > length(group_selections)) {
      process_group_combination(current_combination, names(current_combination))
      return()
    }
    
    current_var <- names(group_selections)[depth]
    for (val in group_selections[[current_var]]) {
      new_combination <- current_combination
      new_combination[[current_var]] <- val
      process_combinations(new_combination, depth + 1)
    }
  }
  
  # Start processing all combinations
  process_combinations()
  
  # Return results and plots
  list(
    results = results,
    plots = score_plots
  )
}

#' Create Inline Legend for Tail Threshold Methods
#'
#' This function creates a simple one-line legend showing label followed by 
#' dashed line for each method, all on the same horizontal line. The legend
#' adapts to the methods used in the comparison.
#'
#' @param methods Character vector of exactly 3 methods used in the comparison.
#'   Must be from: "Quantile", "MAD", "Qn", "Sn", "Tau", "ZScore".
#' @param colors Character vector of 3 colors corresponding to each method.
#'   Default is c("red", "blue", "darkgreen").
#'
#' @return A ggplot object containing the inline legend
#'
#' @details
#' The function creates method-specific labels:
#' \itemize{
#'   \item MAD: "Median+MAD"
#'   \item Quantile: "Quantile"
#'   \item ZScore: "Z-Score"
#'   \item Qn: "Median+Qn"
#'   \item Sn: "Median+Sn"
#'   \item Tau: "Median+Tau"
#' }
#'
#' @examples
#' # Create legend for default methods
#' legend <- af_create_standalone_legend(c("MAD", "Quantile", "ZScore"))
#' print(legend)
#' 
#' # Create legend for robust methods
#' legend <- af_create_standalone_legend(c("Qn", "Sn", "Tau"), 
#'                                       c("purple", "orange", "brown"))
#' print(legend)
#'
#' @export
af_create_standalone_legend <- function(methods = c("MAD", "Quantile", "ZScore"),
                                        colors = c("red", "blue", "darkgreen")) {
  
  # Input validation
  if (!is.character(methods) || length(methods) != 3) {
    stop("methods must be a character vector of exactly 3 elements")
  }
  
  if (!is.character(colors) || length(colors) != 3) {
    stop("colors must be a character vector of exactly 3 elements")
  }
  
  valid_methods <- c("Quantile", "MAD", "Qn", "Sn", "Tau", "ZScore")
  if (!all(methods %in% valid_methods)) {
    stop(paste("All methods must be one of:", paste(valid_methods, collapse = ", ")))
  }
  
  # Load required libraries
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }
  
  library(ggplot2)
  
  # Get method-specific labels using the standalone function
  labels <- sapply(methods, af_get_method_label)
  
  # Calculate approximate text widths (rough estimate: 0.1 units per character)
  text_widths <- nchar(labels) * 0.1
  
  # Calculate positions for each label-line pair
  n_items <- length(labels)
  
  # Start positions for each item
  x_positions <- numeric(n_items)
  x_positions[1] <- 0.5
  
  for(i in 2:n_items) {
    # Position next item after previous text + line + small gap
    x_positions[i] <- x_positions[i-1] + text_widths[i-1] + 0.8 + 0.3
  }
  
  # Create data for text labels
  text_df <- data.frame(
    x = x_positions,
    y = 1,
    label = labels,
    color = colors,
    stringsAsFactors = FALSE
  )
  
  # Create data for dashed lines (positioned right after each label)
  line_df <- data.frame(
    x = x_positions + text_widths + 0.05,  # Start line just after text
    xend = x_positions + text_widths + 0.65,  # Short line
    y = 1,
    yend = 1,
    color = colors,
    stringsAsFactors = FALSE
  )
  
  # Create the plot
  legend_plot <- ggplot() +
    geom_text(data = text_df,
              aes(x = x, y = y, label = label),
              size = 5, hjust = 0) +
    geom_segment(data = line_df,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 color = line_df$color,
                 linetype = "dashed", 
                 linewidth = 0.8) +
    xlim(0.5, max(line_df$xend) + 0.5) +
    ylim(0.8, 1.2) +
    theme_void()
  
  return(legend_plot)
}

#' Test Suitability of Robust K Factor Across Survey Waves
#'
#' This function evaluates whether a specific K factor for robust scale estimators
#' produces suitable tail definitions across survey waves by checking:
#' 1. Adequate sample sizes in tails
#' 2. Sufficient unique values in tails  
#' 3. Exclusion of moderate values from tails (optional)
#' 4. Consistency across waves
#'
#' @param df Data frame containing the survey data
#' @param extremism_var Character string. Name of the variable containing extremism scores
#' @param wave_var Character string. Name of the variable containing wave identifiers (optional)
#' @param k_factor Numeric value for the robust multiplier to test (default: 2.5)
#' @param method Character string: "MAD", "Qn", "Sn", or "Tau" (default: "Qn")
#' @param min_tail_size Minimum acceptable tail sample size (default: 80)
#' @param min_unique_values Minimum acceptable number of unique values in tail (default: 5, set to NULL to disable)
#' @param moderate_threshold Percentile below which values are considered "moderate" (default: 0.75, set to NULL to disable)
#' @param suggest_optimal Logical. Whether to suggest optimal K factors for unsuitable waves (default: TRUE)
#' @param k_range Numeric vector. Range of K factors to test when suggesting optimal (default: seq(1.5, 4.0, 0.1))
#'
#' @return Data frame with detailed results for each wave
#'
#' @examples
#' # Example with simulated multi-wave data
#' df <- data.frame(
#'   wave = rep(c("Wave1", "Wave2", "Wave3"), each = 1000),
#'   extremism = c(rnorm(1000, 2, 1), rbeta(1000, 2, 5) * 6 + 1, rnorm(1000, 3, 1.5))
#' )
#' 
#' results <- af_test_robust_k_suitability(df, "extremism", "wave", k_factor = 2.5)
#' print(results)
#'
#' @export
af_test_robust_k_suitability <- function(df, 
                                         extremism_var,
                                         wave_var = NULL,
                                         k_factor = 1.5,
                                         method = c("MAD", "Sn", "Qn", "Tau"),
                                         min_tail_size = 80,
                                         min_unique_values = 5,
                                         moderate_threshold = 0.75,
                                         suggest_optimal = TRUE,
                                         k_range = seq(1.0, 2.5, 0.1)) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  if (!extremism_var %in% names(df)) {
    stop(paste("extremism_var '", extremism_var, "' not found in data frame"))
  }
  
  if (!is.null(wave_var) && !wave_var %in% names(df)) {
    stop(paste("wave_var '", wave_var, "' not found in data frame"))
  }
  
  if (!is.numeric(df[[extremism_var]])) {
    stop("extremism_var must contain numeric values")
  }
  
  if (!method %in% c("MAD", "Qn", "Sn", "Tau")) {
    stop("method must be one of: 'MAD', 'Qn', 'Sn', 'Tau'")
  }
  
  # Create wave variable if none provided
  if (is.null(wave_var)) {
    df$temp_wave <- "All_Data"
    wave_var <- "temp_wave"
    remove_temp_wave <- TRUE
  } else {
    remove_temp_wave <- FALSE
  }
  
  # Get unique waves
  unique_waves <- unique(df[[wave_var]])
  unique_waves <- unique_waves[!is.na(unique_waves)]
  
  # Initialize results data frame
  results <- data.frame(
    Wave = character(),
    Total_N = integer(),
    K_Tested = numeric(),
    Threshold = numeric(),
    Tail_Size = integer(),
    Tail_Percent = numeric(),
    Unique_Values = integer(),
    Granularity_Ratio = numeric(),
    Moderates_Included = integer(),
    Moderates_Percent = numeric(),
    Mean_Tail = numeric(),
    Median_Tail = numeric(),
    Score_Range = character(),
    Adequate_Size = logical(),
    Adequate_Unique_Values = logical(),
    Low_Moderate_Inclusion = logical(),
    Overall_Suitable = logical(),
    Suggested_K = numeric(),
    Improvement_Note = character(),
    stringsAsFactors = FALSE
  )
  
  # Process each wave
  for (wave_value in unique_waves) {
    
    # Extract data for this wave
    wave_filter <- df[[wave_var]] == wave_value
    wave_data <- df[wave_filter, ]
    scores <- wave_data[[extremism_var]]
    
    # Remove NA values
    scores_clean <- scores[!is.na(scores)]
    n_total <- length(scores_clean)
    
    # Skip if insufficient data
    if (n_total < 10) {
      results <- rbind(results, data.frame(
        Wave = if (remove_temp_wave) NA else as.character(wave_value),
        Total_N = n_total,
        K_Tested = k_factor,
        Threshold = NA,
        Tail_Size = NA,
        Tail_Percent = NA,
        Unique_Values = NA,
        Granularity_Ratio = NA,
        Moderates_Included = NA,
        Moderates_Percent = NA,
        Mean_Tail = NA,
        Median_Tail = NA,
        Score_Range = "Insufficient data",
        Adequate_Size = FALSE,
        Adequate_Unique_Values = FALSE,
        Low_Moderate_Inclusion = FALSE,
        Overall_Suitable = FALSE,
        Suggested_K = NA,
        Improvement_Note = "Insufficient data for analysis"
      ))
      next
    }
    
    # Calculate threshold and tail metrics
    threshold <- af_tail_threshold(scores_clean, type = method, k_factor = k_factor)
    
    tail_values <- scores_clean[scores_clean > threshold]
    tail_values <- scores_clean[scores_clean > threshold]
    tail_size <- length(tail_values)
    tail_percent <- (tail_size / n_total) * 100
    
    # Unique values and granularity metrics
    unique_tail_values <- length(unique(tail_values))
    granularity_ratio <- if (tail_size > 0) unique_tail_values / tail_size else 0
    
    # Moderate inclusion metrics (only if enabled)
    moderates_in_tail <- 0
    moderates_percent <- 0
    if (!is.null(moderate_threshold)) {
      moderate_cutoff <- quantile(scores_clean, moderate_threshold, na.rm = TRUE)
      moderates_in_tail <- sum(tail_values <= moderate_cutoff)
      moderates_percent <- if (tail_size > 0) (moderates_in_tail / tail_size) * 100 else 0
    }
    
    # Tail descriptive statistics
    mean_tail <- if (tail_size > 0) mean(tail_values) else NA
    median_tail <- if (tail_size > 0) median(tail_values) else NA
    score_range <- if (tail_size > 0) paste0(round(min(tail_values), 1), "-", round(max(tail_values), 1)) else "No tail"
    
    # Suitability assessments
    adequate_size <- tail_size >= min_tail_size
    adequate_unique_values <- if (!is.null(min_unique_values)) unique_tail_values >= min_unique_values else TRUE
    low_moderate_inclusion <- if (!is.null(moderate_threshold)) moderates_percent <= 20 else TRUE
    
    # Overall suitability - only check enabled criteria
    overall_suitable <- adequate_size & adequate_unique_values & low_moderate_inclusion
    
    # Suggest optimal K - completely independent process to find highest suitable K
    suggested_k <- k_factor  # Default fallback
    improvement_note <- "Current K factor is suitable"
    
    if (suggest_optimal) {
      # Independent search for highest K that meets criteria
      # Sort k_range in descending order (highest to lowest)
      k_range_desc <- sort(k_range, decreasing = TRUE)
      found_suitable_k <- FALSE
      
      for (k_test in k_range_desc) {
        
        test_threshold <- af_tail_threshold(scores_clean, type = method, k_factor = k_test)
          
        test_tail_values <- scores_clean[scores_clean > test_threshold]
        test_tail_size <- length(test_tail_values)
        
        if (test_tail_size == 0) next  # Skip if no tail
        
        # Check if this K meets all criteria (independent assessment)
        test_adequate_size <- test_tail_size >= min_tail_size
        test_adequate_unique <- if (!is.null(min_unique_values)) length(unique(test_tail_values)) >= min_unique_values else TRUE
        
        test_low_moderate <- TRUE  # Default if moderate check disabled
        if (!is.null(moderate_threshold)) {
          test_moderate_cutoff <- quantile(scores_clean, moderate_threshold, na.rm = TRUE)
          test_moderates_in_tail <- sum(test_tail_values <= test_moderate_cutoff)
          test_moderate_percent <- (test_moderates_in_tail / test_tail_size) * 100
          test_low_moderate <- test_moderate_percent <= 20
        }
        
        # If this K meets all criteria, it's our answer (highest suitable K)
        if (test_adequate_size & test_adequate_unique & test_low_moderate) {
          suggested_k <- k_test
          found_suitable_k <- TRUE
          
          # Create improvement note based on comparison with input K
          if (suggested_k > k_factor) {
            improvement_note <- paste0("Higher K=", suggested_k, " recommended (more selective)")
          } else if (suggested_k < k_factor) {
            improvement_note <- paste0("Lower K=", suggested_k, " recommended for stability")
          } else {
            improvement_note <- "Current K factor is optimal"
          }
          
          break  # Stop at first (highest) K that works
        }
      }
      
      # If no K in range works, note the issue
      if (!found_suitable_k) {
        suggested_k <- min(k_range)  # Suggest lowest as fallback
        improvement_note <- "No fully suitable K found in range - using lowest available"
      }
    }
    
    # Add results
    results <- rbind(results, data.frame(
      Wave = if (remove_temp_wave) NA else as.character(wave_value),
      Total_N = n_total,
      K_Tested = k_factor,
      Threshold = round(threshold, 3),
      Tail_Size = tail_size,
      Tail_Percent = round(tail_percent, 1),
      Unique_Values = unique_tail_values,
      Granularity_Ratio = round(granularity_ratio, 3),
      Moderates_Included = moderates_in_tail,
      Moderates_Percent = round(moderates_percent, 1),
      Mean_Tail = round(mean_tail, 3),
      Median_Tail = round(median_tail, 3),
      Score_Range = score_range,
      Adequate_Size = adequate_size,
      Adequate_Unique_Values = adequate_unique_values,
      Low_Moderate_Inclusion = low_moderate_inclusion,
      Overall_Suitable = overall_suitable,
      Suggested_K = round(suggested_k, 2),
      Improvement_Note = improvement_note
    ))
  }
  
  # Remove temporary wave column if it was created
  if (remove_temp_wave) {
    results$Wave <- NULL
  }
  
  return(results)
}

#' Create GT Table for K Factor Suitability Results
#'
#' This function creates a formatted GT table from the results of af_test_robust_k_suitability
#' with waves as columns in ordinal format (First, Second, Third, etc.)
#'
#' @param results Data frame returned by af_test_robust_k_suitability
#' @param metric Character string. Which metric to display: "Overall_Suitable", "Tail_Size", 
#'   "Tail_Percent", "Unique_Values", "Suggested_K", or "Threshold" (default: "Overall_Suitable")
#' @param title Character string. Table title (default: based on metric)
#'
#' @return GT table object
#'
#' @examples
#' results <- af_test_robust_k_suitability(df, "extremism", "wave")
#' af_create_k_results_table(results, "Tail_Size")
#' af_create_k_results_table(results, "Overall_Suitable")
#'
#' @export
af_create_k_results_table <- function(results, 
                                      metric = "Overall_Suitable",
                                      title = NULL) {
  
  # Input validation
  if (!is.data.frame(results) || nrow(results) == 0) {
    stop("results must be a non-empty data frame from af_test_robust_k_suitability")
  }
  
  if (!"Wave" %in% names(results)) {
    stop("results must contain Wave column (use wave_var in af_test_robust_k_suitability)")
  }
  
  if (!metric %in% names(results)) {
    stop(paste("metric '", metric, "' not found in results"))
  }
  
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("gt package is required for table formatting")
  }
  
  # Create data for table (single row with waves as columns)
  table_data <- data.frame(
    Metric = metric,
    stringsAsFactors = FALSE
  )
  
  # Add columns for each wave
  for (i in 1:nrow(results)) {
    wave_name <- results$Wave[i]
    if (!is.na(wave_name)) {
      table_data[[wave_name]] <- results[[metric]][i]
    }
  }
  
  # Set default title based on metric
  if (is.null(title)) {
    title <- switch(metric,
                    "Overall_Suitable" = "K Factor Suitability by Wave",
                    "Tail_Size" = "Tail Sample Size by Wave", 
                    "Tail_Percent" = "Tail Percentage by Wave",
                    "Unique_Values" = "Unique Values in Tail by Wave",
                    "Suggested_K" = "Suggested K Factor by Wave",
                    "Threshold" = "Extremism Threshold by Wave",
                    paste(metric, "by Wave")
    )
  }
  
  # Reorder columns to ensure proper wave sequence
  desired_order <- c("Metric", "First", "Second", "Third", "Fourth", "Fifth", "Sixth")
  table_data <- table_data[, intersect(desired_order, names(table_data))]
  wave_cols <- names(table_data)[names(table_data) != "Metric"] 
  
  # Create GT table
  gt_table <- table_data %>%
    gt::gt() %>%
    gt::tab_header(
      title = title,
      subtitle = paste("K Factor:", unique(results$K_Tested)[1])
    ) %>%
    gt::cols_hide(columns = "Metric")
  
  if (metric == "Overall_Suitable") {
    # Convert logical to character for better display
    for (col in wave_cols) {
      if (col %in% names(table_data)) {
        table_data[[col]] <- ifelse(table_data[[col]], "✓ Suitable", "✗ Not Suitable")
      }
    }
    
    gt_table <- gt_table %>%
      gt::data_color(
        columns = wave_cols,
        colors = scales::col_factor(
          palette = c("✓ Suitable" = "#28a745", "✗ Not Suitable" = "#dc3545"),
          domain = c("✓ Suitable", "✗ Not Suitable")
        )
      )
  } else if (metric %in% c("Tail_Percent", "Moderates_Percent")) {
    gt_table <- gt_table %>%
      gt::fmt_number(columns = wave_cols, decimals = 1) %>%
      gt::cols_label(.list = setNames(paste0(wave_cols, " (%)"), wave_cols))
  } else if (metric %in% c("Threshold", "Suggested_K")) {
    gt_table <- gt_table %>%
      gt::fmt_number(columns = wave_cols, decimals = 2)
  } else if (metric %in% c("Tail_Size", "Unique_Values", "Total_N")) {
    gt_table <- gt_table %>%
      gt::fmt_integer(columns = wave_cols)
  } else {
    gt_table <- gt_table %>%
      gt::fmt_number(columns = wave_cols, decimals = 3)
  }
  
  # Add source note
  gt_table <- gt_table %>%
    gt::tab_source_note(
      source_note = paste("Generated by af_test_robust_k_suitability() |", 
                          "Adequate waves:", sum(results$Overall_Suitable, na.rm = TRUE), 
                          "of", nrow(results))
    )
  
  return(gt_table)
}

#' Create Comprehensive GT Table for K Factor Results
#'
#' This function creates a multi-metric formatted GT table showing multiple metrics
#' across waves in a comprehensive view
#'
#' @param results Data frame returned by af_test_robust_k_suitability
#' @param metrics Character vector. Which metrics to include (default: key metrics)
#' @param title Character string. Table title
#'
#' @return GT table object
#'
#' @export
af_create_comprehensive_k_table <- function(results, 
                                            metrics = c("Total_N", "Tail_Size", "Tail_Percent", "Mean_Tail",
                                                        "Unique_Values", "Overall_Suitable", "Suggested_K"),
                                            title = "K Factor Analysis Summary", note = "") {
  
  # Input validation
  if (!is.data.frame(results) || nrow(results) == 0) {
    stop("results must be a non-empty data frame")
  }
  
  if (!"Wave" %in% names(results)) {
    stop("results must contain Wave column")
  }
  
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("gt package is required for table formatting")
  }
  
  # Create comprehensive table data
  table_data <- data.frame(
    Metric = c("Sample Size", "Tail Size", "Tail %", "Tail Mean",  "Unique Values", 
               "Suitable", "Suggested K"),
    stringsAsFactors = FALSE
  )
  
  # Map metrics to display names
  metric_map <- c("Total_N" = "Sample Size", "Tail_Size" = "Tail Size", 
                  "Tail_Percent" = "Tail %", "Mean_Tail" = "Tail Mean",
                  "Unique_Values" = "Unique Values",
                  "Overall_Suitable" = "Suitable", "Suggested_K" = "Suggested K")
  
  # Add columns for each wave
  for (i in 1:nrow(results)) {
    wave_name <- results$Wave[i]
    if (!is.na(wave_name)) {
      table_data[[wave_name]] <- NA
      
      for (j in 1:length(metrics)) {
        metric_row <- which(table_data$Metric == metric_map[metrics[j]])
        if (length(metric_row) > 0) {
          value <- results[[metrics[j]]][i]
          # Convert logical to character for "Overall_Suitable"
          if (metrics[j] == "Overall_Suitable") {
            value <- ifelse(value, "✓ Suitable", "✗ Not Suitable")
          }
          table_data[metric_row, wave_name] <- value
        }
      }
    }
  }
  
  # Reorder columns to ensure proper wave sequence
  desired_order <- c("Metric", "First", "Second", "Third", "Fourth", "Fifth", "Sixth")
  table_data <- table_data[, intersect(desired_order, names(table_data))]
  wave_cols <- names(table_data)[names(table_data) != "Metric"] 
  
  # Create GT table
  gt_table <- table_data %>%
    gt::gt() %>%
    gt::tab_header(
      title = title,
      subtitle = paste("K Factor:", unique(results$K_Tested)[1], 
                       "| Waves:", length(wave_cols))
    ) %>%
    gt::tab_stubhead(label = "Metric") %>%
    gt::fmt_number(
      columns = wave_cols,
      rows = table_data$Metric %in% c("Tail %"),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns = wave_cols, 
      rows = table_data$Metric %in% c("Suggested K"),
      decimals = 2
    ) %>%
    gt::fmt_integer(
      columns = wave_cols,
      rows = table_data$Metric %in% c("Sample Size", "Tail Size", "Unique Values")
    ) %>%
    gt::data_color(
      columns = wave_cols,
      rows = table_data$Metric == "Suitable",
      colors = scales::col_factor(
        palette = c("✓ Suitable" = "#28a745", "✗ Not Suitable" = "#dc3545"),
        domain = c("✓ Suitable", "✗ Not Suitable")
      )
    ) %>%
    gt::tab_source_note(
      source_note = note
    )
  
  return(gt_table)
}

#' Generate Summary Report for K Factor Suitability Test
#'
#' This function creates a summary report from the results of af_test_robust_k_suitability
#'
#' @param results Data frame returned by af_test_robust_k_suitability
#' @param verbose Logical. Whether to print detailed summary (default: TRUE)
#'
#' @return List containing summary statistics and recommendations
#'
#' @export
af_summarize_k_test <- function(results, verbose = TRUE) {
  
  if (!is.data.frame(results) || nrow(results) == 0) {
    if (verbose) cat("No results to summarize\n")
    return(NULL)
  }
  
  # Overall statistics
  total_waves <- nrow(results)
  suitable_waves <- sum(results$Overall_Suitable, na.rm = TRUE)
  pct_suitable <- round(suitable_waves / total_waves * 100, 1)
  
  # Problem breakdown
  size_problems <- sum(!results$Adequate_Size, na.rm = TRUE)
  unique_problems <- sum(!results$Adequate_Unique_Values, na.rm = TRUE)
  moderate_problems <- sum(!results$Low_Moderate_Inclusion, na.rm = TRUE)
  
  # Summary statistics
  avg_tail_size <- round(mean(results$Tail_Size, na.rm = TRUE), 1)
  avg_tail_percent <- round(mean(results$Tail_Percent, na.rm = TRUE), 1)
  avg_unique_values <- round(mean(results$Unique_Values, na.rm = TRUE), 1)
  avg_granularity <- round(mean(results$Granularity_Ratio, na.rm = TRUE), 3)
  avg_moderates <- round(mean(results$Moderates_Percent, na.rm = TRUE), 1)
  
  # K factor suggestions
  suggested_ks <- results$Suggested_K[!is.na(results$Suggested_K)]
  most_common_k <- if (length(suggested_ks) > 0) {
    k_table <- table(suggested_ks)
    as.numeric(names(k_table)[which.max(k_table)])
  } else {
    NA
  }
  
  summary_stats <- list(
    total_waves = total_waves,
    suitable_waves = suitable_waves,
    pct_suitable = pct_suitable,
    size_problems = size_problems,
    unique_problems = unique_problems,
    moderate_problems = moderate_problems,
    avg_tail_size = avg_tail_size,
    avg_tail_percent = avg_tail_percent,
    avg_unique_values = avg_unique_values,
    avg_granularity = avg_granularity,
    avg_moderates = avg_moderates,
    most_common_suggested_k = most_common_k
  )
  
  if (verbose) {
    cat("=" , rep("=", 60), "=\n")
    cat("K FACTOR SUITABILITY SUMMARY\n")
    cat("=" , rep("=", 60), "=\n")
    cat("Tested K Factor:", unique(results$K_Tested)[1], "\n")
    cat("Total Waves:", total_waves, "\n")
    cat("Waves with Suitable K Factor:", suitable_waves, "(", pct_suitable, "%)\n\n")
    
    cat("PROBLEM BREAKDOWN:\n")
    cat("- Inadequate tail size:", size_problems, "waves\n")
    cat("- Insufficient unique values:", unique_problems, "waves\n") 
    cat("- Too many moderates:", moderate_problems, "waves\n\n")
    
    cat("AVERAGE METRICS:\n")
    cat("- Tail size:", avg_tail_size, "cases\n")
    cat("- Tail percentage:", avg_tail_percent, "%\n")
    cat("- Unique values in tail:", avg_unique_values, "\n")
    cat("- Granularity ratio:", avg_granularity, "\n")
    cat("- Moderates in tail:", avg_moderates, "%\n\n")
    
    if (!is.na(most_common_k)) {
      cat("MOST COMMONLY SUGGESTED K FACTOR:", most_common_k, "\n\n")
    }
    
    # Recommendations
    cat("RECOMMENDATIONS:\n")
    if (pct_suitable >= 80) {
      cat("✓ Current K factor is suitable for most waves - proceed with analysis\n")
    } else if (size_problems > total_waves * 0.5) {
      cat("! Consider reducing K factor to increase tail sizes\n")
    } else if (unique_problems > total_waves * 0.5) {
      cat("! Consider reducing K factor to increase unique values in tails\n")
    } else if (moderate_problems > total_waves * 0.5) {
      cat("! Consider increasing K factor to exclude more moderates\n")
    } else {
      cat("! Mixed issues - review individual wave suggestions\n")
    }
    
    if (!is.na(most_common_k) && most_common_k != unique(results$K_Tested)[1]) {
      cat("! Consider using K =", most_common_k, "as suggested by multiple waves\n")
    }
    
    cat("=" , rep("=", 60), "=\n")
  }
  
  return(summary_stats)
}

#' Generate Extremism Tail Size Warning Table
#'
#' This function creates a comprehensive table showing the size of extremism tails
#' for each community-wave-dimension combination, with warnings for small tail sizes
#' that might lead to unreliable extremism level calculations.
#'
#' @param df A data frame containing the survey dataset
#' @param community_var Character string representing the community variable column name
#' @param wave_var Character string representing the wave variable column name (default: "Wave")
#' @param dimensions Character vector of exactly 3 variable names corresponding to 
#'   cognitive, behavioral, and social dimensions (default: c("pe_ideology", "pe_violence", "pe_intolerance"))
#' @param threshold_type Character string specifying the threshold calculation method.
#'   Must be either "MAD", "Sn", "Qn" or "Tau". Default is "MAD".
#' @param k_factor Numeric value specifying the multiplier for threshold calculation.
#'   Must be positive. Default is 1.5.
#' @param tail_size_warning Integer specifying the minimum tail size below which 
#'   values should be highlighted as warnings. Default is 10.
#'
#' @return A gt table object displaying tail sizes by community, wave, and dimension
#'   with conditional formatting for values below the warning threshold.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Calculates population-level thresholds for each wave and dimension
#'   \item Counts how many individuals in each community exceed these thresholds
#'   \item Displays results as "count (percentage%)" format
#'   \item Highlights cells in bold red when tail size < tail_size_warning
#'   \item Organizes results by dimension with clear section headers
#' }
#'
#' The table helps identify potential issues with extremism level calculations
#' due to small sample sizes in the extremism tail, which can lead to unstable
#' percentage estimates.
#'
#' @examples
#' # Generate warning table for standard analysis
#' warning_table <- af_extremism_tail_warnings(
#'   df = survey_data,
#'   community_var = "pe_left_center_right",
#'   threshold_type = "MAD",
#'   k_factor = 1.5,
#'   tail_size_warning = 10
#' )
#'
#' # Display the table
#' warning_table
#'
#' @export
af_extremism_tail_warnings <- function(df,
                                       community_var,
                                       wave_var = "Wave",
                                       dimensions = c("pe_ideology", "pe_violence", "pe_intolerance"),
                                       threshold_type = "MAD",
                                       k_factor = 1.5,
                                       tail_size_warning = 10) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  if (!is.character(community_var) || length(community_var) != 1) {
    stop("community_var must be a single character string")
  }
  
  if (!is.character(wave_var) || length(wave_var) != 1) {
    stop("wave_var must be a single character string")
  }
  
  if (length(dimensions) != 3) {
    stop("dimensions must be a character vector of exactly 3 elements")
  }
  
  required_vars <- c(wave_var, community_var, dimensions)
  missing_vars <- required_vars[!required_vars %in% names(df)]
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables in df:", paste(missing_vars, collapse = ", ")))
  }
  
  if (!is.character(threshold_type) || length(threshold_type) != 1) {
    stop("threshold_type must be a single character string")
  }
  
  if (!threshold_type %in% c("MAD", "Sn", "Qn", "Tau")) {
    stop("threshold_type must be either 'MAD', 'Sn', 'Qn' or 'Tau'")
  }
  
  if (!is.numeric(k_factor) || length(k_factor) != 1 || k_factor <= 0) {
    stop("k_factor must be a positive numeric value")
  }
  
  if (!is.numeric(tail_size_warning) || length(tail_size_warning) != 1 || tail_size_warning < 0) {
    stop("tail_size_warning must be a non-negative numeric value")
  }
  
  # Get unique waves and communities
  waves <- sort(unique(df[[wave_var]]))
  communities <- sort(unique(df[[community_var]][!is.na(df[[community_var]])]))
  
  if (length(waves) == 0) {
    stop("No valid waves found in the data")
  }
  
  if (length(communities) == 0) {
    stop("No valid communities found in the data")
  }
  
  # Dimension labels for display
  dimension_labels <- c("Cognitive", "Behavioral", "Social")
  
  # Initialize results list
  all_results <- list()
  
  # Process each dimension
  for (dim_idx in seq_along(dimensions)) {
    dim_name <- dimensions[dim_idx]
    dim_label <- dimension_labels[dim_idx]
    
    # Calculate population thresholds for each wave
    wave_thresholds <- df %>%
      group_by(!!sym(wave_var)) %>%
      summarise(
        threshold = af_tail_threshold(.data[[dim_name]], threshold_type, k_factor = k_factor),
        .groups = "drop"
      )
    
    # Calculate tail sizes for each community-wave combination
    tail_results <- df %>%
      filter(!is.na(!!sym(community_var)), !is.na(.data[[dim_name]])) %>%
      left_join(wave_thresholds, by = wave_var) %>%
      group_by(!!sym(wave_var), !!sym(community_var)) %>%
      summarise(
        valid_n = n(),
        tail_count = sum(.data[[dim_name]] >= first(threshold), na.rm = TRUE),
        tail_pct = ifelse(valid_n > 0, round(100 * tail_count / valid_n, 1), 0),
        display_value = paste0(tail_count, " (", tail_pct, "%)"),
        .groups = "drop"
      ) %>%
      select(!!sym(wave_var), !!sym(community_var), tail_count, display_value)
    
    # Reshape for table format
    table_data <- tail_results %>%
      select(!!sym(wave_var), !!sym(community_var), display_value) %>%
      pivot_wider(
        names_from = !!sym(wave_var),
        values_from = display_value,
        values_fill = "0 (0.0%)"
      )
    
    # Add dimension indicator
    table_data$dimension <- dim_label
    table_data$dimension_order <- dim_idx
    
    # Store the tail counts for conditional formatting
    tail_counts <- tail_results %>%
      select(!!sym(wave_var), !!sym(community_var), tail_count) %>%
      pivot_wider(
        names_from = !!sym(wave_var),
        values_from = tail_count,
        values_fill = 0
      )
    
    # Add tail counts to table data for reference
    for (wave in waves) {
      if (wave %in% names(tail_counts)) {
        table_data[[paste0(wave, "_count")]] <- tail_counts[[wave]]
      }
    }
    
    all_results[[dim_idx]] <- table_data
  }
  
  # Combine all dimensions
  combined_data <- bind_rows(all_results) %>%
    arrange(dimension_order, !!sym(community_var))
  
  # Create the gt table
  gt_table <- combined_data %>%
    select(-dimension_order, -ends_with("_count")) %>%
    gt(groupname_col = "dimension") %>%
    tab_header(
      title = "Extremism Tail Size Analysis by Community and Wave",
      subtitle = paste0("Method: ", threshold_type, " | K-factor: ", k_factor, " | Warning threshold: ", tail_size_warning, " individuals")
    ) %>%
    cols_label(
      !!sym(community_var) := "Community"
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_fill(color = "lightgray")
      ),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels()
    ) %>%
    tab_footnote(
      footnote = "Values shown as: count (percentage of community). Bold red indicates tail size below warning threshold.",
      locations = cells_title(groups = "subtitle")
    )
  
  # Add conditional formatting for small tail sizes
  # We need to work with the combined data and identify cells that need formatting
  for (wave in waves) {
    # Find all rows where this wave's tail count is below warning threshold
    warning_condition <- combined_data[[paste0(wave, "_count")]] < tail_size_warning & 
      !is.na(combined_data[[paste0(wave, "_count")]])
    
    if (any(warning_condition)) {
      # Get the row indices that need formatting
      warning_rows <- which(warning_condition)
      
      gt_table <- gt_table %>%
        tab_style(
          style = list(
            cell_text(color = "red", weight = "bold")
          ),
          locations = cells_body(
            columns = all_of(wave),
            rows = warning_rows
          )
        )
    }
  }
  
  return(gt_table)
}