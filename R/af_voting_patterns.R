#' Compare Categories for Grouping Decision
#'
#' This function analyzes the similarity between categories in a comparison variable
#' to help decide how to group categories together. It compares distributions of 
#' nominal and numerical variables across specified category pairs and provides 
#' both numerical similarity measures and visualizations.
#'
#' @param df A data frame containing survey data with categories to compare
#' @param nominal_vars Character vector of nominal variable names to compare
#'   (e.g., c("vote", "vote2022", "gender"))
#' @param numerical_vars Character vector of numerical variable names to compare
#'   (e.g., c("age"))
#' @param comparison_var Character string specifying the categorical variable name
#'   to analyze (e.g., "religiosity")
#' @param comparison_levels List of character vectors, each containing pairs of 
#'   category levels to compare (e.g., list(c("Traditional", "Secular"), 
#'   c("Traditional", "Religious")))
#' @param wave_var Character string specifying the wave variable name
#'
#' @return A list containing:
#'   \itemize{
#'     \item similarity_scores: Data frame with similarity measures by wave
#'     \item summary_by_wave: Summary of similarity scores by wave
#'     \item overall_similarity: Overall similarity measures across all waves
#'     \item plots: List of ggplot objects for visualization
#'     \item recommendation: Recommended grouping based on similarity scores
#'   }
#'
#' @details
#' The function calculates similarity using:
#' \itemize{
#'   \item Cramér's V for nominal variables (converted to similarity: 1 - Cramér's V)
#'   \item Kolmogorov-Smirnov test p-values for numerical variables
#'   \item Higher similarity scores indicate more similar distributions
#' }
#'
#' @examples
#' \dontrun{
#' # Compare Traditional with Secular and Religious categories
#' result <- af_compare_religiosity_grouping(
#'   df = df,
#'   nominal_vars = c("vote", "vote2022", "gender"),
#'   numerical_vars = c("age"),
#'   comparison_var = "religiosity",
#'   comparison_levels = list(
#'     c("Traditional", "Secular"),
#'     c("Traditional", "Religious")
#'   ),
#'   wave_var = "Wave"
#' )
#' 
#' # View similarity scores (for RMD)
#' knitr::kable(result$summary_by_wave, caption = "Similarity Scores by Wave")
#' result$recommendation
#' 
#' # Display plots
#' result$plots$overall
#' result$plots[["vote"]]
#' result$plots[["gender"]]
#' }
#'
#' @seealso \code{\link[stats]{chisq.test}}, \code{\link[stats]{ks.test}}
#'
#' @export
af_compare_religiosity_grouping <- function(df, 
                                            nominal_vars, 
                                            numerical_vars,
                                            comparison_var,
                                            comparison_levels,
                                            wave_var) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data frame")
  }
  
  if (!comparison_var %in% names(df)) {
    stop(paste("Variable", comparison_var, "not found in data frame"))
  }
  
  if (!wave_var %in% names(df)) {
    stop(paste("Variable", wave_var, "not found in data frame"))
  }
  
  if (!is.list(comparison_levels)) {
    stop("Input 'comparison_levels' must be a list")
  }
  
  if (length(comparison_levels) == 0) {
    stop("Input 'comparison_levels' must contain at least one comparison pair")
  }
  
  # Validate comparison levels
  for (i in seq_along(comparison_levels)) {
    if (!is.character(comparison_levels[[i]]) || length(comparison_levels[[i]]) != 2) {
      stop(paste("Each element in 'comparison_levels' must be a character vector of length 2. Error in element", i))
    }
  }
  
  # Check if all comparison levels exist in the data
  all_levels <- unique(unlist(comparison_levels))
  available_levels <- unique(df[[comparison_var]])
  missing_levels <- setdiff(all_levels, available_levels)
  if (length(missing_levels) > 0) {
    stop(paste("Required comparison levels not found in", comparison_var, ":", paste(missing_levels, collapse = ", ")))
  }
  
  missing_nominal <- setdiff(nominal_vars, names(df))
  if (length(missing_nominal) > 0) {
    stop(paste("Nominal variables not found in data frame:", paste(missing_nominal, collapse = ", ")))
  }
  
  missing_numerical <- setdiff(numerical_vars, names(df))
  if (length(missing_numerical) > 0) {
    stop(paste("Numerical variables not found in data frame:", paste(missing_numerical, collapse = ", ")))
  }
  
  # Load required libraries
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required but not installed")
  }
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Helper function to calculate Cramér's V with warning suppression
  cramers_v <- function(x, y) {
    tbl <- table(x, y)
    
    # Remove empty rows and columns to avoid NaN in chi-square test
    row_sums <- rowSums(tbl)
    col_sums <- colSums(tbl)
    tbl <- tbl[row_sums > 0, col_sums > 0, drop = FALSE]
    
    # Check if we still have a valid table
    if (nrow(tbl) < 2 || ncol(tbl) < 2) {
      return(NA)  # Cannot calculate Cramér's V for tables with < 2 rows or columns
    }
    
    # Check if chi-square test assumptions are met
    expected <- chisq.test(tbl)$expected
    if (any(expected < 5)) {
      # Use simulation for small expected frequencies
      chi2_result <- suppressWarnings(chisq.test(tbl, simulate.p.value = TRUE))
    } else {
      chi2_result <- suppressWarnings(chisq.test(tbl))
    }
    
    chi2 <- chi2_result$statistic
    n <- sum(tbl)
    phi2 <- chi2 / n
    r <- nrow(tbl)
    k <- ncol(tbl)
    sqrt(phi2 / min(k-1, r-1))
  }
  
  # Get unique waves
  waves <- sort(unique(df[[wave_var]]))
  
  # Initialize results storage
  similarity_results <- list()
  plots_list <- list()
  
  # Create comparison names for columns
  comparison_names <- sapply(comparison_levels, function(x) paste(x[1], "vs", x[2]))
  
  # Function to analyze similarity for one wave
  analyze_wave <- function(wave_data, wave_name) {
    # Initialize empty results data frame with proper structure
    results <- data.frame(
      wave = character(),
      variable = character(),
      type = character(),
      stringsAsFactors = FALSE
    )
    
    # Add columns for each comparison
    for (comp_name in comparison_names) {
      results[[paste0(comp_name, "_similarity")]] <- numeric()
    }
    
    # Filter data for all comparison levels
    wave_filtered <- wave_data %>%
      filter(!!sym(comparison_var) %in% all_levels)
    
    if (nrow(wave_filtered) == 0) {
      warning(paste("No data found for wave", wave_name))
      return(results)
    }
    
    # Analyze nominal variables
    for (var in nominal_vars) {
      if (var %in% names(wave_filtered)) {
        # Remove NAs for analysis
        analysis_data <- wave_filtered %>%
          filter(!is.na(!!sym(var))) %>%
          select(all_of(c(comparison_var, var)))
        
        if (nrow(analysis_data) > 0) {
          # Initialize row for this variable
          new_row <- data.frame(
            wave = wave_name,
            variable = var,
            type = "nominal",
            stringsAsFactors = FALSE
          )
          
          # Calculate similarity for each comparison pair
          for (j in seq_along(comparison_levels)) {
            pair <- comparison_levels[[j]]
            comp_name <- comparison_names[j]
            
            # Filter data for this comparison pair
            pair_data <- analysis_data %>%
              filter(!!sym(comparison_var) %in% pair)
            
            # Calculate Cramér's V (higher = more associated, so invert for similarity)
            if (nrow(pair_data) > 0 && length(unique(pair_data[[var]])) > 1) {
              cramers_v_val <- cramers_v(pair_data[[comparison_var]], pair_data[[var]])
              similarity_val <- 1 - cramers_v_val
            } else {
              similarity_val <- NA
            }
            
            new_row[[paste0(comp_name, "_similarity")]] <- similarity_val
          }
          
          results <- rbind(results, new_row)
        }
      }
    }
    
    # Analyze numerical variables
    for (var in numerical_vars) {
      if (var %in% names(wave_filtered)) {
        # Remove NAs for analysis
        analysis_data <- wave_filtered %>%
          filter(!is.na(!!sym(var))) %>%
          select(all_of(c(comparison_var, var)))
        
        if (nrow(analysis_data) > 0) {
          # Initialize row for this variable
          new_row <- data.frame(
            wave = wave_name,
            variable = var,
            type = "numerical",
            stringsAsFactors = FALSE
          )
          
          # Calculate similarity for each comparison pair
          for (j in seq_along(comparison_levels)) {
            pair <- comparison_levels[[j]]
            comp_name <- comparison_names[j]
            
            # Get data for each level in the pair
            level1_data <- analysis_data %>% 
              filter(!!sym(comparison_var) == pair[1]) %>% 
              pull(!!sym(var))
            level2_data <- analysis_data %>% 
              filter(!!sym(comparison_var) == pair[2]) %>% 
              pull(!!sym(var))
            
            # Calculate KS test p-value (higher = more similar)
            if (length(level1_data) > 0 && length(level2_data) > 0) {
              # Suppress warnings about ties - they're expected with survey data
              ks_pval <- suppressWarnings(ks.test(level1_data, level2_data))$p.value
            } else {
              ks_pval <- NA
            }
            
            new_row[[paste0(comp_name, "_similarity")]] <- ks_pval
          }
          
          results <- rbind(results, new_row)
        }
      }
    }
    
    return(results)
  }
  
  # Analyze each wave
  for (wave in waves) {
    wave_data <- df %>% filter(!!sym(wave_var) == wave)
    wave_results <- analyze_wave(wave_data, paste("Wave", wave))
    similarity_results[[paste("Wave", wave)]] <- wave_results
  }
  
  # Analyze overall data
  overall_results <- analyze_wave(df, "Overall")
  similarity_results[["Overall"]] <- overall_results
  
  # Combine all results
  all_results <- do.call(rbind, similarity_results)
  
  # Calculate summary results
  if (nrow(all_results) > 0) {
    # Get similarity columns
    similarity_cols <- paste0(comparison_names, "_similarity")
    
    summary_results <- all_results %>%
      group_by(wave) %>%
      summarise(across(all_of(similarity_cols), ~ mean(.x, na.rm = TRUE)), .groups = 'drop')
    
    # Determine best match for overall data
    overall_summary <- summary_results %>% filter(wave == "Overall")
    
    if (nrow(overall_summary) > 0) {
      # Find which comparison has highest similarity
      similarity_values <- overall_summary[similarity_cols]
      best_comparison_idx <- which.max(similarity_values)
      best_comparison <- comparison_names[best_comparison_idx]
      best_similarity <- similarity_values[[best_comparison_idx]]
      
      recommendation <- paste0(
        "Based on overall similarity scores, the most similar pair is: ",
        best_comparison, 
        " (similarity score: ", round(best_similarity, 3), ")"
      )
    } else {
      recommendation <- "Unable to generate recommendation due to insufficient data"
      overall_summary <- data.frame()
    }
  } else {
    summary_results <- data.frame()
    overall_summary <- data.frame()
    recommendation <- "Unable to generate recommendation due to insufficient data"
  }
  
  # Create visualizations
  plots_list <- list()
  
  # Plot overall similarity scores by wave
  if (nrow(summary_results) > 0) {
    similarity_plot_data <- summary_results %>%
      pivot_longer(cols = all_of(similarity_cols),
                   names_to = "comparison", values_to = "similarity") %>%
      mutate(comparison = gsub("_similarity$", "", comparison))
    
    plots_list$overall <- ggplot(similarity_plot_data, 
                                 aes(x = wave, y = similarity, fill = comparison)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Average Similarity Scores by Wave",
           x = "Wave", y = "Average Similarity Score",
           fill = "Comparison") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Create similarity plots for each nominal variable
  for (var in nominal_vars) {
    var_data <- all_results %>%
      filter(variable == var) %>%
      pivot_longer(cols = all_of(similarity_cols),
                   names_to = "comparison", values_to = "similarity") %>%
      mutate(comparison = gsub("_similarity$", "", comparison))
    
    if (nrow(var_data) > 0) {
      plots_list[[var]] <- ggplot(var_data, aes(x = wave, y = similarity, fill = comparison)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Similarity Scores for", var, "by Wave"),
             x = "Wave", y = "Similarity Score",
             fill = "Comparison") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylim(0, 1)
    }
  }
  
  # Create similarity plots for each numerical variable
  for (var in numerical_vars) {
    var_data <- all_results %>%
      filter(variable == var) %>%
      pivot_longer(cols = all_of(similarity_cols),
                   names_to = "comparison", values_to = "similarity") %>%
      mutate(comparison = gsub("_similarity$", "", comparison))
    
    if (nrow(var_data) > 0) {
      plots_list[[var]] <- ggplot(var_data, aes(x = wave, y = similarity, fill = comparison)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Similarity Scores for", var, "by Wave"),
             x = "Wave", y = "Similarity Score",
             fill = "Comparison") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylim(0, 1)
    }
  }
  
  return(list(
    similarity_scores = all_results,
    summary_by_wave = summary_results,
    overall_similarity = overall_summary,
    plots = plots_list,
    recommendation = recommendation
  ))
}