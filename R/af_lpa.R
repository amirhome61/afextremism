# =============================================================================
# Generic LPA Helper Functions
# All functions are fully parameterized with no hard-coded variable names
# =============================================================================

# Fit multiple LPA models across different numbers of profiles and model types
fit_lpa_models <- function(data, profile_vars, min_profiles, max_profiles, models_to_test) {
  models <- list()
  model_info <- data.frame()
  
  for(n_profiles in min_profiles:max_profiles) {
    for(model_type in models_to_test) {
      tryCatch({
        model <- data %>%
          select(all_of(profile_vars)) %>%
          estimate_profiles(n_profiles, models = model_type, package = "mclust")
        
        # Store model
        model_name <- paste0("profiles_", n_profiles, "_model_", model_type)
        models[[model_name]] <- model
        
        # Extract fit indices
        fit_stats <- get_fit(model)
        model_info <- rbind(model_info, data.frame(
          Model = model_name,
          Profiles = n_profiles,
          ModelType = model_type,
          AIC = fit_stats$AIC,
          BIC = fit_stats$BIC,
          SABIC = fit_stats$SABIC,
          Entropy = fit_stats$Entropy,
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        message(paste("Failed to fit model with", n_profiles, "profiles, type", model_type))
      })
    }
  }
  
  return(list(models = models, model_info = model_info))
}

# Create model comparison table
create_model_comparison_table <- function(model_results) {
  model_results$model_info %>%
    arrange(BIC) %>%
    select(Profiles, ModelType, AIC, BIC, SABIC, Entropy)
}

# Select best model based on BIC
select_best_model <- function(model_results) {
  best_row <- which.min(model_results$model_info$BIC)
  best_model_name <- model_results$model_info$Model[best_row]
  best_model <- model_results$models[[best_model_name]]
  
  return(list(
    model = best_model,
    n_profiles = model_results$model_info$Profiles[best_row],
    model_type = model_results$model_info$ModelType[best_row],
    bic = model_results$model_info$BIC[best_row],
    entropy = model_results$model_info$Entropy[best_row]
  ))
}

# Extract profile means with generic variable names
get_profile_means <- function(model, profile_vars, profile_var_labels = NULL) {
  profile_means <- get_estimates(model) %>%
    filter(Category == "Means") %>%
    select(Class, Parameter, Estimate) %>%
    pivot_wider(names_from = Parameter, values_from = Estimate) %>%
    mutate(Profile = paste0("Profile ", Class)) %>%
    select(Profile, all_of(profile_vars))
  
  return(profile_means)
}

# Get profile assignments
get_profile_assignments <- function(model) {
  assignments <- get_data(model) %>%
    select(Class, CPROB1:last_col()) %>%
    mutate(Profile = paste0("Profile ", Class))
  
  # Check for empty profiles and warn user
  theoretical_classes <- sort(unique(get_estimates(model)$Class))
  actual_classes <- sort(unique(assignments$Class))
  missing_classes <- setdiff(theoretical_classes, actual_classes)
  
  if(length(missing_classes) > 0) {
    warning(paste("Profile(s)", paste(missing_classes, collapse = ", "), 
                  "exist in model estimates but have zero cases assigned.",
                  "Consider using fewer profiles."))
  }
  
  return(assignments)
}

# Create profile size table
create_profile_size_table <- function(profile_assignments) {
  profile_assignments %>%
    count(Profile, name = "N") %>%
    mutate(
      Proportion = N / sum(N),
      Percentage = Proportion * 100
    ) %>%
    arrange(Profile)
}

# Create generic profile plot
create_profile_plot <- function(profile_means, profile_vars, profile_var_labels, 
                                colors, title = "Profile Means") {
  
  # Use labels if provided, otherwise use variable names
  if(is.null(profile_var_labels)) {
    var_labels <- setNames(profile_vars, profile_vars)
  } else {
    var_labels <- setNames(profile_var_labels[profile_vars], profile_vars)
  }
  
  plot_data <- profile_means %>%
    pivot_longer(cols = all_of(profile_vars), 
                 names_to = "Variable", 
                 values_to = "Mean") %>%
    mutate(Variable = factor(Variable, levels = profile_vars))
  
  ggplot(plot_data, aes(x = Variable, y = Mean, color = Profile, group = Profile)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = colors) +
    scale_x_discrete(labels = var_labels) +
    labs(
      title = title,
      subtitle = "Mean values across profiling variables by latent profile",
      x = "Variables",
      y = "Mean Score",
      color = "Profile"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

# Generic profile interpretation function
interpret_profiles <- function(profile_means, profile_vars, profile_var_labels = NULL, 
                               var_direction = NULL) {
  n_profiles <- nrow(profile_means)
  interpretation <- "## Profile Interpretation\n\n"
  
  # Use labels if provided, otherwise use variable names
  if(is.null(profile_var_labels)) {
    var_labels <- setNames(profile_vars, profile_vars)
  } else {
    var_labels <- setNames(profile_var_labels[profile_vars], profile_vars)
  }
  
  # Default all variables to positive direction if not specified
  if(is.null(var_direction)) {
    var_direction <- setNames(rep(TRUE, length(profile_vars)), profile_vars)
  }
  
  for(i in 1:n_profiles) {
    profile_name <- profile_means$Profile[i]
    interpretation <- paste0(interpretation, "### ", profile_name, "\n")
    
    # Get relative standings for each variable
    for(var in profile_vars) {
      var_mean <- profile_means[[var]][i]
      overall_mean <- mean(profile_means[[var]])
      
      # Determine level based on direction
      if(var_direction[[var]]) {
        if(var_mean > overall_mean + 0.2) {
          level <- "high"
        } else if(var_mean < overall_mean - 0.2) {
          level <- "low"
        } else {
          level <- "moderate"
        }
      } else {
        if(var_mean > overall_mean + 0.2) {
          level <- "low"  # Higher score = lower on construct
        } else if(var_mean < overall_mean - 0.2) {
          level <- "high"  # Lower score = higher on construct
        } else {
          level <- "moderate"
        }
      }
      
      interpretation <- paste0(interpretation, "- Shows **", level, "** levels of ", 
                               var_labels[[var]], " (M = ", round(var_mean, 3), ")\n")
    }
    
    # Overall characterization based on average standardized scores
    profile_scores <- as.numeric(profile_means[i, profile_vars])
    overall_scores <- sapply(profile_vars, function(v) mean(profile_means[[v]]))
    
    # Calculate standardized deviation from overall means
    deviations <- profile_scores - overall_scores
    avg_deviation <- mean(abs(deviations))
    
    if(avg_deviation > 0.5) {
      if(mean(deviations) > 0) {
        character <- "**High-scoring profile** - elevated across multiple dimensions"
      } else {
        character <- "**Low-scoring profile** - reduced levels across multiple dimensions"
      }
    } else {
      character <- "**Moderate profile** - average or mixed levels across dimensions"
    }
    
    interpretation <- paste0(interpretation, "\n*Overall characterization*: ", character, "\n\n")
  }
  
  return(interpretation)
}

# Merge profile assignments with binary data
merge_profile_binary_data <- function(analysis_data, profile_assignments, binary_vars) {
  analysis_data$row_id <- 1:nrow(analysis_data)
  profile_assignments$row_id <- 1:nrow(profile_assignments)
  
  merged_data <- merge(analysis_data, profile_assignments, by = "row_id")
  
  # Create dummy variables only for profiles that actually have cases
  actual_classes <- sort(unique(profile_assignments$Class))
  
  for(i in actual_classes) {
    merged_data[[paste0("Profile_", i)]] <- as.numeric(merged_data$Class == i)
  }
  
  return(merged_data)
}

# Calculate correlations between profiles and binary variables (generic)
calculate_profile_correlations <- function(correlation_data, n_profiles, binary_vars, 
                                           binary_var_labels = NULL) {
  profile_vars_dummy <- paste0("Profile_", 1:n_profiles)
  
  # Use labels if provided, otherwise use variable names
  if(is.null(binary_var_labels)) {
    var_labels <- setNames(binary_vars, binary_vars)
  } else {
    var_labels <- setNames(binary_var_labels[binary_vars], binary_vars)
  }
  
  # Calculate point-biserial correlations
  correlation_matrix <- correlation_data %>%
    select(all_of(c(profile_vars_dummy, binary_vars))) %>%
    correlate(method = "pearson") %>%
    select(term, all_of(binary_vars))
  
  # Create detailed correlation table with significance tests
  correlation_table <- data.frame(Variable = var_labels[binary_vars])
  
  for(i in 1:n_profiles) {
    profile_var <- paste0("Profile_", i)
    
    correlations <- numeric(length(binary_vars))
    p_values <- numeric(length(binary_vars))
    
    for(j in 1:length(binary_vars)) {
      test_result <- cor.test(correlation_data[[profile_var]], 
                              correlation_data[[binary_vars[j]]])
      correlations[j] <- test_result$estimate
      p_values[j] <- test_result$p.value
    }
    
    correlation_table[[paste0("Profile_", i, "_r")]] <- correlations
    correlation_table[[paste0("Profile_", i, "_p")]] <- p_values
  }
  
  return(list(
    correlation_matrix = correlation_matrix,
    correlation_table = correlation_table,
    binary_var_mapping = var_labels
  ))
}

# Analyze binary variable proportions by profile (generic)
analyze_binary_proportions <- function(correlation_data, n_profiles, binary_vars, 
                                       binary_var_labels = NULL) {
  
  # Use labels if provided, otherwise use variable names
  if(is.null(binary_var_labels)) {
    var_labels <- setNames(binary_vars, binary_vars)
  } else {
    var_labels <- setNames(binary_var_labels[binary_vars], binary_vars)
  }
  
  proportion_table <- data.frame(Variable = var_labels[binary_vars])
  
  for(i in 1:n_profiles) {
    profile_data <- correlation_data[correlation_data$Class == i, ]
    proportions <- sapply(binary_vars, function(var) mean(profile_data[[var]], na.rm = TRUE))
    proportion_table[[paste0("Profile_", i)]] <- proportions
  }
  
  return(proportion_table)
}

# Create generic correlation heatmap
create_correlation_heatmap <- function(correlation_matrix, colors, n_profiles, 
                                       binary_var_labels = NULL) {
  
  # Prepare data for heatmap
  heatmap_data <- correlation_matrix %>%
    filter(str_detect(term, "Profile_")) %>%
    pivot_longer(cols = -term, names_to = "Binary_Variable", values_to = "Correlation") %>%
    mutate(
      Profile = str_extract(term, "Profile_\\d+"),
      Profile = factor(Profile, levels = paste0("Profile_", 1:n_profiles))
    )
  
  # Apply labels if provided
  if(!is.null(binary_var_labels)) {
    # Create mapping from variable names to labels
    var_mapping <- setNames(names(binary_var_labels), binary_var_labels)
    heatmap_data$Binary_Variable_Label <- binary_var_labels[heatmap_data$Binary_Variable]
    
    heatmap_data <- heatmap_data %>%
      mutate(Binary_Variable_Display = ifelse(is.na(Binary_Variable_Label), 
                                              Binary_Variable, Binary_Variable_Label))
  } else {
    heatmap_data$Binary_Variable_Display <- heatmap_data$Binary_Variable
  }
  
  ggplot(heatmap_data, aes(x = Binary_Variable_Display, y = Profile, fill = Correlation)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                         midpoint = 0, limits = c(-1, 1),
                         name = "Correlation") +
    labs(
      title = "Profile-Binary Variable Correlations Heatmap",
      subtitle = "Point-biserial correlations between profile membership and binary indicators",
      x = "Binary Variables",
      y = "Profiles"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    ) +
    geom_text(aes(label = round(Correlation, 2)), size = 3, color = "black")
}

# Generic correlation interpretation function
interpret_correlations <- function(correlation_results, alpha_level, n_profiles, 
                                   binary_var_labels = NULL) {
  interpretation <- "## Correlation Interpretation\n\n"
  
  # Use labels if provided
  var_labels <- if(is.null(binary_var_labels)) {
    setNames(correlation_results$correlation_table$Variable, 
             correlation_results$correlation_table$Variable)
  } else {
    binary_var_labels
  }
  
  # Find significant correlations
  correlation_table <- correlation_results$correlation_table
  
  for(i in 1:n_profiles) {
    profile_name <- paste0("Profile ", i)
    interpretation <- paste0(interpretation, "### ", profile_name, " Associations\n")
    
    r_col <- paste0("Profile_", i, "_r")
    p_col <- paste0("Profile_", i, "_p")
    
    # Get significant variables using the actual variable names from the table
    if(is.null(binary_var_labels)) {
      significant_vars <- correlation_table$Variable[correlation_table[[p_col]] < alpha_level]
      var_display_names <- significant_vars
    } else {
      # Map back from labels to original variable names for indexing
      significant_indices <- which(correlation_table[[p_col]] < alpha_level)
      var_display_names <- correlation_table$Variable[significant_indices]
    }
    
    if(length(var_display_names) > 0) {
      for(j in seq_along(var_display_names)) {
        if(is.null(binary_var_labels)) {
          row_idx <- which(correlation_table$Variable == var_display_names[j])
        } else {
          row_idx <- significant_indices[j]
        }
        
        correlation <- correlation_table[[r_col]][row_idx]
        p_value <- correlation_table[[p_col]][row_idx]
        
        direction <- ifelse(correlation > 0, "positively", "negatively")
        strength <- case_when(
          abs(correlation) >= 0.5 ~ "strongly",
          abs(correlation) >= 0.3 ~ "moderately", 
          abs(correlation) >= 0.1 ~ "weakly",
          TRUE ~ "very weakly"
        )
        
        interpretation <- paste0(interpretation, "- **", strength, " ", direction, 
                                 "** associated with *", var_display_names[j], 
                                 "* (r = ", round(correlation, 3), ", p = ", 
                                 round(p_value, 3), ")\n")
      }
    } else {
      interpretation <- paste0(interpretation, "- No significant associations found\n")
    }
    
    interpretation <- paste0(interpretation, "\n")
  }
  
  return(interpretation)
}

# Create generic final summary
create_final_summary <- function(best_model_info, profile_means, correlation_results, 
                                 alpha_level, profile_var_labels = NULL, 
                                 binary_var_labels = NULL) {
  n_profiles <- best_model_info$n_profiles
  entropy <- best_model_info$entropy
  
  # Determine what variables were analyzed
  profile_var_text <- if(is.null(profile_var_labels)) {
    paste(names(profile_means)[-1], collapse = ", ")  # Exclude "Profile" column
  } else {
    paste(profile_var_labels, collapse = ", ")
  }
  
  binary_var_text <- if(is.null(binary_var_labels)) {
    paste(correlation_results$correlation_table$Variable, collapse = ", ")
  } else {
    paste(binary_var_labels, collapse = ", ")
  }
  
  summary_text <- paste0(
    "## Key Findings\n\n",
    "This LPA analysis identified **", n_profiles, " distinct profiles** based on ",
    "the following variables: *", profile_var_text, "* ",
    "with good classification quality (Entropy = ", round(entropy, 3), "). ",
    "The profiles show meaningful differentiation across the analyzed dimensions.\n\n"
  )
  
  # Count significant correlations
  correlation_table <- correlation_results$correlation_table
  total_tests <- (n_profiles * nrow(correlation_table))
  significant_count <- 0
  
  for(i in 1:n_profiles) {
    p_col <- paste0("Profile_", i, "_p")
    significant_count <- significant_count + sum(correlation_table[[p_col]] < alpha_level)
  }
  
  summary_text <- paste0(
    summary_text,
    "### Correlation Analysis Results\n",
    "- **Binary variables analyzed**: *", binary_var_text, "*\n",
    "- **Total correlation tests**: ", total_tests, "\n",
    "- **Significant associations** (p < ", alpha_level, "): ", significant_count, "\n",
    "- **Proportion significant**: ", round(significant_count/total_tests * 100, 1), "%\n\n"
  )
  
  summary_text <- paste0(
    summary_text,
    "### Methodological Notes\n",
    "- Model selection based on Bayesian Information Criterion (BIC)\n",
    "- Point-biserial correlations used for profile-binary variable associations\n",
    "- All analyses conducted on cases with complete data on profile variables\n",
    "- Results should be interpreted within the context of the specific sample and measures used"
  )
  
  return(summary_text)
}

# Generic data validation function
validate_data <- function(data, profile_vars, binary_vars) {
  issues <- character(0)
  
  # Check variable presence
  missing_vars <- setdiff(c(profile_vars, binary_vars), names(data))
  if(length(missing_vars) > 0) {
    issues <- c(issues, paste("Missing variables:", paste(missing_vars, collapse = ", ")))
  }
  
  # Check data types for profile variables
  for(var in profile_vars) {
    if(var %in% names(data) && !is.numeric(data[[var]])) {
      issues <- c(issues, paste("Profile variable", var, "is not numeric"))
    }
  }
  
  # Check binary variables
  for(var in binary_vars) {
    if(var %in% names(data)) {
      unique_vals <- unique(data[[var]][!is.na(data[[var]])])
      if(!all(unique_vals %in% c(0, 1))) {
        issues <- c(issues, paste("Binary variable", var, "contains values other than 0 and 1"))
      }
    }
  }
  
  # Check for excessive missing data
  for(var in profile_vars) {
    if(var %in% names(data)) {
      missing_prop <- mean(is.na(data[[var]]))
      if(missing_prop > 0.2) {
        issues <- c(issues, paste("Variable", var, "has", round(missing_prop*100, 1), "% missing data"))
      }
    }
  }
  
  if(length(issues) > 0) {
    warning("Data validation issues found:\n", paste(issues, collapse = "\n"))
  }
  
  return(length(issues) == 0)
}

# Additional utility function for quick LPA setup
create_lpa_config <- function(profile_variables, binary_variables, 
                              profile_labels = NULL, binary_labels = NULL,
                              data_path = NULL, min_prof = 2, max_prof = 6) {
  
  config <- list(
    profile_vars = profile_variables,
    binary_vars = binary_variables,
    profile_var_labels = profile_labels %||% setNames(profile_variables, profile_variables),
    binary_var_labels = binary_labels %||% setNames(binary_variables, binary_variables),
    data_file_path = data_path,
    min_profiles = min_prof,
    max_profiles = max_prof,
    models_to_test = c(1, 2, 3, 6),
    var_direction = setNames(rep(TRUE, length(profile_variables)), profile_variables),
    alpha_level = 0.05
  )
  
  return(config)
}

# Null-coalescing operator helper
`%||%` <- function(x, y) if(is.null(x)) y else x