#' Add additional event information (like Event Types) to Survey Data
#'
#' This function adds event_result and event_type columns to the entire dataframe based on
#' event_table and community variable mapping. The first wave gets "No-Event" and
#' all other waves get values based on the corresponding event in event_table.
#'
#' @param df A dataframe containing all survey data
#' @param wave_var Character string specifying the name of the variable that identifies survey waves
#' @param wave_order Vector mapping wave numbers to wave names (e.g., c("First", "Second", "Third", ...))
#' @param event_table Dataframe containing event additional information 
#' @param community_var Character string specifying the community variable name to match with event_table columns
#'
#' @return Modified dataframe with additional event information columns
#'
#' @export
af_add_event_info <- function(df, wave_var, wave_order, event_table, community_var) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data.frame")
  }
  
  if (!is.character(wave_var) || length(wave_var) != 1) {
    stop("wave_var must be a single character string")
  }
  
  if (!wave_var %in% names(df)) {
    stop(paste("wave_var '", wave_var, "' not found in dataframe columns", sep = ""))
  }
  
  if (!is.character(wave_order) || length(wave_order) == 0) {
    stop("wave_order must be a non-empty character vector")
  }
  
  if (!is.data.frame(event_table)) {
    stop("event_table must be a data.frame")
  }
  
  if (!is.character(community_var) || length(community_var) != 1) {
    stop("community_var must be a single character string")
  }
  
  if (!community_var %in% names(df)) {
    stop(paste("community_var '", community_var, "' not found in dataframe columns", sep = ""))
  }
  
  # Check required columns in event_table
  if (!"waves" %in% names(event_table)) {
    stop("'waves' column not found in event_table")
  }
  
  if (!"type" %in% names(event_table)) {
    warning("'type' column not found in event_table")
  }
  
  # Get unique event types from event_table and create levels
  if ("type" %in% names(event_table)) {
    unique_types <- unique(event_table$type[!is.na(event_table$type)])
    event_type_levels <- c("No-Event", unique_types)
  } else {
    event_type_levels <- c("No-Event")
  }
  
  # Initialize event_type column as a factor
  df$event_type <- factor(NA, levels = event_type_levels)
  
  # Set first wave to "No-Event"
  first_wave <- wave_order[1]
  first_wave_idx <- df[[wave_var]] == first_wave
  df$event_type[first_wave_idx] <- "No-Event"
  
  # Process all other waves
  for (wave_num in 2:length(wave_order)) {
    current_wave <- wave_order[wave_num]
    current_wave_idx <- df[[wave_var]] == current_wave
    
    if (!any(current_wave_idx)) {
      next  # Skip if no data for this wave
    }
    
    # Find the corresponding event in event_table
    # The event should be from previous wave to current wave
    prev_wave_num <- wave_num - 1
    wave_pair_str <- paste(prev_wave_num, wave_num, sep = "-")
    
    # Find matching row in event_table
    table_row_idx <- which(event_table$waves == wave_pair_str)
    
    if (length(table_row_idx) == 0) {
      warning(paste("No matching row found in event_table for waves:", wave_pair_str))
      next
    }
    
    if (length(table_row_idx) > 1) {
      warning(paste("Multiple rows found in event_table for waves:", wave_pair_str, "- using first match"))
      table_row_idx <- table_row_idx[1]
    }
    
    # Set event_type for current wave
    if ("type" %in% names(event_table)) {
      event_type_value <- event_table$type[table_row_idx]
      df$event_type[current_wave_idx] <- event_type_value
    }
  }
  
  return(df)
}

#' Run Regression Models on Wave Pairs
#'
#' This function runs regression models (OLS or Logit) on specified pairs of survey waves
#' from a dataframe and returns a named list of fitted models.
#'
#' @param df A dataframe containing all required variables for the regression
#' @param wave_var Character string specifying the name of the variable that identifies survey waves
#' @param wave_list A named list of wave pairs, where each element contains a vector of two wave identifiers.
#'   Example: list("Pair1" = c("Wave1", "Wave2"), "Pair2" = c("Wave2", "Wave3"))
#' @param formula_str Character string containing the regression formula (e.g., "y ~ x1 + x2")
#' @param regression_type Character string specifying regression type: "OLS" or "Logit"
#' @param set_no_event Logical flag to control whether to override event_occurred of the first wave of each pair to 0. Default is TRUE.
#'
#' @return A named list of fitted regression models, where names correspond to wave pair names
#'
#' @examples
#' \dontrun{
#' # Example usage
#' wave_pairs <- list("First_Second" = c("Wave1", "Wave2"), "Second_Third" = c("Wave2", "Wave3"))
#' models <- af_wave_pair_regression(my_data, "wave", wave_pairs, "outcome ~ predictor", "OLS")
#' }
#'
#' @export
af_wave_pair_regression <- function(df, wave_var, wave_list, formula_str, regression_type, set_no_event = TRUE) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data.frame")
  }
  
  if (!is.character(wave_var) || length(wave_var) != 1) {
    stop("wave_var must be a single character string")
  }
  
  if (!wave_var %in% names(df)) {
    stop(paste("wave_var '", wave_var, "' not found in dataframe columns", sep = ""))
  }
  
  if (!is.list(wave_list) || length(wave_list) == 0) {
    stop("wave_list must be a non-empty list")
  }
  
  if (is.null(names(wave_list)) || any(names(wave_list) == "")) {
    stop("All elements in wave_list must have names")
  }
  
  # Check that each wave_list element has exactly 2 waves
  for (i in seq_along(wave_list)) {
    if (!is.vector(wave_list[[i]]) || length(wave_list[[i]]) != 2) {
      stop(paste("Each element in wave_list must contain exactly 2 wave identifiers. Problem with:", names(wave_list)[i]))
    }
  }
  
  if (!is.character(formula_str) || length(formula_str) != 1) {
    stop("formula_str must be a single character string")
  }
  
  if (!is.character(regression_type) || length(regression_type) != 1) {
    stop("regression_type must be a single character string")
  }
  
  if (!regression_type %in% c("OLS", "Logit")) {
    stop("regression_type must be either 'OLS' or 'Logit'")
  }
  
  if (!is.logical(set_no_event) || length(set_no_event) != 1) {
    stop("set_no_event must be a single logical value (TRUE or FALSE)")
  }
  
  # Convert formula string to formula object
  tryCatch({
    formula_obj <- as.formula(formula_str)
  }, error = function(e) {
    stop(paste("Invalid formula string:", formula_str, ". Error:", e$message))
  })
  
  # Initialize model list
  model_list <- list()
  
  # Process each wave pair
  for (pair_name in names(wave_list)) {
    wave_pair <- wave_list[[pair_name]]
    
    # Check if waves exist in the data
    missing_waves <- wave_pair[!wave_pair %in% df[[wave_var]]]
    if (length(missing_waves) > 0) {
      warning(paste("Waves", paste(missing_waves, collapse = ", "), 
                    "not found in", wave_var, "for pair", pair_name, "- skipping this pair"))
      next
    }
    
    # Subset data for current wave pair
    pair_data <- df[df[[wave_var]] %in% wave_pair, ]
    
    if (nrow(pair_data) == 0) {
      warning(paste("No data found for wave pair", pair_name, "- skipping"))
      next
    }
    
    # Set first wave of pair to "No-Event" if flag is TRUE
    if (set_no_event) {
      # Check if event columns exist (they should if af_add_event_info was applied to df)
      if ("event_occurred" %in% names(pair_data)) {
        first_wave_idx <- pair_data[[wave_var]] == wave_pair[1]
        pair_data$event_occurred[first_wave_idx] <- 0
      }
    }
    
    # Remove rows with NA in any of the formula variables
    # Extract variable names from formula
    formula_vars <- all.vars(formula_obj)
    missing_vars <- formula_vars[!formula_vars %in% names(pair_data)]
    
    if (length(missing_vars) > 0) {
      warning(paste("Variables", paste(missing_vars, collapse = ", "), 
                    "not found in dataframe for pair", pair_name, "- skipping this pair"))
      next
    }
    
    # Remove rows with NA in formula variables
    complete_data <- pair_data[complete.cases(pair_data[formula_vars]), ]
    
    if (nrow(complete_data) == 0) {
      warning(paste("No complete cases found for wave pair", pair_name, "after removing NAs - skipping"))
      next
    }
    
    # Fit regression model
    tryCatch({
      if (regression_type == "OLS") {
        model <- lm(formula_obj, data = complete_data)
      } else if (regression_type == "Logit") {
        model <- glm(formula_obj, data = complete_data, family = binomial(link = "logit"))
      }
      
      # Store model in list with pair name
      model_list[[pair_name]] <- model
      
    }, error = function(e) {
      warning(paste("Failed to fit", regression_type, "model for wave pair", pair_name, ". Error:", e$message))
    })
  }
  
  if (length(model_list) == 0) {
    warning("No models were successfully fitted")
  }
  
  return(model_list)
}

#' Perform Multi-Group Propensity Score Matching
#'
#' This function performs multi-group propensity score matching using nearest neighbor
#' algorithm with equal representation from all treatment groups. For N treatment groups,
#' it creates matched sets with one individual from each group (1:1:1:...:1 ratio).
#' It can be used to match any categorical treatment variable with 2 or more levels 
#' (e.g., political orientation, religiosity, education level, etc.) on specified covariates.
#'
#' @param df Data frame containing the variables
#' @param treatment_var Character, name of treatment variable (must have at least 2 levels)
#' @param covariate_vars Named list of covariate variables to match on (e.g., list(age = "age_col", gender = "gender_col"))
#' @param outcome_var Character, name of outcome variable
#' @param wave_var Character, name of wave identifier variable
#' @param caliper Numeric, maximum propensity score difference for matching (default: 0.1)
#' @param method Character, matching method (default: "nearest")
#' @param seed Integer, random seed for reproducibility (default: 12345)
#' @param balance_metrics Character vector, balance assessment metrics (default: c("smd", "var_ratio"))
#' @param balance_threshold Numeric, acceptable standardized mean difference threshold (default: 0.1)
#' @param var_ratio_threshold Numeric vector, acceptable variance ratio bounds (default: c(0.5, 2.0))
#' @param min_group_size Integer, minimum required observations per treatment group (default: 100)
#' @param required_treatment_levels Character vector, expected treatment levels (default: NULL, auto-detect)
#'
#' @return List containing:
#'   \item{matched_data}{Data frame with matched observations ready for analysis}
#'   \item{matchit_object}{MatchIt object with technical matching details}
#'   \item{balance_plot}{Love plot showing balance before/after matching}
#'   \item{quality_metrics}{Named numeric vector with matching quality indicators}
#'
#' @examples
#' # Political orientation matching (3 groups) - creates 1:1:1 matched triplets
#' covars <- list(religiosity = "relig", gender = "gender", age_group = "age_grp")
#' result <- af_match_groups(df, "political_orient", covars, "extremism", "wave")
#' 
#' # Religiosity matching (6 groups) - creates 1:1:1:1:1:1 matched sextuplets
#' covars <- list(political_orient = "pol_orient", gender = "gender", age_group = "age_grp")
#' result <- af_match_groups(df, "religiosity", covars, "extremism", "wave")
#' 
#' # Education level matching (4 groups) - creates 1:1:1:1 matched quartets
#' covars <- list(income = "income_cat", region = "region", age_group = "age_grp")
#' result <- af_match_groups(df, "education", covars, "outcome", "time_period")
#'
#' @export
af_match_groups <- function(df,
                            treatment_var,
                            covariate_vars,
                            outcome_var,
                            wave_var,
                            caliper = 0.1,
                            method = "nearest",
                            seed = 12345,
                            balance_metrics = c("smd", "var_ratio"),
                            balance_threshold = 0.1,
                            var_ratio_threshold = c(0.5, 2.0),
                            min_group_size = 100,
                            required_treatment_levels = NULL) {
  
  # Load required packages
  if (!requireNamespace("MatchIt", quietly = TRUE)) {
    stop("Error: Package 'MatchIt' is required but not installed")
  }
  if (!requireNamespace("cobalt", quietly = TRUE)) {
    stop("Error: Package 'cobalt' is required but not installed")
  }
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Input parameter validation
  if (!is.data.frame(df)) {
    stop("Error: 'df' must be a data frame")
  }
  
  if (nrow(df) == 0) {
    stop("Error: 'df' is empty (0 rows)")
  }
  
  if (!is.character(treatment_var) || length(treatment_var) != 1) {
    stop("Error: 'treatment_var' must be a single character string")
  }
  
  if (!is.list(covariate_vars) || is.null(names(covariate_vars))) {
    stop("Error: 'covariate_vars' must be a named list")
  }
  
  if (!is.character(outcome_var) || length(outcome_var) != 1) {
    stop("Error: 'outcome_var' must be a single character string")
  }
  
  if (!is.character(wave_var) || length(wave_var) != 1) {
    stop("Error: 'wave_var' must be a single character string")
  }
  
  if (!is.numeric(caliper) || length(caliper) != 1 || caliper <= 0 || caliper > 1) {
    stop("Error: 'caliper' must be a single numeric value between 0 and 1")
  }
  
  if (!is.character(method) || length(method) != 1) {
    stop("Error: 'method' must be a single character string")
  }
  
  valid_methods <- c("nearest", "optimal", "genetic", "full")
  if (!method %in% valid_methods) {
    stop(paste("Error: 'method' must be one of:", paste(valid_methods, collapse = ", ")))
  }
  
  if (!is.numeric(seed) || length(seed) != 1) {
    stop("Error: 'seed' must be a single numeric value")
  }
  
  if (!is.character(balance_metrics)) {
    stop("Error: 'balance_metrics' must be a character vector")
  }
  
  valid_metrics <- c("smd", "var_ratio", "ks_test")
  if (!all(balance_metrics %in% valid_metrics)) {
    stop(paste("Error: 'balance_metrics' must contain only:", paste(valid_metrics, collapse = ", ")))
  }
  
  if (!is.numeric(balance_threshold) || length(balance_threshold) != 1 || balance_threshold <= 0) {
    stop("Error: 'balance_threshold' must be a single positive numeric value")
  }
  
  if (!is.numeric(var_ratio_threshold) || length(var_ratio_threshold) != 2) {
    stop("Error: 'var_ratio_threshold' must be a numeric vector of length 2")
  }
  
  if (var_ratio_threshold[1] >= var_ratio_threshold[2] || var_ratio_threshold[1] <= 0) {
    stop("Error: 'var_ratio_threshold' must be c(lower, upper) with 0 < lower < upper")
  }
  
  if (!is.numeric(min_group_size) || length(min_group_size) != 1 || min_group_size <= 0) {
    stop("Error: 'min_group_size' must be a single positive integer")
  }
  
  if (!is.null(required_treatment_levels) && !is.character(required_treatment_levels)) {
    stop("Error: 'required_treatment_levels' must be NULL or a character vector")
  }
  
  # Check if all variables exist in data frame
  all_vars <- c(treatment_var, unlist(covariate_vars), outcome_var, wave_var)
  missing_vars <- setdiff(all_vars, names(df))
  if (length(missing_vars) > 0) {
    stop(paste("Error: The following variables are not found in df:", paste(missing_vars, collapse = ", ")))
  }
  
  # Data structure validation
  treatment_data <- df[[treatment_var]]
  if (is.factor(treatment_data)) {
    treatment_levels <- levels(treatment_data)
  } else {
    treatment_levels <- unique(treatment_data[!is.na(treatment_data)])
  }
  
  if (length(treatment_levels) < 2) {
    stop(paste("Error: Treatment variable must have at least 2 levels. Found:", length(treatment_levels), "level(s)"))
  }
  
  # Warn for large number of groups (computational complexity)
  if (length(treatment_levels) > 6) {
    warning(paste("Warning: Matching with", length(treatment_levels), "treatment groups may be computationally intensive and require large sample sizes"))
  }
  
  if (!is.null(required_treatment_levels)) {
    if (!all(required_treatment_levels %in% treatment_levels)) {
      missing_levels <- setdiff(required_treatment_levels, treatment_levels)
      stop(paste("Error: Required treatment levels not found:", paste(missing_levels, collapse = ", ")))
    }
    treatment_levels <- required_treatment_levels
  }
  
  # Check missing data in key variables
  missing_treatment <- sum(is.na(df[[treatment_var]]))
  if (missing_treatment > 0) {
    warning(paste("Warning:", missing_treatment, "observations have missing treatment variable and will be excluded"))
  }
  
  missing_outcome <- sum(is.na(df[[outcome_var]]))
  if (missing_outcome > 0) {
    warning(paste("Warning:", missing_outcome, "observations have missing outcome variable"))
  }
  
  for (i in seq_along(covariate_vars)) {
    var_name <- names(covariate_vars)[i]
    col_name <- covariate_vars[[i]]
    missing_count <- sum(is.na(df[[col_name]]))
    if (missing_count > 0) {
      warning(paste("Warning:", missing_count, "observations have missing values in covariate:", var_name))
    }
  }
  
  # Check treatment group sizes
  group_sizes <- table(df[[treatment_var]], useNA = "no")
  small_groups <- names(group_sizes)[group_sizes < min_group_size]
  if (length(small_groups) > 0) {
    stop(paste("Error: The following treatment groups have fewer than", min_group_size, "observations:", paste(small_groups, collapse = ", ")))
  }
  
  # Prepare matching formula
  covariate_names <- unlist(covariate_vars)
  formula_str <- paste(treatment_var, "~", paste(covariate_names, collapse = " + "))
  matching_formula <- as.formula(formula_str)
  
  # Remove observations with missing values in key variables
  complete_vars <- c(treatment_var, covariate_names)
  complete_cases <- complete.cases(df[complete_vars])
  df_clean <- df[complete_cases, ]
  
  excluded_count <- sum(!complete_cases)
  if (excluded_count > 0) {
    message(paste("Excluded", excluded_count, "observations due to missing values in matching variables"))
  }
  
  # Perform matching
  tryCatch({
    match_result <- MatchIt::matchit(
      formula = matching_formula,
      data = df_clean,
      method = method,
      caliper = caliper,
      ratio = 1,
      replace = FALSE
    )
  }, error = function(e) {
    stop(paste("Error in matching process:", e$message))
  })
  
  # Extract matched data
  matched_data <- MatchIt::match.data(match_result)
  
  # Check matching success
  if (nrow(matched_data) == 0) {
    stop("Error: Matching failed - no matched observations found. Consider relaxing the caliper or checking data quality")
  }
  
  matched_group_sizes <- table(matched_data[[treatment_var]])
  min_matched_size <- min(matched_group_sizes)
  
  if (any(matched_group_sizes == 0)) {
    stop("Error: Some treatment groups have no matched observations. Consider relaxing matching criteria")
  }
  
  # Warn if some groups have very small matched samples
  if (min_matched_size < 20) {
    warning(paste("Warning: Smallest matched group has only", min_matched_size, "observations. Results may be unreliable"))
  }
  
  message(paste("Successfully matched", length(treatment_levels), "treatment groups"))
  message(paste("Matched group sizes:", paste(names(matched_group_sizes), "=", matched_group_sizes, collapse = ", ")))
  
  # Calculate balance metrics
  balance_stats <- cobalt::bal.tab(match_result, stats = balance_metrics)
  
  # Extract SMD values after matching
  if ("smd" %in% balance_metrics) {
    smd_values <- abs(balance_stats$Balance$Diff.Adj)
    smd_values <- smd_values[!is.na(smd_values)]
    max_smd <- max(smd_values)
    mean_smd <- mean(smd_values)
    balance_achieved <- ifelse(max_smd <= balance_threshold, 1, 0)
  } else {
    max_smd <- NA
    mean_smd <- NA
    balance_achieved <- NA
  }
  
  # Create Love plot
  balance_plot <- tryCatch({
    plot_title <- paste("Covariate Balance Before and After Matching (", length(treatment_levels), "Groups )")
    cobalt::love.plot(match_result, 
                      threshold = balance_threshold,
                      stats = "mean.diffs",
                      abs = TRUE,
                      title = plot_title)
  }, error = function(e) {
    warning(paste("Could not create balance plot:", e$message))
    NULL
  })
  
  # Calculate quality metrics
  original_size <- nrow(df_clean)
  matched_size <- nrow(matched_data)
  matching_ratio <- matched_size / original_size
  num_groups <- length(treatment_levels)
  min_group_matched <- min(matched_group_sizes)
  
  quality_metrics <- c(
    max_smd = max_smd,
    mean_smd = mean_smd,
    matched_sample_size = matched_size,
    matching_ratio = matching_ratio,
    balance_achieved = balance_achieved,
    num_treatment_groups = num_groups,
    min_group_size = min_group_matched
  )
  
  # Return results
  return(list(
    matched_data = matched_data,
    matchit_object = match_result,
    balance_plot = balance_plot,
    quality_metrics = quality_metrics
  ))
}

#' Add Event Labels to a ggplot
#'
#' This function adds predefined event labels at specific x-axis positions to an existing ggplot object.
#' The labels are positioned at the bottom of the plot and include major events such as "Inland Terror",
#' "Bennet Gov. Fall", "Judicial Reform", "Galant Dismissal", and "October 7".
#'
#' @param p A ggplot object to which event labels will be added
#'
#' @return A modified ggplot object with event labels added at the bottom, expanded plot margins,
#'   and adjusted axis text positioning
#'
#' @details The function adds five event labels at x-positions 1.5, 2.5, 3.5, 4.5, and 5.5.
#'   Labels are displayed in dark red color and positioned below the x-axis. The plot margins
#'   are expanded to accommodate the labels, and clipping is turned off to ensure visibility.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_boxplot()
#' af_event_labels(p)
#' }
#'
#' @export
af_event_labels <- function(p){
  # Input validation
  if (!inherits(p, "ggplot")) {
    stop("Input 'p' must be a ggplot object")
  }
  
  event_labels <- c("Inland Terror", "Bennet Gov. Fall", "Judicial Reform", "Galant Dismissal", "October 7")
  event_positions <- seq(1.5, 5.5, by = 1)
  
  event_df <- data.frame(x = event_positions, label = event_labels)
  
  p + 
    geom_text(data = event_df, aes(x = x, y = -Inf, label = label),
              inherit.aes = FALSE, size = 4, vjust = 0.1, color = "darkred") + 
    coord_cartesian(clip = "off") + 
    theme(plot.margin = margin(20, 20, 40, 20), axis.text.x = element_text(vjust = 1.5))
}


