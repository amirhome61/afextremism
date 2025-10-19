#' Test for significant differences in group composition across waves
#' 
#' @param data Survey data frame
#' @param wave_var Name of wave variable
#' @param group_var Name of grouping variable (political orientation)
#' @param console_mode Logical, if TRUE returns plain text, if FALSE returns HTML (default: auto-detect)
#' @return List with test results, effect size, tables, and interpretation
af_test_composition_differences <- function(data, wave_var, group_var, console_mode = NULL) {
  # Input validation
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!wave_var %in% names(data)) stop("wave_var not found in data")
  if (!group_var %in% names(data)) stop("group_var not found in data")
  
  # Auto-detect mode if not specified
  if (is.null(console_mode)) {
    console_mode <- !isTRUE(getOption('knitr.in.progress'))
  }
  
  # Load required libraries
  library(dplyr)
  library(tidyr)
  
  # Create contingency table
  comp_table <- table(data[[wave_var]], data[[group_var]])
  
  # Chi-square test
  chi_test <- chisq.test(comp_table)
  
  # Cramer's V for effect size
  cramers_v <- sqrt(chi_test$statistic / (sum(comp_table) * (min(dim(comp_table)) - 1)))
  
  # Proportions by wave
  prop_table <- prop.table(comp_table, margin = 1)
  
  # Effect size interpretation
  effect_size_text <- case_when(
    cramers_v < 0.1 ~ "small",
    cramers_v < 0.3 ~ "medium", 
    cramers_v < 0.5 ~ "large",
    TRUE ~ "very large"
  )
  
  # Significance interpretation
  is_significant <- chi_test$p.value < 0.05
  
  # Create interpretation text
  if (console_mode) {
    interpretation <- sprintf(
      "Chi-square Test Results:\n\nThe Pearson's Chi-squared test examines whether group composition (%s) is independent across waves.\n\nResults:\n- Chi-squared: %.3f (df = %d)\n- P-value: %.6f\n- Cramer's V: %.3f (%s effect size)\n\nInterpretation:\n%s There %s %s differences in group composition across waves. The effect size is %s (Cramer's V = %.3f), indicating that while %sstatistically meaningful, the practical differences in composition are %s.\n\nMethodological Implication:\n%s",
      group_var,
      chi_test$statistic,
      chi_test$parameter,
      chi_test$p.value,
      cramers_v,
      effect_size_text,
      ifelse(is_significant, "✓", "✗"),
      ifelse(is_significant, "are", "are no"),
      ifelse(is_significant, "significant", "significant"),
      effect_size_text,
      cramers_v,
      ifelse(is_significant, "", "not "),
      ifelse(cramers_v < 0.1, "relatively modest", "substantial"),
      ifelse(is_significant, 
             "Equal weighting or population weighting should be considered to control for composition differences in your analyses.",
             "Raw unweighted analyses should be appropriate given the non-significant composition differences.")
    )
    
    result <- list(
      chi_square = chi_test,
      cramers_v = as.numeric(cramers_v),
      proportions = prop_table,
      raw_counts = comp_table,
      interpretation = interpretation
    )
    
  } else {
    # HTML mode for R Markdown
    library(gt)
    
    # Convert tables to data frames for gt processing
    prop_df <- as.data.frame(prop_table)
    names(prop_df) <- c(wave_var, group_var, "Freq")
    
    counts_df <- as.data.frame(comp_table)
    names(counts_df) <- c(wave_var, group_var, "Freq")
    
    # Format proportions table
    prop_gt <- prop_df %>%
      pivot_wider(names_from = !!sym(group_var), values_from = Freq) %>%
      gt() %>%
      fmt_percent(columns = -1, decimals = 1) %>%
      tab_header(title = "Group Proportions by Wave") %>%
      cols_label(!!sym(wave_var) := "Wave")
    
    # Format raw counts table  
    counts_gt <- counts_df %>%
      pivot_wider(names_from = !!sym(group_var), values_from = Freq) %>%
      gt() %>%
      fmt_number(columns = -1, decimals = 0) %>%
      tab_header(title = "Raw Counts by Wave") %>%
      cols_label(!!sym(wave_var) := "Wave")
    
    # Load htmltools for HTML rendering
    library(htmltools)
    
    interpretation <- HTML(sprintf(
      "<div class='chi-square-results'>
      <h3>Chi-square Test Results</h3>
      <p>The Pearson's Chi-squared test examines whether group composition (<em>%s</em>) is independent across waves.</p>
      
      <h4>Results:</h4>
      <ul>
      <li><strong>Chi-squared:</strong> %.3f (df = %d)</li>
      <li><strong>P-value:</strong> %.6f</li>
      <li><strong>Cramer's V:</strong> %.3f (%s effect size)</li>
      </ul>
      
      <h4>Interpretation:</h4>
      <p><span style='color: %s; font-weight: bold;'>%s</span> There %s %s differences in group composition across waves. The effect size is %s (Cramer's V = %.3f), indicating that while %sstatistically meaningful, the practical differences in composition are %s.</p>
      
      <h4>Methodological Implication:</h4>
      <p><em>%s</em></p>
      </div>",
      group_var,
      chi_test$statistic,
      chi_test$parameter, 
      chi_test$p.value,
      cramers_v,
      effect_size_text,
      ifelse(is_significant, "green", "red"),
      ifelse(is_significant, "✓", "✗"),
      ifelse(is_significant, "are", "are no"),
      ifelse(is_significant, "significant", "significant"),
      effect_size_text,
      cramers_v,
      ifelse(is_significant, "", "not "),
      ifelse(cramers_v < 0.1, "relatively modest", "substantial"),
      ifelse(is_significant,
             "Equal weighting or population weighting should be considered to control for composition differences in your analyses.",
             "Raw unweighted analyses should be appropriate given the non-significant composition differences.")
    ))
    
    result <- list(
      chi_square = chi_test,
      cramers_v = as.numeric(cramers_v), 
      proportions_table = prop_gt,
      raw_counts_table = counts_gt,
      interpretation = interpretation
    )
  }
  
  return(result)
}

#' Create weights for equal group sizes within each wave
#' 
#' @param data Survey data frame  
#' @param wave_var Name of wave variable
#' @param group_var Name of grouping variable
#' @return Data frame with added weight variable
af_create_equal_weights <- function(data, wave_var, group_var) {
  # Input validation
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!wave_var %in% names(data)) stop("wave_var not found in data")
  if (!group_var %in% names(data)) stop("group_var not found in data")
  
  library(dplyr)
  
  # FIXED: Properly calculate group sizes and target within each wave
  result <- data %>%
    group_by(!!sym(wave_var), !!sym(group_var)) %>%
    mutate(group_size = n()) %>%
    group_by(!!sym(wave_var)) %>%
    mutate(
      target_n = min(group_size),
      equal_weight = target_n / group_size
    ) %>%
    ungroup()
  
  # Diagnostic check
  if(any(is.na(result$equal_weight))) {
    warning("NAs generated in weights - check for empty groups")
  }
  
  # Diagnostic check for reasonable weight range
  weight_range <- range(result$equal_weight, na.rm = TRUE)
  if(weight_range[2] > 5) {
    warning(paste("Very large weights detected (max =", round(weight_range[2], 2), "). Check for very small groups."))
  }
  
  return(result)
}

#' Create population-representative weights
#' 
#' @param data Survey data frame
#' @param wave_var Name of wave variable  
#' @param group_var Name of grouping variable
#' @param pop_props Named vector of population proportions (must sum to 1)
#' @return Data frame with added weight variable
af_create_pop_weights <- function(data, wave_var, group_var, pop_props) {
  # Input validation
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!wave_var %in% names(data)) stop("wave_var not found in data")
  if (!group_var %in% names(data)) stop("group_var not found in data")
  if (!is.numeric(pop_props)) stop("pop_props must be numeric")
  if (abs(sum(pop_props) - 1) > 0.01) stop("pop_props must sum to 1")
  if (is.null(names(pop_props))) stop("pop_props must be a named vector")
  
  library(dplyr)
  
  # FIXED: Properly calculate sample proportions within each wave
  result <- data %>%
    group_by(!!sym(wave_var)) %>%
    mutate(wave_n = n()) %>%
    group_by(!!sym(wave_var), !!sym(group_var)) %>%
    mutate(
      group_n = n(),
      sample_prop = group_n / wave_n,
      pop_weight = pop_props[as.character(!!sym(group_var))] / sample_prop
    ) %>%
    ungroup()
  
  # Diagnostic check
  if(any(is.na(result$pop_weight))) {
    warning("NAs generated in weights - check that all groups in data have corresponding population proportions")
  }
  
  # Check for reasonable weight range
  weight_range <- range(result$pop_weight, na.rm = TRUE)
  if(weight_range[2] > 10) {
    warning(paste("Very large population weights detected (max =", round(weight_range[2], 2), "). Check population proportions vs sample proportions."))
  }
  
  return(result)
}

#' Summarize weighting results in weighted dataset
#' 
#' @param data Weighted data frame (output from af_create_equal_weights)
#' @param wave_var Name of wave variable
#' @param group_var Name of grouping variable
#' @param weight_var Name of weight variable (default: "equal_weight")
#' @param console_mode Logical, if TRUE returns plain text summary, if FALSE returns gt table (default: auto-detect)
#' @return Summary table showing original vs effective sample sizes and interpretation
af_summarize_weights <- function(data, wave_var, group_var, weight_var = "equal_weight", console_mode = NULL) {
  # Input validation
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!wave_var %in% names(data)) stop("wave_var not found in data")
  if (!group_var %in% names(data)) stop("group_var not found in data")
  if (!weight_var %in% names(data)) stop("weight_var not found in data")
  
  # Auto-detect mode if not specified
  if (is.null(console_mode)) {
    console_mode <- !isTRUE(getOption('knitr.in.progress'))
  }
  
  # Load required libraries
  library(dplyr)
  
  # Create summary table
  weight_summary <- data %>%
    group_by(!!sym(wave_var), !!sym(group_var)) %>%
    summarise(
      n_original = n(),
      effective_n = sum(!!sym(weight_var)),
      weight_mean = mean(!!sym(weight_var)),
      .groups = "drop"
    )
  
  # Calculate overall statistics
  total_original <- sum(weight_summary$n_original)
  total_effective <- sum(weight_summary$effective_n)
  min_weight <- min(weight_summary$weight_mean)
  max_weight <- max(weight_summary$weight_mean)
  
  # Create interpretation
  if (console_mode) {
    interpretation <- sprintf(
      "Weighting Summary:\n\nThe weighting scheme transforms unequal group sizes into equal effective sample sizes within each wave.\n\nOverall Statistics:\n- Total original sample: %d\n- Total effective sample: %.0f\n- Weight range: %.3f to %.3f\n\nInterpretation:\nGroups with weight = 1.000 are the smallest groups (no adjustment needed).\nGroups with weight < 1.000 are down-weighted (were over-represented).\nGroups with weight > 1.000 are up-weighted (were under-represented).\n\nThe 'effective_n' shows what each group contributes to analyses after weighting.",
      total_original,
      total_effective,
      min_weight,
      max_weight
    )
    
    # Print summary table in console
    cat("Weight Summary Table:\n")
    print(weight_summary, n = Inf)
    cat("\n")
    cat(interpretation)
    
    return(invisible(list(
      weight_table = weight_summary,
      interpretation = interpretation
    )))
    
  } else {
    # HTML mode for R Markdown
    library(gt)
    library(htmltools)
    
    # Format summary table
    weight_gt <- weight_summary %>%
      gt() %>%
      fmt_number(columns = c("n_original"), decimals = 0) %>%
      fmt_number(columns = c("effective_n"), decimals = 1) %>%
      fmt_number(columns = c("weight_mean"), decimals = 3) %>%
      tab_header(title = "Weighting Summary by Wave and Group") %>%
      cols_label(
        !!sym(wave_var) := "Wave",
        !!sym(group_var) := "Group",
        n_original = "Original N",
        effective_n = "Effective N",
        weight_mean = "Weight"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightblue"),
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = "weight_mean",
          rows = weight_mean == 1.000
        )
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightcoral")
        ),
        locations = cells_body(
          columns = "weight_mean", 
          rows = weight_mean < 1.000
        )
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightgreen")
        ),
        locations = cells_body(
          columns = "weight_mean",
          rows = weight_mean > 1.000
        )
      )
    
    interpretation <- HTML(sprintf(
      "<div class='weight-summary'>
      <h3>Weighting Summary</h3>
      <p>The weighting scheme transforms unequal group sizes into equal effective sample sizes within each wave.</p>
      
      <h4>Overall Statistics:</h4>
      <ul>
      <li><strong>Total original sample:</strong> %d</li>
      <li><strong>Total effective sample:</strong> %.0f</li>
      <li><strong>Weight range:</strong> %.3f to %.3f</li>
      </ul>
      
      <h4>Color Coding:</h4>
      <ul>
      <li><span style='background-color: lightblue; padding: 2px;'><strong>Blue (weight = 1.000)</strong></span>: Smallest groups (no adjustment needed)</li>
      <li><span style='background-color: lightcoral; padding: 2px;'><strong>Red (weight &lt; 1.000)</strong></span>: Down-weighted groups (were over-represented)</li>
      <li><span style='background-color: lightgreen; padding: 2px;'><strong>Green (weight &gt; 1.000)</strong></span>: Up-weighted groups (were under-represented)</li>
      </ul>
      
      <p><em>The 'Effective N' shows what each group contributes to analyses after weighting.</em></p>
      </div>",
      total_original,
      total_effective,
      min_weight,
      max_weight
    ))
    
    return(list(
      weight_table = weight_gt,
      interpretation = interpretation
    ))
  }
}

#' Analyze extremism differences with equal group weighting
#' 
#' @param data Weighted data frame
#' @param outcome_var Your extremism outcome variable name
#' @param group_var Political group variable  
#' @param wave_var Wave variable
#' @param weight_var Weight variable name
af_analyze_extremism_weighted <- function(data, outcome_var, group_var, wave_var, weight_var = "equal_weight") {
  
  # Input validation
  if (!outcome_var %in% names(data)) stop("outcome_var not found in data")
  if (!weight_var %in% names(data)) stop("weight_var not found in data")
  
  library(dplyr)
  library(broom)
  
  # Weighted means by group and wave
  group_means <- data %>%
    group_by(!!sym(wave_var), !!sym(group_var)) %>%
    summarise(
      weighted_mean = weighted.mean(!!sym(outcome_var), !!sym(weight_var), na.rm = TRUE),
      n_effective = sum(!!sym(weight_var)),
      .groups = "drop"
    )
  
  # Overall group differences (collapsed across waves)
  overall_means <- data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      weighted_mean = weighted.mean(!!sym(outcome_var), !!sym(weight_var), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Weighted ANOVA-style test
  weighted_model <- lm(
    formula = as.formula(paste(outcome_var, "~", group_var, "*", wave_var)),
    data = data,
    weights = data[[weight_var]]
  )
  
  list(
    group_wave_means = group_means,
    overall_group_means = overall_means,
    model_results = tidy(weighted_model),
    model_summary = summary(weighted_model)
  )
}

#' Plot group trajectories across waves
#' 
#' @param results Output from af_analyze_extremism_weighted
#' @param group_var Name of grouping variable (for proper legend labeling)
#' @param wave_var Name of wave variable (for proper x-axis labeling) 
#' @param outcome_name Readable name for outcome variable (for y-axis label)
af_plot_group_trajectories <- function(results, group_var = "pe_left_center_right", 
                                       wave_var = "Wave", outcome_name = "Extremism Score") {
  library(ggplot2)
  
  results$group_wave_means %>%
    ggplot(aes(x = !!sym(wave_var), y = weighted_mean, 
               color = !!sym(group_var), group = !!sym(group_var))) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    labs(
      title = paste(outcome_name, "by Group Across Waves"),
      subtitle = "Equal-weighted analysis",
      y = outcome_name,
      x = "Wave",
      color = "Political Group"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "bottom"
    )
}

#' Compare regression results with and without weighting
#' 
#' @param data Weighted data frame
#' @param outcome_var Name of outcome variable
#' @param group_var Name of grouping variable (e.g., political group)
#' @param wave_var Name of wave variable
#' @param weight_var Name of weight variable to compare against unweighted
#' @param additional_vars Optional vector of additional control variables
#' @param console_mode Logical, if TRUE returns plain text, if FALSE returns gt table (default: auto-detect)
#' @return Comparison of regression coefficients with and without weighting
af_compare_regression_weighting <- function(data, outcome_var, group_var, wave_var, weight_var, 
                                            additional_vars = NULL, console_mode = NULL) {
  # Input validation
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!outcome_var %in% names(data)) stop("outcome_var not found in data")
  if (!group_var %in% names(data)) stop("group_var not found in data") 
  if (!wave_var %in% names(data)) stop("wave_var not found in data")
  if (!weight_var %in% names(data)) stop("weight_var not found in data")
  
  # Auto-detect mode if not specified
  if (is.null(console_mode)) {
    console_mode <- !isTRUE(getOption('knitr.in.progress'))
  }
  
  # Load required libraries
  library(dplyr)
  library(broom)
  
  # Build formula
  if (is.null(additional_vars)) {
    formula_str <- paste(outcome_var, "~", group_var, "*", wave_var)
  } else {
    additional_str <- paste(additional_vars, collapse = " + ")
    formula_str <- paste(outcome_var, "~", group_var, "*", wave_var, "+", additional_str)
  }
  
  model_formula <- as.formula(formula_str)
  
  # Run unweighted regression
  unweighted_model <- lm(model_formula, data = data)
  unweighted_results <- tidy(unweighted_model) %>%
    select(term, estimate_unweighted = estimate, std.error_unweighted = std.error, 
           p.value_unweighted = p.value) %>%
    mutate(
      significant_unweighted = p.value_unweighted < 0.05,
      stars_unweighted = case_when(
        p.value_unweighted < 0.001 ~ "***",
        p.value_unweighted < 0.01 ~ "**", 
        p.value_unweighted < 0.05 ~ "*",
        p.value_unweighted < 0.1 ~ ".",
        TRUE ~ ""
      )
    )
  
  # Run weighted regression
  weighted_model <- lm(model_formula, data = data, weights = data[[weight_var]])
  weighted_results <- tidy(weighted_model) %>%
    select(term, estimate_weighted = estimate, std.error_weighted = std.error,
           p.value_weighted = p.value) %>%
    mutate(
      significant_weighted = p.value_weighted < 0.05,
      stars_weighted = case_when(
        p.value_weighted < 0.001 ~ "***",
        p.value_weighted < 0.01 ~ "**", 
        p.value_weighted < 0.05 ~ "*",
        p.value_weighted < 0.1 ~ ".",
        TRUE ~ ""
      )
    )
  
  # Combine results
  comparison <- unweighted_results %>%
    left_join(weighted_results, by = "term") %>%
    mutate(
      coefficient_difference = estimate_weighted - estimate_unweighted,
      percent_change = ifelse(abs(estimate_unweighted) > 0.001,
                              (coefficient_difference / abs(estimate_unweighted)) * 100,
                              NA),
      significance_change = case_when(
        significant_unweighted & significant_weighted ~ "Both significant",
        significant_unweighted & !significant_weighted ~ "Lost significance", 
        !significant_unweighted & significant_weighted ~ "Gained significance",
        TRUE ~ "Both non-significant"
      )
    )
  
  # Calculate summary statistics
  max_abs_coef_diff <- max(abs(comparison$coefficient_difference), na.rm = TRUE)
  max_abs_percent <- max(abs(comparison$percent_change[is.finite(comparison$percent_change)]), na.rm = TRUE)
  
  # Check for significance changes
  significance_changes <- comparison %>%
    filter(significance_change %in% c("Lost significance", "Gained significance")) %>%
    nrow()
  
  # Determine impact magnitude
  impact_level <- case_when(
    max_abs_coef_diff < 0.05 & significance_changes == 0 ~ "minimal",
    max_abs_coef_diff < 0.10 & significance_changes <= 1 ~ "small", 
    max_abs_coef_diff < 0.20 & significance_changes <= 2 ~ "moderate",
    TRUE ~ "substantial"
  )
  
  # Model fit comparison
  unweighted_r2 <- summary(unweighted_model)$r.squared
  weighted_r2 <- summary(weighted_model)$r.squared
  
  # Create interpretation
  if (console_mode) {
    interpretation <- sprintf(
      "Regression Weighting Impact Analysis:\n\nComparing regression results with and without %s weighting for outcome: %s\n\nModel: %s\n\nSummary Statistics:\n- Maximum absolute coefficient difference: %.4f\n- Maximum absolute percent change: %.1f%%\n- Significance changes: %d coefficients\n- Impact magnitude: %s\n- R-squared unweighted: %.3f\n- R-squared weighted: %.3f\n\nInterpretation:\n%s weighting has a %s impact on regression estimates. %s\n\nMethodological Implication:\n%s",
      weight_var,
      outcome_var,
      formula_str,
      max_abs_coef_diff,
      ifelse(is.finite(max_abs_percent), max_abs_percent, 0),
      significance_changes,
      impact_level,
      unweighted_r2,
      weighted_r2,
      stringr::str_to_title(weight_var),
      impact_level,
      if (impact_level %in% c("minimal", "small")) {
        "Your regression conclusions should be robust to weighting choice."
      } else {
        "Weighting meaningfully changes your regression estimates and/or significance patterns."
      },
      if (impact_level %in% c("minimal", "small")) {
        "Either weighted or unweighted regression would lead to similar substantive conclusions."
      } else {
        "Weighting choice affects your regression conclusions and should be carefully justified based on your research design."
      }
    )
    
    # Print comparison table
    cat("Regression Coefficient Comparison:\n")
    print(comparison %>% select(term, estimate_unweighted, stars_unweighted, estimate_weighted, 
                                stars_weighted, coefficient_difference, percent_change, significance_change), 
          n = Inf)
    cat("\n")
    cat(interpretation)
    
    return(invisible(list(
      coefficient_comparison = comparison,
      unweighted_model = unweighted_model,
      weighted_model = weighted_model,
      interpretation = interpretation
    )))
    
  } else {
    # HTML mode for R Markdown
    library(gt)
    library(htmltools)
    
    # Format comparison table
    comparison_gt <- comparison %>%
      select(term, estimate_unweighted, stars_unweighted, estimate_weighted, 
             stars_weighted, coefficient_difference, percent_change, significance_change) %>%
      gt() %>%
      fmt_number(columns = c("estimate_unweighted", "estimate_weighted", "coefficient_difference"), decimals = 4) %>%
      fmt_number(columns = c("percent_change"), decimals = 1) %>%
      tab_header(title = paste("Regression Weighting Impact:", stringr::str_to_title(weight_var), "vs. Unweighted")) %>%
      cols_label(
        term = "Term",
        estimate_unweighted = "Unweighted Coef",
        stars_unweighted = "Sig",
        estimate_weighted = "Weighted Coef", 
        stars_weighted = "Sig",
        coefficient_difference = "Difference",
        percent_change = "% Change",
        significance_change = "Significance Change"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightcoral")
        ),
        locations = cells_body(
          columns = "significance_change",
          rows = significance_change %in% c("Lost significance", "Gained significance")
        )
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightyellow")
        ),
        locations = cells_body(
          columns = "coefficient_difference",
          rows = abs(coefficient_difference) >= 0.05 & abs(coefficient_difference) < 0.10
        )
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightcoral")
        ),
        locations = cells_body(
          columns = "coefficient_difference", 
          rows = abs(coefficient_difference) >= 0.10
        )
      )
    
    interpretation <- HTML(sprintf(
      "<div class='regression-weighting'>
      <h3>Regression Weighting Impact Analysis</h3>
      <p>Comparing regression results with and without <strong>%s</strong> weighting for outcome: <em>%s</em></p>
      <p><strong>Model:</strong> <code>%s</code></p>
      
      <h4>Summary Statistics:</h4>
      <ul>
      <li><strong>Maximum absolute coefficient difference:</strong> %.4f</li>
      <li><strong>Maximum absolute percent change:</strong> %.1f%%</li>
      <li><strong>Significance changes:</strong> %d coefficients</li>
      <li><strong>Impact magnitude:</strong> %s</li>
      <li><strong>R-squared unweighted:</strong> %.3f</li>
      <li><strong>R-squared weighted:</strong> %.3f</li>
      </ul>
      
      <h4>Color Coding:</h4>
      <ul>
      <li><span style='background-color: lightyellow; padding: 2px;'><strong>Yellow</strong></span>: Moderate coefficient changes (0.05-0.10)</li>
      <li><span style='background-color: lightcoral; padding: 2px;'><strong>Red</strong></span>: Large coefficient changes (≥0.10) or significance changes</li>
      </ul>
      
      <h4>Interpretation:</h4>
      <p><strong>%s</strong> weighting has a <strong>%s</strong> impact on regression estimates. %s</p>
      
      <h4>Methodological Implication:</h4>
      <p><em>%s</em></p>
      </div>",
      weight_var,
      outcome_var, 
      formula_str,
      max_abs_coef_diff,
      ifelse(is.finite(max_abs_percent), max_abs_percent, 0),
      significance_changes,
      impact_level,
      unweighted_r2,
      weighted_r2,
      stringr::str_to_title(weight_var),
      impact_level,
      if (impact_level %in% c("minimal", "small")) {
        "Your regression conclusions should be robust to weighting choice."
      } else {
        "Weighting meaningfully changes your regression estimates and/or significance patterns."
      },
      if (impact_level %in% c("minimal", "small")) {
        "Either weighted or unweighted regression would lead to similar substantive conclusions."
      } else {
        "Weighting choice affects your regression conclusions and should be carefully justified based on your research design."
      }
    ))
    
    return(list(
      coefficient_table = comparison_gt,
      unweighted_model = unweighted_model,
      weighted_model = weighted_model,
      interpretation = interpretation
    ))
  }
}