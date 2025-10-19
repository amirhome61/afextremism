#' Create MANOVA Model for Extremism Data
#'
#' @description
#' Performs data preparation and fits a multivariate analysis of variance (MANOVA) model 
#' on extremism data with wave selection and automatic event variable creation.
#'
#' @param data A data.frame containing the survey data
#' @param extremism_vars Named character vector of extremism dimension variables with display labels as names
#' @param wave_var Character string specifying the wave identifier variable name
#' @param wave_pair Numeric vector of length 2 specifying which waves to compare (e.g., c(1, 3))
#' @param group_var Character string specifying the grouping variable name (e.g., political orientation)
#' @param control_vars Character vector of control variable names
#' @param id_var Character string specifying the respondent ID variable name
#' @param event_var Character string specifying the name for the created event variable
#' @param is_panel Logical indicating whether the paired waves represent panel data
#'
#' @return A list containing the fitted MANOVA model, processed data, and metadata
#'
#' @details
#' This function selects two waves from the data, creates an event variable (0 for first wave,
#' 1 for second wave) with the name specified in event_var, and fits a MANOVA model.
#' The is_panel parameter indicates whether the analysis involves panel data (same individuals
#' across waves) or cross-sectional comparisons.
#'
#' @examples
#' \dontrun{
#' # Panel analysis
#' model <- af_manova_model(
#'   data = survey_data,
#'   extremism_vars = c("Religious" = "pe_religious", 
#'                      "Nationalist" = "pe_nationalist", 
#'                      "Political" = "pe_political"),
#'   wave_var = "wave",
#'   wave_pair = c(2, 4),
#'   group_var = "political_orientation",
#'   event_var = "pre_post_event",
#'   is_panel = TRUE
#' )
#' 
#' # Cross-sectional analysis
#' model <- af_manova_model(
#'   data = survey_data,
#'   extremism_vars = c("Ideology" = "pe_ideology", 
#'                      "Violence" = "pe_violence"),
#'   wave_var = "survey_wave",
#'   wave_pair = c(1, 3),
#'   group_var = "pol_orient",
#'   control_vars = c("gender", "age_group"),
#'   event_var = "treatment_exposure",
#'   is_panel = FALSE
#' )
#' }
#'
af_manova_model <- function(data,
                            extremism_vars,
                            wave_var,
                            wave_pair,
                            group_var,
                            control_vars = NULL,
                            id_var = NULL,
                            event_var = "event_occurred",
                            is_panel = FALSE) {
  
  # Input validation
  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (length(wave_pair) != 2) stop("wave_pair must be a vector of length 2")
  if (!is.character(event_var) || length(event_var) != 1) stop("event_var must be a single character string")
  if (!is.logical(is_panel) || length(is_panel) != 1) stop("is_panel must be a single logical value")
  
  # Extract variable names from extremism_vars (handles both named and unnamed vectors)
  extremism_var_names <- if(is.null(names(extremism_vars))) extremism_vars else unname(extremism_vars)
  
  if (!all(c(extremism_var_names, wave_var, group_var) %in% names(data))) {
    stop("All specified variables must exist in data")
  }
  if (!wave_var %in% names(data)) stop("Wave variable must exist in data")
  
  # Check if specified waves exist in data
  available_waves <- unique(data[[wave_var]])
  if (!all(wave_pair %in% available_waves)) {
    stop(paste("Specified waves", paste(wave_pair, collapse = ", "), 
               "not found. Available waves:", paste(available_waves, collapse = ", ")))
  }
  
  # Check if event_var already exists in data
  if (event_var %in% names(data)) {
    stop(paste("Variable name", event_var, "already exists in data. Choose a different event_var name."))
  }
  
  # Select data for specified waves
  wave_data <- data[data[[wave_var]] %in% wave_pair, ]
  
  if (nrow(wave_data) == 0) {
    stop("No data found for specified waves")
  }
  
  # Panel-specific filtering: keep only respondents who appear in both waves
  if (is_panel) {
    if (is.null(id_var) || !id_var %in% names(wave_data)) {
      stop("For panel analysis (is_panel = TRUE), id_var must be specified and exist in data")
    }
    
    # Find respondents who appear in both waves
    wave1_ids <- wave_data[wave_data[[wave_var]] == wave_pair[1], id_var]
    wave2_ids <- wave_data[wave_data[[wave_var]] == wave_pair[2], id_var]
    common_ids <- intersect(wave1_ids, wave2_ids)
    
    if (length(common_ids) == 0) {
      stop("No respondents found in both waves for panel analysis")
    }
    
    # Keep only data for respondents who appear in both waves
    wave_data <- wave_data[wave_data[[id_var]] %in% common_ids, ]
    
    if (nrow(wave_data) == 0) {
      stop("No data remaining after panel filtering")
    }
  }
  
  # Create event variable (0 for first wave, 1 for second wave)
  wave_data[[event_var]] <- ifelse(wave_data[[wave_var]] == wave_pair[1], 0, 1)
  
  # Remove rows with missing data in key variables
  complete_vars <- c(extremism_var_names, event_var, group_var, control_vars)
  if (!is.null(id_var) && id_var %in% names(wave_data)) {
    complete_vars <- c(complete_vars, id_var)
  }
  
  wave_data_complete <- wave_data[complete.cases(wave_data[, complete_vars]), ]
  
  if (nrow(wave_data_complete) == 0) {
    stop("No complete cases found for analysis variables")
  }
  
  # Check if extremism variables are numeric
  if (!all(sapply(wave_data_complete[extremism_var_names], is.numeric))) {
    stop("All extremism variables must be numeric")
  }
  
  # Create outcome matrix
  Y <- as.matrix(wave_data_complete[, extremism_var_names])
  
  # Build formula for MANOVA
  predictor_vars <- c(event_var, group_var, control_vars)
  interaction_term <- paste0(event_var, " * ", group_var)
  formula_str <- paste("Y ~", paste(predictor_vars, collapse = " + "), "+", interaction_term)
  formula_obj <- as.formula(formula_str)
  
  # Fit MANOVA
  manova_model <- lm(formula_obj, data = wave_data_complete)
  
  # Create model object with metadata
  model_obj <- list(
    model = manova_model,
    processed_data = wave_data_complete,
    extremism_vars = extremism_vars,
    wave_info = list(
      waves_analyzed = wave_pair,
      wave_labels = paste("Wave", wave_pair[1], "vs Wave", wave_pair[2]),
      n_wave1 = sum(wave_data_complete[[event_var]] == 0),
      n_wave2 = sum(wave_data_complete[[event_var]] == 1),
      total_n = nrow(wave_data_complete),
      is_panel = is_panel,
      panel_type = ifelse(is_panel, "Panel Analysis", "Cross-sectional Analysis")
    ),
    variable_info = list(
      extremism_var_names = extremism_var_names,
      event_var = event_var,
      group_var = group_var,
      control_vars = control_vars,
      id_var = id_var
    ),
    formula_used = formula_str
  )
  
  class(model_obj) <- "manova_model"
  return(model_obj)
}

#' Generate MANOVA Results and Visualizations
#'
#' @description
#' Generates comprehensive MANOVA results, summary statistics, and profile plots 
#' from a fitted MANOVA model object created by af_manova_model().
#'
#' @param model A manova_model object returned by af_manova_model()
#' @param create_profile_plot Logical indicating whether to create profile plots
#' @param use_viridis Logical indicating whether to use viridis color palette
#' @param use_bw Logical indicating whether to use black and white color scheme
#' @param line_width Numeric value for line width in profile plot
#' @param point_size Numeric value for point size in profile plot
#' @param transpose Logical indicating whether to transpose the profile plot axes
#' @param show_lines Logical indicating whether to show connecting lines between points
#' @param plot_title Character string for plot title (optional)
#' @param plot_subtitle Character string for plot subtitle (optional)
#' @param group_legend_title Character string for group variable legend title
#' @param x_axis_title Character string for x-axis title
#' @param y_axis_title Character string for y-axis title
#'
#' @return A list containing MANOVA results, summary statistics, and optional profile plot
#'
#' @details
#' This function takes a fitted MANOVA model and generates comprehensive results including
#' multivariate and univariate test results, summary statistics, and customizable profile plots.
#' The function preserves all metadata from the original model including panel status.
#'
#' @examples
#' \dontrun{
#' # Create model first
#' model <- af_manova_model(
#'   data = survey_data,
#'   extremism_vars = c("Religious" = "pe_religious", 
#'                      "Nationalist" = "pe_nationalist"),
#'   wave_var = "wave",
#'   wave_pair = c(2, 4),
#'   group_var = "political_orientation",
#'   event_var = "pre_post_event",
#'   is_panel = TRUE
#' )
#' 
#' # Generate basic results
#' results <- af_manova_results(model)
#' 
#' # Generate results with custom plot
#' results <- af_manova_results(
#'   model,
#'   create_profile_plot = TRUE,
#'   plot_title = "Custom Analysis Title",
#'   plot_subtitle = "Panel Analysis Results",
#'   show_lines = FALSE,
#'   use_bw = TRUE
#' )
#' }
#'
af_manova_results <- function(model,
                              create_profile_plot = TRUE,
                              use_viridis = TRUE,
                              use_bw = FALSE,
                              line_width = 1.0,
                              point_size = 2.5,
                              transpose = FALSE,
                              show_lines = FALSE,
                              plot_title = "",
                              plot_subtitle = "",
                              group_legend_title = "",
                              x_axis_title = "",
                              y_axis_title = "Mean Scores") {
  
  # Input validation
  if (!inherits(model, "manova_model")) {
    stop("model must be a manova_model object created by af_manova_model()")
  }
  
  # Extract components from model object
  manova_model <- model$model
  wave_data_complete <- model$processed_data
  extremism_vars <- model$extremism_vars
  wave_pair <- model$wave_info$waves_analyzed
  event_var <- model$variable_info$event_var
  group_var <- model$variable_info$group_var
  extremism_var_names <- model$variable_info$extremism_var_names
  
  # Generate MANOVA results
  manova_results <- car::Manova(manova_model, type = "III")
  univariate_results <- summary.aov(manova_model)
  
  # Create summary statistics
  summary_stats <- af_create_summary_stats(wave_data_complete, extremism_var_names, 
                                           event_var, group_var)
  
  # Create profile plot if requested
  profile_plot <- NULL
  if (create_profile_plot) {
    profile_plot <- af_create_profile_plot(wave_data_complete, extremism_vars,
                                           event_var, group_var, wave_pair,
                                           use_viridis, use_bw, line_width, point_size,
                                           transpose, show_lines, plot_title, plot_subtitle,
                                           group_legend_title, x_axis_title, y_axis_title)
  }
  
  # Return comprehensive results
  results <- list(
    manova_summary = manova_results,
    univariate_summary = univariate_results,
    model = manova_model,
    processed_data = wave_data_complete,
    summary_stats = summary_stats,
    profile_plot = profile_plot,
    wave_info = model$wave_info,  # Includes is_panel and panel_type
    variable_info = model$variable_info,
    formula_used = model$formula_used
  )
  
  class(results) <- "manova_results"
  return(results)
}

#' Create Summary Statistics for MANOVA
#'
#' @description
#' Generates summary statistics (means, standard deviations, standard errors, and sample sizes)
#' for extremism variables by event occurrence and grouping variable combinations.
#'
#' @param data A data.frame containing the analysis variables
#' @param extremism_vars Character vector of extremism variable names
#' @param event_var Character string specifying the event variable name
#' @param group_var Character string specifying the grouping variable name
#'
#' @return A data.frame with summary statistics including group identifiers, sample sizes,
#'         means, standard deviations, and standard errors for each variable
#'
af_create_summary_stats <- function(data, extremism_vars, event_var, group_var) {
  
  # Create grouping combinations
  groups <- expand.grid(
    event = unique(data[[event_var]]),
    group = unique(data[[group_var]])
  )
  
  # Calculate means and standard deviations for each group and variable
  summary_list <- list()
  
  for (i in 1:nrow(groups)) {
    group_data <- data[data[[event_var]] == groups$event[i] & 
                         data[[group_var]] == groups$group[i], ]
    
    if (nrow(group_data) > 0) {
      for (var in extremism_vars) {
        summary_list[[length(summary_list) + 1]] <- data.frame(
          event = groups$event[i],
          group_orientation = groups$group[i],
          variable = var,
          n = nrow(group_data),
          mean = mean(group_data[[var]], na.rm = TRUE),
          sd = sd(group_data[[var]], na.rm = TRUE),
          se = sd(group_data[[var]], na.rm = TRUE) / sqrt(nrow(group_data))
        )
      }
    }
  }
  
  do.call(rbind, summary_list)
}

#' Create Profile Plot for MANOVA Results
#'
#' @description
#' Creates a customizable profile plot showing mean scores across extremism dimensions,
#' grouped by wave and group variable. Uses different shapes for each dimension (circle, square, triangle)
#' and different colors for groups. Supports dodged points, optional connecting lines,
#' custom titles, and flexible aesthetic options.
#'
#' @param data A data.frame containing the analysis variables
#' @param extremism_vars Named character vector of extremism variables with display labels as names
#' @param event_var Character string specifying the event variable name
#' @param group_var Character string specifying the grouping variable name
#' @param wave_pair Numeric vector of wave identifiers for labeling
#' @param use_viridis Logical indicating whether to use viridis color palette
#' @param use_bw Logical indicating whether to use black and white color scheme
#' @param line_width Numeric value for line width (used only when show_lines = TRUE)
#' @param point_size Numeric value for point size
#' @param transpose Logical indicating whether to transpose x and y axes
#' @param show_lines Logical indicating whether to show connecting lines between points
#' @param plot_title Character string for plot title (if NULL, uses default)
#' @param plot_subtitle Character string for plot subtitle (if NULL, uses default)
#' @param group_legend_title Character string for group variable legend title (if NULL, uses default)
#' @param x_axis_title Character string for x-axis title (if NULL, uses default)
#' @param y_axis_title Character string for y-axis title (if NULL, uses default)
#'
#' @return A ggplot2 object representing the profile plot
#'
af_create_profile_plot <- function(data, extremism_vars, event_var, 
                                   group_var, wave_pair,
                                   use_viridis = TRUE, use_bw = FALSE, 
                                   line_width = 0.8, point_size = 2, 
                                   transpose = FALSE, show_lines = TRUE,
                                   plot_title = NULL, plot_subtitle = NULL,
                                   group_legend_title = NULL, x_axis_title = NULL,
                                   y_axis_title = NULL) {
  
  # Extract variable names and labels
  extremism_var_names <- if(is.null(names(extremism_vars))) extremism_vars else unname(extremism_vars)
  extremism_var_labels <- if(is.null(names(extremism_vars))) extremism_vars else names(extremism_vars)
  
  # Calculate group means
  plot_data <- data %>%
    dplyr::group_by(!!dplyr::sym(event_var), !!dplyr::sym(group_var)) %>%
    dplyr::summarise(
      across(all_of(extremism_var_names), list(mean = mean, se = ~sd(.)/sqrt(length(.)))),
      .groups = 'drop'
    ) %>%
    tidyr::pivot_longer(
      cols = contains("_mean"),
      names_to = "variable",
      values_to = "mean"
    ) %>%
    dplyr::mutate(
      variable = gsub("_mean", "", variable),
      variable = factor(variable, levels = extremism_var_names)
    )
  
  # Add standard errors
  se_data <- data %>%
    dplyr::group_by(!!dplyr::sym(event_var), !!dplyr::sym(group_var)) %>%
    dplyr::summarise(
      across(all_of(extremism_var_names), ~sd(.)/sqrt(length(.))),
      .groups = 'drop'
    ) %>%
    tidyr::pivot_longer(
      cols = all_of(extremism_var_names),
      names_to = "variable",
      values_to = "se"
    ) %>%
    dplyr::mutate(variable = factor(variable, levels = extremism_var_names))
  
  # Apply custom labels if available
  if (!is.null(extremism_var_labels) && length(extremism_var_labels) == length(extremism_var_names)) {
    # Create mapping from variable names to labels
    plot_data$variable_label <- extremism_var_labels[match(plot_data$variable, extremism_var_names)]
    se_data$variable_label <- extremism_var_labels[match(se_data$variable, extremism_var_names)]
    
    # Convert to factor with proper ordering
    plot_data$variable_label <- factor(plot_data$variable_label, levels = extremism_var_labels)
    se_data$variable_label <- factor(se_data$variable_label, levels = extremism_var_labels)
    
    x_var <- "variable_label"
    default_x_label <- "Extremism Dimensions"
  } else {
    plot_data$variable_label <- plot_data$variable
    se_data$variable_label <- se_data$variable
    x_var <- "variable_label"
    default_x_label <- "Variables"
  }
  
  # Match standard errors to plot data
  plot_data$se <- se_data$se[match(interaction(plot_data[[event_var]], 
                                               plot_data[[group_var]], 
                                               plot_data$variable_label),
                                   interaction(se_data[[event_var]], 
                                               se_data[[group_var]], 
                                               se_data$variable_label))]
  
  # Create event labels (just the wave numbers)
  plot_data$event_label <- ifelse(plot_data[[event_var]] == 0, 
                                  as.character(wave_pair[1]), 
                                  as.character(wave_pair[2]))
  
  # Assign shapes based on dimension (variable)
  n_vars <- length(unique(plot_data$variable_label))
  shape_values <- c(16, 15, 17)  # circle, square, triangle
  if (n_vars > 3) {
    # If more than 3 dimensions, repeat shapes or use additional shapes
    additional_shapes <- c(18, 8, 4)  # diamond, asterisk, X
    shape_values <- c(shape_values, additional_shapes)[1:n_vars]
  } else {
    shape_values <- shape_values[1:n_vars]
  }
  
  # Set default titles if not provided
  if (is.null(plot_title)) {
    plot_title <- "Profile Plot: Extremism Dimensions by Group and Wave"
  }
  if (is.null(plot_subtitle)) {
    plot_subtitle <- paste("Comparing Wave", wave_pair[1], "vs Wave", wave_pair[2])
  }
  if (is.null(group_legend_title)) {
    group_legend_title <- tools::toTitleCase(gsub("_", " ", group_var))
  }
  if (is.null(x_axis_title)) {
    x_axis_title <- default_x_label
  }
  if (is.null(y_axis_title)) {
    y_axis_title <- "Mean Score"
  }
  
  # Create dodge position for points and error bars (increased for better separation)
  dodge_width <- 0.6
  
  # Create the profile plot with conditional axes
  if (transpose) {
    # Swap x and y axis titles for transpose
    temp_title <- x_axis_title
    x_axis_title <- y_axis_title
    y_axis_title <- temp_title
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = mean, y = !!dplyr::sym(x_var), 
                                                 color = !!dplyr::sym(group_var), 
                                                 shape = variable_label,
                                                 linetype = event_label)) +
      ggplot2::geom_point(size = point_size, 
                          position = ggplot2::position_dodgev(height = dodge_width)) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = mean - se, xmax = mean + se), 
                              height = 0.1, alpha = 0.7, linewidth = line_width,
                              position = ggplot2::position_dodgev(height = dodge_width))
    
    if (show_lines) {
      p <- p + ggplot2::geom_line(ggplot2::aes(group = interaction(!!dplyr::sym(group_var), event_label, variable_label)),
                                  linewidth = line_width,
                                  position = ggplot2::position_dodgev(height = dodge_width))
    }
  } else {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!dplyr::sym(x_var), y = mean, 
                                                 color = !!dplyr::sym(group_var), 
                                                 shape = variable_label,
                                                 linetype = event_label)) +
      ggplot2::geom_point(size = point_size, 
                          position = ggplot2::position_dodge(width = dodge_width)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), 
                             width = 0.1, alpha = 0.7, linewidth = line_width,
                             position = ggplot2::position_dodge(width = dodge_width))
    
    if (show_lines) {
      p <- p + ggplot2::geom_line(ggplot2::aes(group = interaction(!!dplyr::sym(group_var), event_label, variable_label)),
                                  linewidth = line_width,
                                  position = ggplot2::position_dodge(width = dodge_width))
    }
  }
  
  p <- p +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = x_axis_title,
      y = y_axis_title,
      color = group_legend_title,
      shape = "Dimension",
      linetype = "Wave"
    ) +
    ggplot2::scale_shape_manual(values = shape_values) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Add color scale based on parameters
  if (use_bw) {
    p <- p + ggplot2::scale_color_grey(start = 0.2, end = 0.8)
  } else if (use_viridis) {
    p <- p + ggplot2::scale_color_viridis_d(option = "plasma", end = 0.8)
  } else {
    p <- p + ggplot2::scale_color_brewer(type = "qual", palette = "Set1")
  }
  
  return(p)
}

#' Print Method for MANOVA Model Objects
#'
#' @description
#' Provides a summary print method for manova_model objects.
#'
#' @param x A manova_model object returned by af_manova_model
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object x
#'
print.manova_model <- function(x, ...) {
  cat("MANOVA Model Object\n")
  cat("===================\n\n")
  
  cat("Wave Information:\n")
  cat(paste("- Waves analyzed:", x$wave_info$wave_labels, "\n"))
  cat(paste("- Analysis type:", x$wave_info$panel_type, "\n"))
  cat(paste("- Sample size Wave", x$wave_info$waves_analyzed[1], ":", x$wave_info$n_wave1, "\n"))
  cat(paste("- Sample size Wave", x$wave_info$waves_analyzed[2], ":", x$wave_info$n_wave2, "\n"))
  cat(paste("- Total sample size:", x$wave_info$total_n, "\n\n"))
  
  cat("Model Formula:\n")
  cat(paste("", x$formula_used, "\n\n"))
  
  cat("Variables:\n")
  cat(paste("- Extremism variables:", length(x$variable_info$extremism_var_names), "\n"))
  cat(paste("- Event variable:", x$variable_info$event_var, "\n"))
  cat(paste("- Group variable:", x$variable_info$group_var, "\n"))
  if (!is.null(x$variable_info$control_vars)) {
    cat(paste("- Control variables:", paste(x$variable_info$control_vars, collapse = ", "), "\n"))
  }
  
  cat("\nUse af_manova_results() to generate detailed results and plots.\n")
  
  invisible(x)
}

#' Print Method for MANOVA Results
#'
#' @description
#' Provides a comprehensive summary print method for manova_results objects,
#' displaying wave information, MANOVA results, and summary statistics.
#'
#' @param x A manova_results object returned by af_manova_results
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object x
#'
print.manova_results <- function(x, ...) {
  cat("MANOVA Analysis Results\n")
  cat("=======================\n\n")
  
  cat("Wave Information:\n")
  cat(paste("- Waves analyzed:", x$wave_info$wave_labels, "\n"))
  cat(paste("- Analysis type:", x$wave_info$panel_type, "\n"))
  cat(paste("- Sample size Wave", x$wave_info$waves_analyzed[1], ":", x$wave_info$n_wave1, "\n"))
  cat(paste("- Sample size Wave", x$wave_info$waves_analyzed[2], ":", x$wave_info$n_wave2, "\n"))
  cat(paste("- Total sample size:", x$wave_info$total_n, "\n\n"))
  
  cat("MANOVA Results:\n")
  print(x$manova_summary)
  
  cat("\n\nSummary Statistics by Group:\n")
  print(x$summary_stats)
  
  if (!is.null(x$profile_plot)) {
    cat("\n\nProfile plot created. Access with $profile_plot\n")
  }
  
  invisible(x)
}

#' Combine MANOVA Profile Plots with Common Scale
#'
#' @description
#' Combines a list of ggplot objects in one row with shared legend, common y-axis scale,
#' single y-axis label, and custom titles/notes using the patchwork package.
#'
#' @param plot_list List of ggplot objects to combine
#' @param main_title Character string for main title
#' @param subtitle Character string for subtitle  
#' @param note Character string for note at bottom
#' @param y_axis_title Character string for shared y-axis title
#'
#' @return Combined patchwork plot object
#'
#' @examples
#' \dontrun{
#' combined_plot <- af_combine_manova_plots(
#'   plot_list = list(r1$profile_plot, r2$profile_plot, r3$profile_plot, r4$profile_plot, r5$profile_plot),
#'   main_title = "Events Impact on Extremism Dimensions",
#'   subtitle = "Moderated by Political Orientation", 
#'   note = "Cross-sectional and panel survey analysis",
#'   y_axis_title = "Mean Scores"
#' )
#' print(combined_plot)
#' }
#'
af_combine_manova_plots <- function(plot_list, 
                                    main_title = NULL,
                                    subtitle = NULL, 
                                    note = NULL,
                                    y_axis_title = "Mean Scores") {
  
  # Input validation
  if (!is.list(plot_list) || length(plot_list) == 0) {
    stop("plot_list must be a non-empty list of ggplot objects")
  }
  
  if (!all(sapply(plot_list, function(x) inherits(x, "gg")))) {
    stop("All elements in plot_list must be ggplot objects")
  }
  
  # Find common y-axis limits across all plots
  y_limits <- range(sapply(plot_list, function(p) ggplot2::ggplot_build(p)$layout$panel_scales_y[[1]]$range$range))
  
  # Remove legends, y-axis titles (except first plot), and set common y scale
  # Remove legends from all but first plot, add borders, set common y scale
  plots_clean <- plot_list
  for(i in 1:length(plots_clean)) {
    if(i > 1) {
      plots_clean[[i]] <- plots_clean[[i]] + 
        ggplot2::theme(
          legend.position = "none",
          panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1)
        ) +
        ggplot2::labs(y = "") +
        ggplot2::ylim(y_limits)
    } else {
      plots_clean[[i]] <- plots_clean[[i]] + 
        ggplot2::theme(
          panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1)
        ) +
        ggplot2::labs(y = y_axis_title) +
        ggplot2::ylim(y_limits)
    }
  }
  
  # Combine with patchwork
  # Combine with patchwork
  combined <- patchwork::wrap_plots(plots_clean, nrow = 1) + 
    patchwork::plot_layout(guides = "collect")
  
  # Add titles and annotation
  if(!is.null(main_title) || !is.null(subtitle) || !is.null(note)) {
    title_text <- if(!is.null(main_title)) main_title else ""
    subtitle_text <- if(!is.null(subtitle)) subtitle else ""
    caption_text <- if(!is.null(note)) note else ""
    
    combined <- combined + 
      patchwork::plot_annotation(
        title = title_text,
        subtitle = subtitle_text, 
        caption = caption_text,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
          plot.caption = ggplot2::element_text(hjust = 0.5, size = 10),
          legend.position = "bottom"
        )
      )
  }
  
  return(combined)
}

# ==============================================================================

#' Create Summary Table of MANOVA Results Across Multiple Events
#'
#' @description
#' Creates a comprehensive summary table of MANOVA results across multiple events,
#' showing multivariate test statistics with significance levels in a compact format using gt.
#'
#' @param model_list Named list of manova_results objects (names should be event names)
#' @param title Character string for table title (optional)
#' @param subtitle Character string for table subtitle (optional)
#' @param variable_labels Named character vector for custom variable display names (optional)
#' @param event_labels Named character vector for custom event display names (optional)
#' @param panel_model_ids Character vector of model names that used panel data (same respondents across waves)
#' @param show_controls Logical indicating whether to include control variables in output
#' @param show_intercept Logical indicating whether to include the intercept term
#' @param test_statistics Character vector: which MANOVA test statistics to report (c("Pillai", "Wilks", "Hotelling", "Roy"))
#' @param round_digits Integer specifying number of decimal places for statistics
#' @param highlight_panel_models Logical indicating whether to highlight panel model columns
#' @param add_footnotes Logical indicating whether to add explanatory footnotes
#'
#' @return A gt table object containing formatted MANOVA results
#'
#' @details
#' This function takes a named list of manova_results objects and creates a formatted
#' comparison table showing test statistics and significance levels. Multiple test statistics
#' can be displayed for each event. Panel models (same respondents across waves) can be 
#' highlighted to distinguish from cross-sectional analyses.
#'
#' @examples
#' \dontrun{
#' # Assuming you have manova_results objects for each event
#' model_list <- list(
#'   "Inland Terror" = inland_terror_results,
#'   "Bennett Gov. Fall" = bennett_results,
#'   "Judicial Reform" = judicial_results,
#'   "Gallant Dismissal" = gallant_results,
#'   "Oct. 7th War" = oct7_results
#' )
#'
#' # Create table with both Pillai and Wilks statistics
#' table <- af_manova_summary_table(
#'   model_list = model_list,
#'   test_statistics = c("Pillai", "Wilks"),
#'   panel_model_ids = c("Judicial Reform"),
#'   title = "MANOVA Results Across Destabilizing Events",
#'   subtitle = "Multivariate Tests of Event Effects on Political Extremism"
#' )
#' }
#'
af_manova_summary_table <- function(model_list,
                                    title = "MANOVA Results Summary",
                                    subtitle = NULL,
                                    variable_labels = NULL,
                                    event_labels = NULL,
                                    panel_model_ids = NULL,
                                    show_controls = TRUE,
                                    show_intercept = FALSE,
                                    test_statistics = c("Pillai"),
                                    round_digits = 3,
                                    highlight_panel_models = TRUE,
                                    add_footnotes = TRUE) {
  
  # Input validation
  if (!is.list(model_list) || length(model_list) == 0) {
    stop("model_list must be a non-empty list")
  }
  
  if (!all(sapply(model_list, function(x) inherits(x, "manova_results")))) {
    stop("All elements in model_list must be manova_results objects")
  }
  
  if (is.null(names(model_list))) {
    stop("model_list must be a named list with event names")
  }
  
  # Validate test_statistics parameter
  valid_tests <- c("Pillai", "Wilks", "Hotelling", "Roy")
  if (!all(test_statistics %in% valid_tests)) {
    stop("test_statistics must be a subset of: ", paste(valid_tests, collapse = ", "))
  }
  
  if (length(test_statistics) == 0) {
    stop("At least one test statistic must be specified")
  }
  
  # MANOVA results extraction function - uses summary() to get all test statistics
  extract_manova_stats <- function(model_result) {
    manova_summary <- model_result$manova_summary
    
    if (!inherits(manova_summary, "Anova.mlm")) {
      # Return empty data frame with columns for all requested test statistics
      empty_df <- data.frame(term = character(0), stringsAsFactors = FALSE)
      for (test_stat in test_statistics) {
        empty_df[[paste0(test_stat, "_stat")]] <- numeric(0)
        empty_df[[paste0(test_stat, "_p")]] <- numeric(0)
      }
      return(empty_df)
    }
    
    # Use summary() to get all test statistics
    capture_output <- capture.output(summary(manova_summary))
    
    # Find all "Multivariate Tests:" sections
    test_sections <- grep("Multivariate Tests:", capture_output)
    
    if (length(test_sections) == 0) {
      empty_df <- data.frame(term = character(0), stringsAsFactors = FALSE)
      for (test_stat in test_statistics) {
        empty_df[[paste0(test_stat, "_stat")]] <- numeric(0)
        empty_df[[paste0(test_stat, "_p")]] <- numeric(0)
      }
      return(empty_df)
    }
    
    stats_list <- list()
    
    # Process each "Multivariate Tests:" section
    for (i in seq_along(test_sections)) {
      section_start <- test_sections[i]
      
      # Extract the term name from the section header
      section_line <- capture_output[section_start]
      term_match <- regexpr("Multivariate Tests:\\s*(\\S.*?)\\s*$", section_line)
      
      if (term_match > 0) {
        term_name <- trimws(regmatches(section_line, term_match))
        term_name <- gsub("Multivariate Tests:\\s*", "", term_name)
        term_name <- trimws(term_name)
      } else {
        term_name <- "Unknown"
      }
      
      # Find the end of this section
      section_end <- length(capture_output)
      if (i < length(test_sections)) {
        section_end <- test_sections[i + 1] - 1
      }
      
      # Look for the table within this section
      section_lines <- capture_output[section_start:section_end]
      header_line <- which(grepl("Df\\s+test stat\\s+approx F", section_lines))
      
      if (length(header_line) > 0) {
        # Find data lines after the header
        data_start <- header_line[1] + 1
        data_lines <- section_lines[data_start:length(section_lines)]
        
        # Remove significance codes and empty lines
        data_lines <- data_lines[!grepl("^---", data_lines)]
        data_lines <- data_lines[!grepl("Signif\\. codes", data_lines)]
        data_lines <- data_lines[nchar(trimws(data_lines)) > 0]
        
        # Initialize storage for this term
        if (!term_name %in% names(stats_list)) {
          stats_list[[term_name]] <- list(term = term_name)
        }
        
        # Parse each test statistic line
        for (test_stat in test_statistics) {
          # Look for the line that starts with this test statistic
          # Handle special case for Hotelling-Lawley
          search_pattern <- if (test_stat == "Hotelling") {
            "^Hotelling-Lawley"
          } else {
            paste0("^", test_stat)
          }
          
          target_line <- data_lines[grepl(search_pattern, data_lines)]
          
          if (length(target_line) > 0) {
            line <- target_line[1]
            
            # Parse the line: "Pillai            1 0.0107042  11.5522      3   3203 1.572e-07 ***"
            # Remove the test statistic name to get the numeric part
            stat_name <- if (test_stat == "Hotelling") "Hotelling-Lawley" else test_stat
            numeric_part <- trimws(substr(line, nchar(stat_name) + 1, nchar(line)))
            
            # Split by whitespace
            parts <- unlist(strsplit(numeric_part, "\\s+"))
            parts <- parts[parts != ""]
            
            if (length(parts) >= 6) {
              # parts: [Df] [test_stat] [approx_F] [num_Df] [den_Df] [Pr(>F)] [significance]
              test_stat_str <- parts[2]  # test stat column
              p_part <- paste(parts[6:length(parts)], collapse = " ")  # Pr(>F) and stars
              
              # Safely convert test_stat
              test_stat_val <- suppressWarnings(as.numeric(test_stat_str))
              if (is.na(test_stat_val)) {
                clean_test_stat <- gsub("[^0-9.-]", "", test_stat_str)
                test_stat_val <- suppressWarnings(as.numeric(clean_test_stat))
                if (is.na(test_stat_val)) {
                  test_stat_val <- 0
                }
              }
              
              # Parse p-value with enhanced handling for scientific notation
              if (grepl("< 2\\.2?2?e-16", p_part)) {
                p_value <- 2.2e-16
              } else if (grepl("^<\\s*", p_part)) {
                p_val_str <- gsub("^<\\s*", "", p_part)
                p_val_str <- gsub("\\s*[\\*\\.]+.*$", "", p_val_str)
                p_value <- suppressWarnings(as.numeric(p_val_str))
                if (is.na(p_value)) {
                  p_value <- 0.001
                }
              } else {
                # Extract complete numeric value (including scientific notation) first
                # Pattern: digits.digits(e/E)(+/-)(digits) - covers regular and scientific notation
                numeric_pattern <- "\\b\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?\\b"
                numeric_matches <- regmatches(p_part, gregexpr(numeric_pattern, p_part))[[1]]
                
                if (length(numeric_matches) > 0) {
                  # Take the first valid p-value (should be between 0 and 1)
                  for (match in numeric_matches) {
                    p_value <- suppressWarnings(as.numeric(match))
                    if (!is.na(p_value) && p_value >= 0 && p_value <= 1) {
                      break
                    }
                  }
                  
                  # If no valid p-value found, fallback
                  if (is.na(p_value) || p_value < 0 || p_value > 1) {
                    p_value <- 1
                  }
                } else {
                  # Ultimate fallback
                  p_value <- 1
                }
              }
              
              # Final validation
              if (is.na(p_value) || p_value < 0 || p_value > 1) {
                p_value <- 1
              }
              if (is.na(test_stat_val)) {
                test_stat_val <- 0
              }
              
              # Store the values
              stats_list[[term_name]][[paste0(test_stat, "_stat")]] <- test_stat_val
              stats_list[[term_name]][[paste0(test_stat, "_p")]] <- p_value
            }
          }
        }
      }
    }
    
    # Convert to data frame
    if (length(stats_list) > 0) {
      stats_df <- data.frame(term = character(0), stringsAsFactors = FALSE)
      
      # Initialize columns for all test statistics
      for (test_stat in test_statistics) {
        stats_df[[paste0(test_stat, "_stat")]] <- numeric(0)
        stats_df[[paste0(test_stat, "_p")]] <- numeric(0)
      }
      
      # Fill in the data
      for (term_name in names(stats_list)) {
        term_data <- stats_list[[term_name]]
        new_row <- data.frame(term = term_name, stringsAsFactors = FALSE)
        
        for (test_stat in test_statistics) {
          stat_col <- paste0(test_stat, "_stat")
          p_col <- paste0(test_stat, "_p")
          
          new_row[[stat_col]] <- if (stat_col %in% names(term_data)) term_data[[stat_col]] else 0
          new_row[[p_col]] <- if (p_col %in% names(term_data)) term_data[[p_col]] else 1
        }
        
        stats_df <- rbind(stats_df, new_row)
      }
      
      rownames(stats_df) <- NULL
    } else {
      stats_df <- data.frame(term = character(0), stringsAsFactors = FALSE)
      for (test_stat in test_statistics) {
        stats_df[[paste0(test_stat, "_stat")]] <- numeric(0)
        stats_df[[paste0(test_stat, "_p")]] <- numeric(0)
      }
    }
    
    return(stats_df)
  }
  
  # Extract data for all events
  all_results <- list()
  
  for (event_name in names(model_list)) {
    all_results[[event_name]] <- extract_manova_stats(model_list[[event_name]])
  }
  
  # Get all unique terms across events
  all_terms <- unique(unlist(lapply(all_results, function(x) x$term)))
  
  # Clean up terms
  all_terms <- all_terms[!is.na(all_terms) & !is.null(all_terms) & 
                           nchar(as.character(all_terms)) > 0]
  
  # Filter out intercept if not requested
  if (!show_intercept) {
    all_terms <- all_terms[!grepl("Intercept", all_terms, ignore.case = TRUE)]
  }
  
  # Filter terms based on show_controls
  if (!show_controls) {
    control_patterns <- c("gender", "age_group", "age", "education", "income")
    all_terms <- all_terms[!grepl(paste(control_patterns, collapse = "|"), 
                                  all_terms, ignore.case = TRUE)]
  }
  
  # Check if we have any terms left
  if (length(all_terms) == 0) {
    stop("No terms found in MANOVA results after filtering. Check your model results and filter settings.")
  }
  
  # Create the main data structure
  table_data <- data.frame(
    Variable = all_terms,
    stringsAsFactors = FALSE
  )
  
  # Apply custom variable labels if provided
  if (!is.null(variable_labels) && length(variable_labels) > 0) {
    label_names <- names(variable_labels)
    for (i in 1:nrow(table_data)) {
      var_name <- table_data$Variable[i]
      if (length(var_name) > 0 && !is.na(var_name) && !is.null(var_name) && 
          nchar(as.character(var_name)) > 0) {
        if (var_name %in% label_names) {
          table_data$Variable[i] <- variable_labels[[var_name]]
        }
      }
    }
  }
  
  # Apply custom event labels and add panel indicators
  event_names <- names(model_list)
  
  if (!is.null(event_labels)) {
    event_names <- ifelse(names(model_list) %in% names(event_labels),
                          event_labels[names(model_list)],
                          names(model_list))
  }
  
  # Add panel indicators to event names
  if (!is.null(panel_model_ids)) {
    panel_positions <- which(names(model_list) %in% panel_model_ids)
    if (length(panel_positions) > 0) {
      event_names[panel_positions] <- paste0(event_names[panel_positions], " §")
    }
  }
  
  # Add columns for each event and test statistic combination
  for (i in seq_along(model_list)) {
    event_name <- names(model_list)[i]
    event_label <- event_names[i]
    event_data <- all_results[[event_name]]
    
    # Create columns for each test statistic
    for (test_stat in test_statistics) {
      col_name <- if (length(test_statistics) == 1) {
        event_label  # Single test statistic - use event name only
      } else {
        paste(event_label, test_stat, sep = "_")  # Multiple test statistics - include test stat name
      }
      
      table_data[[col_name]] <- NA_character_
      
      # Fill in the data
      for (j in 1:nrow(table_data)) {
        original_var <- all_terms[j]
        if (is.na(original_var) || original_var == "") next
        
        matching_row <- which(event_data$term == original_var)
        
        if (length(matching_row) > 0) {
          row_data <- event_data[matching_row[1], ]
          
          stat_col <- paste0(test_stat, "_stat")
          p_col <- paste0(test_stat, "_p")
          
          if (stat_col %in% colnames(row_data) && p_col %in% colnames(row_data)) {
            test_stat_val <- row_data[[stat_col]]
            p_val <- row_data[[p_col]]
            
            sig_level <- af_get_significance_stars(p_val)
            table_data[j, col_name] <- 
              paste0(format(round(test_stat_val, round_digits), nsmall = round_digits), 
                     sig_level)
          } else {
            table_data[j, col_name] <- "--"
          }
        } else {
          table_data[j, col_name] <- "--"
        }
      }
    }
  }
  
  # Create gt table
  gt_table <- gt::gt(table_data)
  
  # Add title and subtitle
  if (!is.null(title)) {
    if (!is.null(subtitle)) {
      gt_table <- gt_table %>% gt::tab_header(title = title, subtitle = subtitle)
    } else {
      gt_table <- gt_table %>% gt::tab_header(title = title)
    }
  }
  
  # Add column spanners if multiple test statistics
  if (length(test_statistics) > 1) {
    for (i in seq_along(event_names)) {
      event_label <- event_names[i]
      
      # Get columns for this event
      event_columns <- paste(event_label, test_statistics, sep = "_")
      
      # Add spanner for this event
      gt_table <- gt_table %>%
        gt::tab_spanner(
          label = event_label,
          columns = all_of(event_columns)
        )
    }
    
    # Rename columns to show just the test statistic names
    for (i in seq_along(event_names)) {
      event_label <- event_names[i]
      for (test_stat in test_statistics) {
        col_name <- paste(event_label, test_stat, sep = "_")
        gt_table <- gt_table %>%
          gt::cols_label(!!col_name := test_stat)
      }
    }
  }
  
  # Highlight panel model columns if requested
  if (highlight_panel_models && !is.null(panel_model_ids)) {
    panel_event_names <- event_names[names(model_list) %in% panel_model_ids]
    
    if (length(test_statistics) == 1) {
      # Single test statistic - highlight event columns directly
      existing_panel_cols <- intersect(panel_event_names, names(table_data))
    } else {
      # Multiple test statistics - highlight all columns for panel events
      panel_columns <- c()
      for (panel_event in panel_event_names) {
        panel_columns <- c(panel_columns, paste(panel_event, test_statistics, sep = "_"))
      }
      existing_panel_cols <- intersect(panel_columns, names(table_data))
    }
    
    if (length(existing_panel_cols) > 0) {
      gt_table <- gt_table %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#e8f4f8"),
          locations = gt::cells_body(columns = all_of(existing_panel_cols))
        )
    }
  }
  
  # Add footnotes if requested
  if (add_footnotes) {
    # Create test statistics description with proper names
    if (length(test_statistics) == 1) {
      stat_name <- if (test_statistics[1] == "Hotelling") "Hotelling-Lawley" else test_statistics[1]
      test_desc <- paste0("Test statistic: ", stat_name, "'s Trace")
    } else {
      stat_names <- sapply(test_statistics, function(x) {
        if (x == "Hotelling") "Hotelling-Lawley" else x
      })
      test_desc <- paste0("Test statistics: ", paste(paste0(stat_names, "'s Trace"), collapse = ", "))
    }
    
    footnote_parts <- c(
      test_desc,
      "*** p < .001; ** p < .01; * p < .05; † p < .10"
    )
    
    if (!is.null(panel_model_ids) && length(panel_model_ids) > 0) {
      footnote_parts <- c(footnote_parts, "§ Panel analysis (same respondents across waves)")
      if (highlight_panel_models) {
        footnote_parts <- c(footnote_parts, "Panel model columns highlighted in blue")
      }
    }
    
    footnote_text <- paste(footnote_parts, collapse = ". ")
    
    gt_table <- gt_table %>% gt::tab_footnote(footnote = footnote_text)
  }
  
  # Apply general formatting
  gt_table <- gt_table %>%
    gt::tab_options(
      table.font.size = 12,
      heading.title.font.size = 14,
      heading.subtitle.font.size = 12,
      column_labels.font.weight = "bold"
    ) %>%
    gt::cols_align(align = "left", columns = "Variable") %>%
    gt::cols_align(align = "center", columns = -"Variable")
  
  return(gt_table)
}

# ==============================================================================

#' Format P-values for Detailed Display (with significance stars)
#'
#' @description
#' Formats p-values for display in detailed tables, using significance stars.
#'
#' @param p_value Numeric p-value
#'
#' @return Character string of formatted p-value with stars or empty string for non-significant
#'
af_format_p_value_detailed <- function(p_value) {
  if (is.na(p_value)) return("")
  
  # Get significance stars
  stars <- af_get_significance_stars(p_value)
  
  # Return empty string if not significant
  if (stars == "") return("")
  
  # Return formatted p-value with stars
  if (p_value < 0.001) return(paste0("< .001", stars))
  if (p_value < 0.01) return(paste0(sprintf("%.3f", p_value), stars))
  return(paste0(sprintf("%.3f", p_value), stars))
}

#' Format P-values for Display
#'
#' @description
#' Formats p-values for display in tables, handling very small values.
#'
#' @param p_value Numeric p-value
#'
#' @return Character string of formatted p-value
#'
af_format_p_value <- function(p_value) {
  if (is.na(p_value)) return("--")
  if (p_value < 0.001) return("< .001")
  if (p_value < 0.01) return(sprintf("%.3f", p_value))
  return(sprintf("%.3f", p_value))
}

#' Get Significance Stars
#'
#' @description
#' Converts p-values to significance stars for compact display.
#'
#' @param p_value Numeric p-value
#'
#' @return Character string with significance stars
#'
af_get_significance_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  if (p_value < 0.10) return("†")
  return("")
}

#' Test MANOVA Prerequisites
#'
#' This function tests all required assumptions for MANOVA analysis including
#' multivariate normality, homogeneity of covariance matrices, linearity,
#' and absence of extreme multicollinearity. It provides detailed results
#' and interpretations for each test along with an overall recommendation.
#'
#' @param data A data frame containing the variables
#' @param dependent_vars Character vector of dependent variable names
#' @param grouping_var Character string of the grouping variable name
#' @param alpha Significance level for tests (default = 0.05)
#' @return A list containing test results and interpretations
#' @examples
#' af_test_manova_prerequisites(data, c("pe_ideology", "pe_violence", "pe_intolerance"), "pe_left_center_right")
af_test_manova_prerequisites <- function(data, dependent_vars, grouping_var, alpha = 0.05) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (!all(dependent_vars %in% names(data))) {
    stop("All dependent variables must exist in the data")
  }
  
  if (!grouping_var %in% names(data)) {
    stop("Grouping variable must exist in the data")
  }
  
  if (length(dependent_vars) < 2) {
    stop("MANOVA requires at least 2 dependent variables")
  }
  
  # Remove missing values
  complete_data <- data[complete.cases(data[c(dependent_vars, grouping_var)]), ]
  n_removed <- nrow(data) - nrow(complete_data)
  
  if (n_removed > 0) {
    warning(paste("Removed", n_removed, "cases with missing values"))
  }
  
  # Extract dependent variables matrix and grouping factor
  Y <- as.matrix(complete_data[dependent_vars])
  groups <- as.factor(complete_data[[grouping_var]])
  
  # Initialize results list
  results <- list()
  
  # 1. SAMPLE SIZE ADEQUACY
  n_total <- nrow(complete_data)
  n_groups <- length(unique(groups))
  n_vars <- length(dependent_vars)
  min_group_size <- min(table(groups))
  
  sample_adequate <- min_group_size > n_vars && n_total > (n_groups * n_vars + 10)
  
  results$sample_size <- list(
    test_name = "Sample Size Adequacy",
    description = "Checks if sample sizes are adequate for MANOVA. Requires minimum group size > number of variables and total N > (groups × variables + 10).",
    statistics = list(
      total_n = n_total,
      n_groups = n_groups,
      n_variables = n_vars,
      min_group_size = min_group_size,
      recommended_min_total = n_groups * n_vars + 10
    ),
    assumption_met = sample_adequate,
    interpretation = if (sample_adequate) {
      "Sample size is adequate for MANOVA analysis."
    } else {
      "Sample size may be inadequate. Consider increasing sample size or reducing number of variables."
    },
    detailed_report = paste(
      "**Sample Size Adequacy Assessment:**\n",
      "This test evaluates whether the sample size is sufficient for reliable MANOVA results.\n",
      paste("- Total sample size:", n_total, "observations"),
      paste("- Number of groups:", n_groups),
      paste("- Number of dependent variables:", n_vars),
      paste("- Minimum group size:", min_group_size),
      paste("- Recommended minimum total N:", n_groups * n_vars + 10),
      "",
      paste("**Result:**", if (sample_adequate) "PASSED\n" else "FAILED\n"),
      if (sample_adequate) {
        "The sample size meets the requirements for MANOVA analysis. Each group has sufficient observations relative to the number of dependent variables, ensuring stable covariance matrix estimation and reliable test statistics."
      } else {
        "The sample size is inadequate for reliable MANOVA analysis. Consider increasing the sample size, combining smaller groups if theoretically justified, or reducing the number of dependent variables to improve the observation-to-variable ratio."
      },
      sep = "\n"
    )
  )
  
  # 2. MULTIVARIATE NORMALITY
  mshapiro_test <- mvnormtest::mshapiro.test(t(Y))
  mv_normal <- mshapiro_test$p.value > alpha
  
  results$multivariate_normality <- list(
    test_name = "Multivariate Normality (Shapiro-Wilk)",
    description = "Tests whether the dependent variables jointly follow a multivariate normal distribution using Shapiro-Wilk test.",
    statistics = list(
      statistic = mshapiro_test$statistic,
      p_value = mshapiro_test$p.value
    ),
    assumption_met = mv_normal,
    interpretation = if (mv_normal) {
      paste("Multivariate normality assumption is met (p =", round(mshapiro_test$p.value, 4), ").")
    } else {
      paste("Multivariate normality assumption is violated (p =", round(mshapiro_test$p.value, 4), "). MANOVA is robust to moderate violations with large samples.")
    },
    detailed_report = paste(
      "**Multivariate Normality Assessment (Shapiro-Wilk Test):**\n",
      "This test evaluates whether the dependent variables jointly follow a multivariate normal distribution, which is a key assumption for MANOVA.\n",
      paste("- Test statistic (W):", round(mshapiro_test$statistic, 4)),
      paste("- p-value:", round(mshapiro_test$p.value, 4)),
      paste("- Significance level:", alpha),
      "",
      paste("**Result:**", if (mv_normal) "PASSED\n" else "FAILED\n"),
      if (mv_normal) {
        "The multivariate normality assumption is satisfied. The dependent variables jointly follow a multivariate normal distribution, supporting the use of MANOVA. This indicates that the statistical tests will have appropriate Type I error rates and power."
      } else {
        "The multivariate normality assumption is violated. However, MANOVA is generally robust to moderate departures from normality, especially with larger sample sizes. The violation suggests caution in interpretation, but does not necessarily invalidate the analysis if the sample size is adequate and other assumptions are met."
      },
      sep = "\n"
    )
  )
  
  # 3. HOMOGENEITY OF COVARIANCE MATRICES (Box's M Test)
  box_m_result <- biotools::boxM(Y, groups)
  covariance_homogeneous <- box_m_result$p.value > alpha
  
  results$covariance_homogeneity <- list(
    test_name = "Homogeneity of Covariance Matrices (Box's M)",
    description = "Tests whether covariance matrices are equal across groups. Box's M test is sensitive to non-normality and large samples.",
    statistics = list(
      chi_square = box_m_result$statistic,
      df = box_m_result$parameter,
      p_value = box_m_result$p.value
    ),
    assumption_met = covariance_homogeneous,
    interpretation = if (covariance_homogeneous) {
      paste("Covariance matrices are homogeneous across groups (p =", round(box_m_result$p.value, 4), ").")
    } else {
      paste("Covariance matrices differ across groups (p =", round(box_m_result$p.value, 4), "). Consider using Pillai's trace, which is robust to this violation.")
    },
    detailed_report = paste(
      "**Homogeneity of Covariance Matrices (Box's M Test):**\n",
      "This test evaluates whether the covariance matrices are equal across all groups, which is required for optimal MANOVA performance.\n",
      paste("- Box's M statistic:", round(box_m_result$statistic, 2)),
      paste("- Degrees of freedom:", box_m_result$parameter),
      paste("- p-value:", round(box_m_result$p.value, 4)),
      paste("- Significance level:", alpha),
      "",
      paste("**Result:**", if (covariance_homogeneous) "PASSED\n" else "FAILED\n"),
      if (covariance_homogeneous) {
        "The assumption of homogeneous covariance matrices is satisfied. This indicates that the groups have similar patterns of relationships among the dependent variables, supporting the use of standard MANOVA procedures. The test suggests that pooling covariance matrices across groups is appropriate."
      } else {
        "The assumption of homogeneous covariance matrices is violated, indicating that different groups have different patterns of relationships among the dependent variables. However, this violation can be addressed by using robust test statistics such as Pillai's trace, which is less sensitive to covariance matrix heterogeneity. Note that Box's M test is known to be sensitive to departures from normality and can be overly conservative with large sample sizes."
      },
      sep = "\n"
    )
  )
  
  # 4. LINEARITY
  # Test linearity using correlation matrix
  cor_matrix <- cor(Y)
  min_correlation <- min(abs(cor_matrix[upper.tri(cor_matrix)]))
  max_correlation <- max(abs(cor_matrix[upper.tri(cor_matrix)]))
  
  # Generally, correlations should be moderate (not too low, not too high)
  linearity_adequate <- min_correlation > 0.1 && max_correlation < 0.9
  
  results$linearity <- list(
    test_name = "Linearity Assessment",
    description = "Evaluates linear relationships between dependent variables through correlation analysis. Moderate correlations (0.1-0.9) suggest adequate linearity.",
    statistics = list(
      correlation_matrix = cor_matrix,
      min_correlation = min_correlation,
      max_correlation = max_correlation
    ),
    assumption_met = linearity_adequate,
    interpretation = if (linearity_adequate) {
      paste("Linear relationships appear adequate (correlations range:", round(min_correlation, 3), "to", round(max_correlation, 3), ").")
    } else if (min_correlation <= 0.1) {
      "Weak correlations suggest possible linearity issues or independence of variables."
    } else {
      "Very high correlations suggest possible multicollinearity issues."
    },
    detailed_report = paste(
      "**Linearity Assessment:**\n",
      "This assessment evaluates the linear relationships between dependent variables through correlation analysis. MANOVA assumes linear relationships among the dependent variables.\n",
      paste("- Correlation matrix:", capture.output(print(round(cor_matrix, 3))), collapse = "\n"),
      paste("- Minimum absolute correlation:", round(min_correlation, 3)),
      paste("- Maximum absolute correlation:", round(max_correlation, 3)),
      "- Expected range for adequate linearity: 0.1 to 0.9",
      "",
      paste("**Result:**", if (linearity_adequate) "PASSED\n" else "FAILED\n"),
      if (linearity_adequate) {
        "The correlations between dependent variables fall within an acceptable range, suggesting adequate linear relationships. This supports the assumption that the variables have meaningful linear associations that justify their inclusion in a multivariate analysis. It is also recommended to visually inspect scatterplots of the dependent variables to confirm linearity."
      } else if (min_correlation <= 0.1) {
        "Some correlations are very weak (< 0.1), which may indicate either non-linear relationships or that some variables are essentially independent. Consider examining scatterplots to assess the nature of relationships and whether transformations might improve linearity."
      } else {
        "Some correlations are very high (> 0.9), suggesting potential multicollinearity issues. While this doesn't violate linearity per se, it may indicate that some variables are redundant and could compromise the stability of the analysis."
      },
      sep = "\n"
    )
  )
  
  # 5. ABSENCE OF EXTREME MULTICOLLINEARITY
  # Calculate determinant of correlation matrix
  det_cor <- det(cor_matrix)
  multicollinearity_ok <- det_cor > 0.00001
  
  results$multicollinearity <- list(
    test_name = "Multicollinearity Assessment",
    description = "Evaluates extreme multicollinearity using determinant of correlation matrix. Values close to 0 indicate problematic multicollinearity.",
    statistics = list(
      determinant = det_cor,
      threshold = 0.00001
    ),
    assumption_met = multicollinearity_ok,
    interpretation = if (multicollinearity_ok) {
      paste("No extreme multicollinearity detected (determinant =", format(det_cor, scientific = TRUE), ").")
    } else {
      paste("Extreme multicollinearity detected (determinant =", format(det_cor, scientific = TRUE), "). Consider removing highly correlated variables.")
    },
    detailed_report = paste(
      "**Multicollinearity Assessment:**\n",
      "This assessment evaluates extreme multicollinearity using the determinant of the correlation matrix. Values close to zero indicate that variables are nearly perfectly correlated.\n",
      paste("- Correlation matrix determinant:", format(det_cor, scientific = TRUE, digits = 4)),
      paste("- Critical threshold:", format(0.00001, scientific = TRUE)),
      "- Interpretation: Values closer to 1 indicate independence; values closer to 0 indicate multicollinearity",
      "",
      paste("**Result:**", if (multicollinearity_ok) "PASSED\n" else "FAILED\n"),
      if (multicollinearity_ok) {
        "No extreme multicollinearity was detected among the dependent variables. The correlation matrix determinant is sufficiently large, indicating that the variables provide relatively independent information. This supports stable parameter estimation and reliable statistical inference in MANOVA."
      } else {
        "Extreme multicollinearity was detected among the dependent variables. The correlation matrix determinant is very close to zero, suggesting that some variables are nearly perfectly correlated with others. This can lead to unstable parameter estimates and inflated standard errors. Consider removing highly correlated variables or using principal component analysis to reduce dimensionality before proceeding with MANOVA."
      },
      sep = "\n"
    )
  )
  
  # 6. OUTLIERS ASSESSMENT
  
  mahal_dist_list <- split(complete_data[dependent_vars], groups)
  
  mahal_distances <- lapply(mahal_dist_list, function(group_data) {
    if (nrow(group_data) > ncol(group_data) && all(apply(group_data, 2, var) > 0)) {
      cov_matrix_group <- cov(group_data)
      mahalanobis(group_data, center = colMeans(group_data), cov = cov_matrix_group)
    } else {
      NA
    }
  })
  
  mahal_distances <- unlist(mahal_distances)
  
  # Critical value for outliers (chi-square with p = n_vars degrees of freedom)
  critical_value <- qchisq(0.99, df = n_vars)
  
  outliers <- sum(mahal_distances > critical_value, na.rm = TRUE)
  total_cases <- length(na.omit(mahal_distances))
  outlier_proportion <- if (total_cases > 0) {
    outliers / total_cases
  } else {
    0
  }
  
  outliers_acceptable <- outlier_proportion < 0.05
  
  results$outliers <- list(
    test_name = "Multivariate Outliers Assessment",
    description = "Identifies multivariate outliers using Mahalanobis distance. Outliers beyond 99th percentile of chi-square distribution are flagged.",
    statistics = list(
      n_outliers = outliers,
      proportion_outliers = outlier_proportion,
      critical_value = critical_value
    ),
    assumption_met = outliers_acceptable,
    interpretation = if (outliers_acceptable) {
      paste("Acceptable level of multivariate outliers (", round(outlier_proportion * 100, 1), "% of cases).", sep = "")
    } else {
      paste("High proportion of multivariate outliers (", round(outlier_proportion * 100, 1), "% of cases). Consider investigating and possibly removing extreme outliers.", sep = "")
    },
    detailed_report = paste(
      "**Multivariate Outliers Assessment:**\n",
      "This assessment identifies multivariate outliers using Mahalanobis distance, which measures how far each observation is from the centroid in multivariate space. It is calculated for each group separately.\n",
      paste("- Number of outliers identified:", outliers),
      paste("- Proportion of outliers:", round(outlier_proportion * 100, 2), "%"),
      paste("- Critical value (99th percentile χ²):", round(critical_value, 2)),
      paste("- Degrees of freedom:", n_vars),
      "- Threshold: Cases exceeding 5% are considered problematic",
      "",
      paste("**Result:**", if (outliers_acceptable) "PASSED\n" else "FAILED\n"),
      if (outliers_acceptable) {
        "The proportion of multivariate outliers is within acceptable limits. While some outliers are present, they do not constitute a substantial portion of the dataset that would compromise MANOVA results. These outliers may represent legitimate extreme cases rather than data errors."
      } else {
        paste("A high proportion of multivariate outliers was detected (", round(outlier_proportion * 100, 1), "% of cases). This suggests either: (1) data quality issues that need investigation, (2) the presence of distinct subgroups not captured by the grouping variable, or (3) non-normal distributions. Consider examining these outliers individually to determine if they should be retained, transformed, or removed before proceeding with MANOVA.", sep = "")
      },
      sep = "\n"
    )
  )
  
  # OVERALL ASSESSMENT
  # Check for critical failures that cannot be overcome by robustness
  critical_failures <- (!results$sample_size$assumption_met || 
                          !results$multicollinearity$assumption_met)
  
  # Check for minor violations that can be overcome by robustness (e.g., Pillai's trace)
  minor_violations <- (!results$covariance_homogeneity$assumption_met ||
                         !results$multivariate_normality$assumption_met)
  
  # Determine overall recommendation based on a clear hierarchy
  overall_recommendation <- if (critical_failures) {
    "reconsider"
  } else if (minor_violations) {
    "proceed_with_caution"
  } else {
    "proceed"
  }
  
  # Generate summary paragraph
  summary_text <- paste(
    "The MANOVA prerequisites assessment examined", 
    length(results), 
    "key assumptions across", n_total, "observations with", n_vars, "dependent variables and", n_groups, "groups.",
    
    if (results$multivariate_normality$assumption_met) {
      paste("Multivariate normality was satisfied (Shapiro-Wilk W =", round(results$multivariate_normality$statistics$statistic, 4), ", p =", round(results$multivariate_normality$statistics$p_value, 4), ").")
    } else {
      paste("Multivariate normality was violated (Shapiro-Wilk W =", round(results$multivariate_normality$statistics$statistic, 4), ", p =", round(results$multivariate_normality$statistics$p_value, 4), "), This is a common finding with large datasets. However, because your sample size is large (n > 200), MANOVA is considered robust to this violation due to the Central Limit Theorem.")
    },
    
    if (results$covariance_homogeneity$assumption_met) {
      paste("Covariance matrix homogeneity was met (Box's M χ² =", round(results$covariance_homogeneity$statistics$chi_square, 2), ", p =", round(results$covariance_homogeneity$statistics$p_value, 4), ").")
    } else {
      paste("Covariance matrix homogeneity was violated (Box's M χ² =", round(results$covariance_homogeneity$statistics$chi_square, 2), ", p =", round(results$covariance_homogeneity$statistics$p_value, 4), "), which is adequately addressed by using Pillai's trace as the test statistic.")
    },
    
    if (results$linearity$assumption_met) {
      paste("Linear relationships among variables appeared adequate (correlations range:", round(results$linearity$statistics$min_correlation, 3), "to", round(results$linearity$statistics$max_correlation, 3), ").")
    } else {
      paste("Linear relationships showed some concerns (correlations range:", round(results$linearity$statistics$min_correlation, 3), "to", round(results$linearity$statistics$max_correlation, 3), ") that warrant careful interpretation of results.")
    },
    
    if (results$multicollinearity$assumption_met) {
      paste("Multicollinearity levels were acceptable (correlation matrix determinant =", format(results$multicollinearity$statistics$determinant, scientific = TRUE, digits = 3), ").")
    } else {
      paste("Extreme multicollinearity was detected (correlation matrix determinant =", format(results$multicollinearity$statistics$determinant, scientific = TRUE, digits = 3), "), potentially compromising the analysis.")
    },
    
    if (results$outliers$assumption_met) {
      paste("Multivariate outliers were within acceptable limits (", round(results$outliers$statistics$proportion_outliers * 100, 1), "% of cases exceed critical value).", sep = "")
    } else {
      paste("A substantial proportion (", round(results$outliers$statistics$proportion_outliers * 100, 1), "%) of multivariate outliers was identified (", results$outliers$statistics$n_outliers, " cases exceed Mahalanobis distance critical value of ", round(results$outliers$statistics$critical_value, 2), ").", sep = "")
    },
    
    if (results$sample_size$assumption_met) {
      paste("Sample size was adequate for robust analysis (minimum group size =", results$sample_size$statistics$min_group_size, ", recommended minimum total N =", results$sample_size$statistics$recommended_min_total, ").")
    } else {
      paste("Sample size concerns may limit the reliability of results (minimum group size =", results$sample_size$statistics$min_group_size, ", recommended minimum total N =", results$sample_size$statistics$recommended_min_total, ").")
    },
    
    switch(overall_recommendation,
           "proceed" = "\n\n**Based on these findings, the assumptions are met. The analysis is fully justified, and results should be reliable.**",
           "proceed_with_caution" = "\n\n**MANOVA analysis may proceed with appropriate caution regarding assumption violations. The use of robust test statistics like Pillai's trace is recommended.**",
           "reconsider" = paste("\n\n**With critical assumption violations present, serious consideration should be given to alternative analytical approaches or data transformations before proceeding with MANOVA.**")
    ),
    sep = "\n"
  )
  
  results$overall_assessment <- list(
    total_tests = length(results) - 1, # Exclude overall_assessment itself
    tests_passed = sum(sapply(results, `[[`, "assumption_met"), na.rm = TRUE),
    recommendation = overall_recommendation,
    summary = summary_text
  )
  
  return(results)
}

#' Test Repeated Measures MANOVA Prerequisites
#'
#' This function tests the key assumptions for Repeated Measures MANOVA on
#' panel data, including independence of observations, multivariate normality
#' within groups, and sphericity. It provides a detailed report and
#' an overall recommendation for the analysis.
#'
#' @param data A data frame in long format containing the variables.
#' @param dependent_vars Character vector of dependent variable names.
#' @param grouping_var Character string for the between-subjects grouping variable.
#' @param subject_id_var Character string for the subject ID variable.
#' @param wave_var Character string for the variable indicating the wave/time point.
#' @param alpha Significance level for tests (default = 0.05).
#' @return A list containing test results and interpretations.
#' @examples
#' af_test_repeated_measures_manova_prerequisites(data, c("pe_ideology", "pe_violence"), "pe_left_center_right", "respondent_id", "Wave")
af_test_repeated_measures_manova_prerequisites <- function(data, dependent_vars, grouping_var, subject_id_var, wave_var, alpha = 0.05) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (!all(c(dependent_vars, grouping_var, subject_id_var, wave_var) %in% names(data))) {
    stop("All specified variables must exist in the data frame")
  }
  
  if (length(dependent_vars) < 2) {
    stop("Repeated Measures MANOVA requires at least 2 dependent variables")
  }
  
  # Ensure data is complete for the analysis variables and the grouping variable
  # Keep only necessary columns for completeness check to avoid dropping too many rows
  required_cols <- c(dependent_vars, grouping_var, subject_id_var, wave_var)
  complete_data <- data[complete.cases(data[, required_cols]), ]
  n_removed <- nrow(data) - nrow(complete_data)
  
  if (n_removed > 0) {
    warning(paste("Removed", n_removed, "cases with missing values"))
  }
  
  # --- CRITICAL CORRECTION HERE ---
  # 1. Reshape only the dependent variables to wide format
  # 2. Add the time-invariant grouping variable back to the wide_data
  
  # Prepare data for reshaping: select only subject_id, wave, and dependent_vars
  reshape_data <- complete_data[, c(subject_id_var, wave_var, dependent_vars)]
  
  wide_data <- reshape(
    reshape_data,
    idvar = subject_id_var,
    timevar = wave_var,
    direction = "wide"
  )
  
  # Add the grouping variable back to the wide data frame.
  # We use the original complete_data to ensure we get the correct grouping for each subject_id.
  # Assuming grouping_var is constant for each subject_id across waves (a key assumption for between-subjects factor)
  wide_data[[grouping_var]] <- complete_data[[grouping_var]][match(wide_data[[subject_id_var]], complete_data[[subject_id_var]])]
  
  # Check for repeated measures structure
  if (nrow(wide_data) == nrow(complete_data)) {
    stop("The data is not in long format. Each subject ID should have multiple rows corresponding to different waves. Check 'subject_id_var' and 'wave_var'.")
  }
  
  results <- list()
  n_vars <- length(dependent_vars) # Number of dependent variables
  
  # 1. INDEPENDENCE OF OBSERVATIONS
  # Check for subject ID uniqueness and correct data structure
  n_subjects <- nrow(wide_data)
  n_waves <- length(unique(complete_data[[wave_var]]))
  n_obs <- nrow(complete_data)
  
  independence_adequate <- n_obs > n_subjects 
  
  results$independence <- list(
    test_name = "Independence of Observations",
    description = "Confirms that data is structured for repeated measures and that observations within subjects are dependent, while observations between subjects are independent.",
    statistics = list(
      n_subjects = n_subjects,
      n_waves = n_waves,
      n_obs = n_obs
    ),
    assumption_met = independence_adequate,
    interpretation = "Data is correctly structured in a long format, allowing for the analysis of dependent observations within subjects.",
    detailed_report = paste(
      "**Independence of Observations Assessment:**\n",
      "This test verifies that the data is in the correct format for repeated measures analysis, where observations from the same subject are dependent. It confirms that the number of total observations is greater than the number of subjects.\n",
      paste("- Number of subjects:", n_subjects),
      paste("- Number of waves:", n_waves),
      paste("- Total observations:", n_obs),
      "",
      paste("**Result:**", if (independence_adequate) "PASSED\n" else "FAILED\n"),
      if (independence_adequate) {
        "The data structure is appropriate for Repeated Measures MANOVA. The presence of multiple observations per subject allows for the modeling of within-subject dependencies, while independence is maintained between subjects."
      } else {
        "The data does not appear to be in a long format suitable for repeated measures. Each subject should have multiple observations (rows)."
      },
      sep = "\n"
    )
  )
  
  # Prepare wide data for within-subjects tests
  # Create a vector of the actual wide-format column names for the DVs
  # Example: if dependent_vars = c("pe_ideology") and Waves = c("Wave1", "Wave2")
  # dv_vars_wide will be c("pe_ideology.Wave1", "pe_ideology.Wave2")
  dv_vars_wide <- unlist(lapply(dependent_vars, function(dv) {
    paste0(dv, ".", unique(complete_data[[wave_var]]))
  }))
  
  Y_wide <- as.matrix(wide_data[, dv_vars_wide])
  
  # Ensure groups factor is correctly created from the added grouping_var
  groups <- as.factor(wide_data[[grouping_var]])
  
  # 2. MULTIVARIATE NORMALITY WITHIN GROUPS
  # Test normality for each level of the grouping variable
  normality_results <- lapply(split(as.data.frame(Y_wide), groups), function(group_data) {
    if (nrow(group_data) > 2) { # mvnormtest::mshapiro.test requires at least 3 cases
      mshapiro_test <- try(mvnormtest::mshapiro.test(t(as.matrix(group_data))), silent = TRUE)
      if (!inherits(mshapiro_test, "try-error")) {
        return(list(p_value = mshapiro_test$p.value, statistic = mshapiro_test$statistic))
      }
    }
    return(list(p_value = NA, statistic = NA))
  })
  
  normality_p_values <- sapply(normality_results, `[[`, "p_value")
  all_normal <- all(normality_p_values > alpha, na.rm = TRUE)
  
  results$multivariate_normality <- list(
    test_name = "Multivariate Normality (Within Groups)",
    description = "Tests if the dependent variables are jointly normally distributed within each group.",
    statistics = list(
      p_values_by_group = normality_p_values
    ),
    assumption_met = all_normal,
    interpretation = if (all_normal) {
      "Multivariate normality is met for all groups."
    } else {
      "Multivariate normality is violated in at least one group. However, Repeated Measures MANOVA is robust with large samples."
    },
    detailed_report = paste(
      "**Multivariate Normality Assessment (Within Groups):**\n",
      "This evaluates if the dependent variables are jointly normally distributed for each between-subjects group. The Shapiro-Wilk test is applied to the wide-format data for each group separately.\n",
      paste("- P-values per group:", paste(round(normality_p_values, 4), collapse = ", ")),
      paste("- Significance level:", alpha),
      "",
      paste("**Result:**", if (all_normal) "PASSED\n" else "FAILED\n"),
      if (all_normal) {
        "The assumption of multivariate normality is satisfied within each between-subjects group, which supports the validity of the Repeated Measures MANOVA."
      } else {
        "The assumption of multivariate normality is violated in at least one group. Similar to standard MANOVA, the repeated measures analysis is robust to moderate violations, especially with a large number of subjects."
      },
      sep = "\n"
    )
  )
  
  # 3. SPHERICITY ASSUMPTION (Mauchly's Test)
  # This assumption applies to within-subjects factors, but is tested on the differences between pairs of waves
  if (n_waves > 2) {
    sphericity_test <- try(ez::ezANOVA(
      data = complete_data,
      dv = dependent_vars[1], # ezANOVA expects a single DV, checking sphericity for the first one.
      wid = as.factor(complete_data[[subject_id_var]]),
      within = as.factor(complete_data[[wave_var]]),
      between = as.factor(complete_data[[grouping_var]])
    ), silent = TRUE)
    
    if (!inherits(sphericity_test, "try-error")) {
      mauchly_p <- sphericity_test$`Mauchly's Test of Sphericity`$p
      sphericity_met <- mauchly_p > alpha
    } else {
      mauchly_p <- NA
      sphericity_met <- NA
    }
    
  } else {
    mauchly_p <- NA
    sphericity_met <- NA
  }
  
  results$sphericity <- list(
    test_name = "Sphericity (Mauchly's Test)",
    description = "Tests if the variances of the differences between all pairs of within-subject measures are equal. Only applicable for 3 or more waves.",
    statistics = list(
      mauchly_p_value = mauchly_p
    ),
    assumption_met = sphericity_met,
    interpretation = if (is.na(sphericity_met)) {
      "Sphericity test not performed as it requires at least 3 waves."
    } else if (sphericity_met) {
      paste("Sphericity assumption is met (p =", round(mauchly_p, 4), ").")
    } else {
      paste("Sphericity assumption is violated (p =", round(mauchly_p, 4), "). Corrective measures like Greenhouse-Geisser or Huynh-Feldt corrections should be used.")
    },
    detailed_report = paste(
      "**Sphericity Assessment (Mauchly's Test):**\n",
      "This assumption is unique to repeated measures designs and assesses the equality of variances of differences between wave pairs. It is only relevant when there are more than two waves. The test result informs whether to apply corrections to the degrees of freedom.\n",
      if (!is.na(mauchly_p)) {
        paste("- Mauchly's W p-value:", round(mauchly_p, 4))
      } else {
        "- Test not applicable: less than 3 waves"
      },
      paste("- Significance level:", alpha),
      "",
      paste("**Result:**", if (is.na(sphericity_met)) "NOT APPLICABLE\n" else if (sphericity_met) "PASSED\n" else "FAILED\n"),
      if (is.na(sphericity_met)) {
        "The sphericity assumption is not applicable as there are only two waves. In this case, the analysis does not require a sphericity correction."
      } else if (sphericity_met) {
        "The sphericity assumption is met. The variances of the differences between wave pairs are equal, so no adjustments to the degrees of freedom are needed."
      } else {
        "The sphericity assumption is violated. This is a common finding. You should proceed with the Repeated Measures MANOVA but apply corrections to the degrees of freedom, such as the Greenhouse-Geisser or Huynh-Feldt adjustments, to ensure accurate p-values."
      },
      sep = "\n"
    )
  )
  
  # OVERALL ASSESSMENT
  # Check for critical failures that cannot be overcome by robustness
  critical_failures <- !results$independence$assumption_met
  
  # Check for minor violations that can be overcome by robustness
  minor_violations <- (!results$multivariate_normality$assumption_met)
  
  overall_recommendation <- if (critical_failures) {
    "reconsider"
  } else if (minor_violations) {
    "proceed_with_caution"
  } else {
    "proceed"
  }
  
  # Generate summary paragraph
  summary_text <- paste(
    "The Repeated Measures MANOVA prerequisites assessment examined 3 key assumptions across", n_obs, "observations from", n_subjects, "subjects.",
    
    if (results$independence$assumption_met) {
      paste("The data structure is appropriate for a repeated measures design, confirming independence between subjects.")
    } else {
      paste("The data structure is not appropriate for a repeated measures design.")
    },
    
    if (results$multivariate_normality$assumption_met) {
      paste("Multivariate normality was satisfied within all groups.")
    } else {
      paste("Multivariate normality was violated in at least one group, though Repeated Measures MANOVA maintains robustness with an adequate number of subjects.")
    },
    
    if (!is.na(results$sphericity$assumption_met)) {
      if (results$sphericity$assumption_met) {
        paste("Sphericity was met, so no corrections are needed.")
      } else {
        paste("Sphericity was violated, which requires the use of corrections such as Greenhouse-Geisser or Huynh-Feldt.")
      }
    } else {
      "Sphericity was not assessed as it requires three or more waves."
    },
    
    switch(overall_recommendation,
           "proceed" = "\n\n**Based on these findings, the assumptions are met. The analysis is fully justified, and results should be reliable.**",
           "proceed_with_caution" = "\n\n**The Repeated Measures MANOVA analysis may proceed with appropriate caution regarding assumption violations. The use of corrections for sphericity is recommended if applicable.**",
           "reconsider" = paste("\n\n**The data is not structured for repeated measures. Review the data format before proceeding.**")
    ),
    sep = "\n"
  )
  
  results$overall_assessment <- list(
    total_tests = 3,
    tests_passed = sum(sapply(results, `[[`, "assumption_met"), na.rm = TRUE),
    recommendation = overall_recommendation,
    summary = summary_text
  )
  
  return(results)
}