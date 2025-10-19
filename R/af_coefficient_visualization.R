# # Default layout (variables as columns)
# coef_table <- af_coef_and_ci_table(models, c("wt", "hp", "am1"))
# plot1 <- af_coef_and_ci_plot(coef_table)
# 
# # Transposed layout (models as columns) 
# coef_table_xpose <- af_coef_and_ci_table(models, c("wt", "hp", "am1"), xpose = TRUE)
# plot2 <- af_coef_and_ci_plot(coef_table_xpose, xpose = TRUE)


#' Extract coefficient, confidence interval, and p-value for a specified covariate
#'
#' This function extracts the coefficient estimate, confidence interval bounds,
#' and p-value for a specified covariate from a fitted model object.
#' Supports both lm and plm (panel model) objects.
#'
#' @param m A fitted model object (e.g., lm, glm, plm)
#' @param model_name Character string specifying the name of the model
#' @param cov_name Character string specifying the name of the covariate
#'
#' @return A list containing model_name, cov_name, coef, conf1, conf2, pvalue, and stars
#'
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' af_get_coef_and_ci(model, "Model1", "wt")
#'
#' @export
af_get_coef_and_ci <- function(m, model_name, cov_name) {
  # Input validation
  if (missing(m) || is.null(m)) {
    stop("Model object 'm' is required and cannot be NULL")
  }
  if (missing(model_name) || !is.character(model_name) || length(model_name) != 1) {
    stop("'model_name' must be a single character string")
  }
  if (missing(cov_name) || !is.character(cov_name) || length(cov_name) != 1) {
    stop("'cov_name' must be a single character string")
  }
  
  # Check if covariate exists in model
  if (!cov_name %in% names(coef(m))) {
    stop(paste("Covariate", cov_name, "not found in model"))
  }
  
  # Extract the coefficient for the specified covariate
  coef_val <- coef(m)[cov_name]
  
  # Handle confidence intervals based on model type
  tryCatch({
    conf_interval <- confint.default(m)[cov_name, ] # Wald method  more efficient than profile likelihood 
    conf1 <- conf_interval[1]
    conf2 <- conf_interval[2]
  }, error = function(e) {
    warning(paste("Could not extract confidence interval for", cov_name, "- setting to NA"))
    conf1 <<- NA
    conf2 <<- NA
  })
  
  # Extract p-value based on model type
  model_summary <- summary(m)
  
  # Handle different coefficient matrix structures
  if (inherits(m, "plm")) {
    # For plm models, check if the column exists
    coef_matrix <- model_summary$coefficients
    if ("Pr(>|t|)" %in% colnames(coef_matrix)) {
      pvalue <- coef_matrix[cov_name, "Pr(>|t|)"]
    } else if ("Pr(>|z|)" %in% colnames(coef_matrix)) {
      pvalue <- coef_matrix[cov_name, "Pr(>|z|)"]
    } else {
      warning(paste("Could not find p-value column for plm model - setting to NA"))
      pvalue <- NA
    }
  } else {
    # # For lm/glm models
    # pvalue <- model_summary$coefficients[cov_name, "Pr(>|t|)"]
    # For lm/glm models - check which p-value column exists
    coef_matrix <- model_summary$coefficients
    if ("Pr(>|z|)" %in% colnames(coef_matrix)) {
      pvalue <- coef_matrix[cov_name, "Pr(>|z|)"]
    } else if ("Pr(>|t|)" %in% colnames(coef_matrix)) {
      pvalue <- coef_matrix[cov_name, "Pr(>|t|)"]
    } else {
      warning(paste("Could not find p-value column for model - setting to NA"))
      pvalue <- NA
    }
  }
  
  # Generate star ranking
  stars <- ""
  if (!is.na(pvalue)) {
    if (pvalue < 0.001) {
      stars <- "***"
    } else if (pvalue < 0.01) {
      stars <- "**"
    } else if (pvalue < 0.05) {
      stars <- "*"
    }
  }
  
  # Create a list with the required information
  result_list <- list(model_name = model_name,
                      cov_name = cov_name,
                      coef = coef_val,
                      conf1 = conf1,
                      conf2 = conf2,
                      pvalue = pvalue,
                      stars = stars)
  
  return(result_list)
}


#' Create coefficient and confidence interval table for multiple variables
#'
#' This function creates a comprehensive table with coefficients, confidence intervals,
#' p-values, and significance stars for multiple covariates across multiple models.
#'
#' @param models A named list of fitted model objects
#' @param cov_names A character vector of covariate names to extract, or a named vector
#'   where names are coefficient names and values are display names
#' @param xpose Logical indicating whether to transpose the output (models as columns, variables as rows)
#'
#' @return A data frame with columns: model_name, cov_name, coef, conf1, conf2, pvalue, stars
#'         If xpose = TRUE, the layout is transposed for easier comparison across models
#'
#' @examples
#' model1 <- lm(mpg ~ wt + hp, data = mtcars)
#' model2 <- lm(mpg ~ wt + hp + cyl, data = mtcars)
#' models <- list("Model1" = model1, "Model2" = model2)
#' af_coef_and_ci_table(models, c("wt", "hp"))
#' # With display names:
#' cov_names <- c("wt" = "Weight", "hp" = "Horsepower")
#' af_coef_and_ci_table(models, cov_names)
#'
#' @export
af_coef_and_ci_table <- function(models, cov_names, xpose = FALSE) {
  # Input validation
  if (missing(models) || !is.list(models) || length(models) == 0) {
    stop("'models' must be a non-empty list of model objects")
  }
  if (is.null(names(models)) || any(names(models) == "")) {
    stop("'models' list must have named elements")
  }
  if (missing(cov_names) || !is.character(cov_names) || length(cov_names) == 0) {
    stop("'cov_names' must be a non-empty character vector")
  }
  if (!is.logical(xpose) || length(xpose) != 1) {
    stop("'xpose' must be a single logical value")
  }
  
  # Handle named vs unnamed cov_names
  if (is.null(names(cov_names))) {
    # If no names provided, use the values as both coefficient names and display names
    actual_cov_names <- cov_names
    display_names <- cov_names
  } else {
    # If names provided, names are coefficient names, values are display names
    actual_cov_names <- names(cov_names)
    display_names <- as.character(cov_names)
  }
  
  coef_ci_table <- NULL
  
  # Determine the order based on xpose parameter
  if (xpose) {
    # When xpose = TRUE: group by models first, then variables
    # This creates: Model A - var1, Model A - var2, Model B - var1, Model B - var2, etc.
    for (i in 1:length(models)) {
      m <- models[[i]]
      model_name <- names(models[i])
      
      for (j in seq_along(actual_cov_names)) {
        actual_cov_name <- actual_cov_names[j]
        display_name <- display_names[j]
        
        # Check if covariate exists in this model
        if (actual_cov_name %in% names(coef(m))) {
          coef_ci_line <- unlist(af_get_coef_and_ci(m, model_name, actual_cov_name))
          # Replace the coefficient name with display name
          coef_ci_line[2] <- display_name
          names(coef_ci_line) <- NULL
          coef_ci_table <- rbind(coef_ci_table, coef_ci_line)
        } else {
          # Add row with NA values if covariate not in model
          coef_ci_line <- c(model_name, display_name, NA, NA, NA, NA, "")
          coef_ci_table <- rbind(coef_ci_table, coef_ci_line)
        }
      }
    }
  } else {
    # Original order: group by variables first, then models
    # This creates: var1 - Model A, var1 - Model B, var2 - Model A, var2 - Model B, etc.
    for (j in seq_along(actual_cov_names)) {
      actual_cov_name <- actual_cov_names[j]
      display_name <- display_names[j]
      
      # Loop through each model for this covariate
      for (i in 1:length(models)) {
        m <- models[[i]]
        model_name <- names(models[i])
        
        # Check if covariate exists in this model
        if (actual_cov_name %in% names(coef(m))) {
          coef_ci_line <- unlist(af_get_coef_and_ci(m, model_name, actual_cov_name))
          # Replace the coefficient name with display name
          coef_ci_line[2] <- display_name
          names(coef_ci_line) <- NULL
          coef_ci_table <- rbind(coef_ci_table, coef_ci_line)
        } else {
          # Add row with NA values if covariate not in model
          coef_ci_line <- c(model_name, display_name, NA, NA, NA, NA, "")
          coef_ci_table <- rbind(coef_ci_table, coef_ci_line)
        }
      }
    }
  }
  
  rownames(coef_ci_table) <- NULL
  colnames(coef_ci_table) <- c("model_name", "cov_name", "coef", "conf1", "conf2", "pvalue", "stars")
  
  # Convert to data frame and ensure proper data types
  result_df <- as.data.frame(coef_ci_table, stringsAsFactors = FALSE)
  result_df$coef <- as.numeric(result_df$coef)
  result_df$conf1 <- as.numeric(result_df$conf1)
  result_df$conf2 <- as.numeric(result_df$conf2)
  result_df$pvalue <- as.numeric(result_df$pvalue)
  
  return(result_df)
}

#' Create coefficient plots for multiple variables
#'
#' This function creates separate coefficient plots for each variable and combines
#' them horizontally into a single plot with aligned y-axes and model names
#' appearing only once on the left side.
#'
#' @param coef_ci_table A data frame created by af_coef_and_ci_table
#' @param xpose Logical indicating whether to transpose the layout (models as columns, variables as rows)
#' @param use_patchwork Logical indicating whether to use patchwork (TRUE) or gridExtra (FALSE) for combining plots
#' @param label_width Maximum width in characters for y-axis labels before wrapping (default: 20)
#' @param label_size Font size for y-axis labels (default: 12)
#' @param x_breaks Number of breaks (tick marks) on x-axis (default: 3)
#' @param title Optional main title for the combined plot (default: NULL)
#' @param exp_ctrl A logical vector indicating which models should have exponentiated coefficients.
#'   Length must match number of unique models. Can be named with model names. (default: NULL)
#'
#' @return A combined plot object showing coefficient plots for each variable
#'
#' @examples
#' # Assuming coef_table is created from af_coef_and_ci_table
#' af_coef_and_ci_plot(coef_table)
#' af_coef_and_ci_plot(coef_table, xpose = TRUE)
#' af_coef_and_ci_plot(coef_table, label_width = 15, label_size = 14)
#' # With exponentiation for models 1 and 3
#' exp_ctrl <- c(TRUE, FALSE, TRUE)
#' af_coef_and_ci_plot(coef_table, exp_ctrl = exp_ctrl)
#'
#' @export
af_coef_and_ci_plot <- function(coef_ci_table, xpose = FALSE, use_patchwork = TRUE, 
                                label_width = 20, label_size = 12, x_breaks = 3, title = NULL, exp_ctrl = NULL) {
  # Input validation
  if (missing(coef_ci_table) || !is.data.frame(coef_ci_table)) {
    stop("'coef_ci_table' must be a data frame")
  }
  
  if (!is.null(title) && (!is.character(title) || length(title) != 1)) {
    stop("'title' must be NULL or a single character string")
  }
  
  required_cols <- c("model_name", "cov_name", "coef", "conf1", "conf2", "stars")
  if (!all(required_cols %in% names(coef_ci_table))) {
    stop(paste("'coef_ci_table' must contain columns:", paste(required_cols, collapse = ", ")))
  }
  
  if (!is.logical(xpose) || length(xpose) != 1) {
    stop("'xpose' must be a single logical value")
  }
  if (!is.logical(use_patchwork) || length(use_patchwork) != 1) {
    stop("'use_patchwork' must be a single logical value")
  }
  if (!is.numeric(label_width) || length(label_width) != 1 || label_width <= 0) {
    stop("'label_width' must be a single positive numeric value")
  }
  if (!is.numeric(label_size) || length(label_size) != 1 || label_size <= 0) {
    stop("'label_size' must be a single positive numeric value")
  }
  if (!is.numeric(x_breaks) || length(x_breaks) != 1 || x_breaks <= 0 || x_breaks != round(x_breaks)) {
    stop("'x_breaks' must be a single positive integer value")
  }
  
  # Handle exp_ctrl parameter
  if (!is.null(exp_ctrl)) {
    unique_models <- unique(coef_ci_table$model_name)
    if (!is.logical(exp_ctrl) || length(exp_ctrl) != length(unique_models)) {
      stop("'exp_ctrl' must be a logical vector with length equal to number of unique models")
    }
    if (is.null(names(exp_ctrl))) {
      names(exp_ctrl) <- unique_models
    } else if (!all(names(exp_ctrl) %in% unique_models)) {
      stop("Names in 'exp_ctrl' must match model names in the table")
    }
  }
  
  # Load required libraries
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required but not installed")
  }
  
  # Make a copy of the table
  plot_table <- coef_ci_table
  
  # Apply exponentiation if specified (BEFORE transposing)
  if (!is.null(exp_ctrl)) {
    for (model in names(exp_ctrl)) {
      if (exp_ctrl[model]) {
        model_rows <- plot_table$model_name == model & !is.na(plot_table$coef)
        plot_table$coef[model_rows] <- exp(plot_table$coef[model_rows])
        plot_table$conf1[model_rows] <- exp(plot_table$conf1[model_rows])
        plot_table$conf2[model_rows] <- exp(plot_table$conf2[model_rows])
      }
    }
    
    # Create exponentiation note
    exp_models <- names(exp_ctrl)[exp_ctrl]
    if (length(exp_models) > 0) {
      if (all(exp_ctrl)) {
        exp_note <- "All coefficients are exponentiated."
      } else {
        model_indices <- which(names(exp_ctrl) %in% exp_models)
        if (length(model_indices) == 1) {
          exp_note <- paste0("Coefficients from model ", model_indices[1], " are exponentiated.")
        } else if (length(model_indices) == 2) {
          exp_note <- paste0("Coefficients from models ", paste(model_indices, collapse = " and "), " are exponentiated.")
        } else {
          exp_note <- paste0("Coefficients from models ", 
                             paste(model_indices[-length(model_indices)], collapse = ", "), 
                             ", and ", model_indices[length(model_indices)], " are exponentiated.")
        }
      }
    } else {
      exp_note <- NULL
    }
  } else {
    exp_note <- NULL
  }
  
  # Transpose data if xpose = TRUE (AFTER exponentiation)
  if (xpose) {
    # Swap model_name and cov_name columns
    temp_col <- plot_table$model_name
    plot_table$model_name <- plot_table$cov_name
    plot_table$cov_name <- temp_col
  }
  
  # Get unique covariate names and model names from potentially transposed data
  unique_covs <- unique(plot_table$cov_name)
  all_models <- unique(plot_table$model_name)
  
  # Wrap model names for better display
  all_models_wrapped <- stringr::str_wrap(all_models, width = label_width)
  names(all_models_wrapped) <- all_models
  
  # Create a list to store individual plots
  plot_list <- list()
  
  # Create a plot for each covariate
  for (i in seq_along(unique_covs)) {
    cov <- unique_covs[i]
    
    # Filter data for this covariate and remove NA rows
    plot_data <- plot_table[plot_table$cov_name == cov & !is.na(plot_table$coef), ]
    
    if (nrow(plot_data) == 0) {
      warning(paste("No valid data for covariate:", cov))
      next
    }
    
    # Ensure consistent factor levels for model_name across all plots
    plot_data$model_name <- factor(plot_data$model_name, levels = rev(all_models))
    
    # Create wrapped labels for y-axis
    plot_data$model_name_wrapped <- factor(
      all_models_wrapped[as.character(plot_data$model_name)],
      levels = rev(all_models_wrapped)
    )
    
    ref_line_x <- if (!is.null(exp_ctrl) && any(exp_ctrl)) 1 else 0
    
    # Create the plot for this covariate
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = coef, y = model_name_wrapped)) +
      ggplot2::geom_vline(xintercept = ref_line_x, linetype = "dashed", color = "black") +
      ggplot2::geom_segment(ggplot2::aes(xend = conf1, yend = model_name_wrapped), color = "black") +
      ggplot2::geom_segment(ggplot2::aes(xend = conf2, yend = model_name_wrapped), color = "black") +
      ggplot2::geom_point(ggplot2::aes(x = coef), shape = 15, size = 3, color = "black") +
      ggplot2::geom_text(ggplot2::aes(label = paste0(sprintf("%.2f", coef), stars)), 
                         nudge_y = 0.4, size = 3) +
      ggplot2::labs(x = "", title = cov) +
      ggplot2::scale_x_continuous(labels = scales::number_format(digits = 2), n.breaks = x_breaks) +
      ggplot2::scale_y_discrete(drop = FALSE) +  # Keep all factor levels even if not present
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    
    # Add padding to x-axis to prevent coefficients from being cut off
    x_range <- range(c(plot_data$coef, plot_data$conf1, plot_data$conf2), na.rm = TRUE)
    x_padding <- diff(x_range) * 0.1  # 10% padding
    p <- p + ggplot2::xlim(x_range[1] - x_padding, x_range[2] + x_padding)
    
    # For all plots except the first one, remove y-axis labels and text
    # Add full border to all plots for consistent appearance
    if (i == 1) {
      p <- p + 
        ggplot2::labs(y = "") +
        ggplot2::theme(
          panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
          axis.text.y = ggplot2::element_text(size = label_size, hjust = 1),
          plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 10, "pt")  # Add more left margin for wrapped text
        )
    } else {
      p <- p + 
        ggplot2::labs(y = "") +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
          plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5, "pt")
        )
    }
    
    plot_list[[cov]] <- p
  }
  
  # Combine plots
  if (length(plot_list) == 1) {
    final_plot <- plot_list[[1]]
    if (!is.null(exp_note)) {
      final_plot <- final_plot + ggplot2::labs(caption = exp_note) +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 14))
    }
    return(final_plot)
  } else if (length(plot_list) > 1) {
    if (use_patchwork) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        warning("Package 'patchwork' not available, using gridExtra instead")
        use_patchwork <- FALSE
      }
    }
    
    if (use_patchwork) {
      # Combine plots with equal widths and aligned y-axes
      combined_plot <- Reduce(`+`, plot_list) + 
        patchwork::plot_layout(ncol = length(plot_list), guides = "collect")
      if (!is.null(title)) {
        combined_plot <- combined_plot + patchwork::plot_annotation(title = title)
      }
      if (!is.null(exp_note)) {
        combined_plot <- combined_plot + patchwork::plot_annotation(
          caption = exp_note,
          theme = ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 14))
        )
      }
      return(combined_plot)
    } else {
      if (!requireNamespace("gridExtra", quietly = TRUE)) {
        stop("Either 'patchwork' or 'gridExtra' package is required for combining plots")
      }
      
      # Create the main title and caption for gridExtra
      main_title <- title
      if (!is.null(exp_note)) {
        if (is.null(main_title)) {
          main_title <- exp_note
        } else {
          main_title <- paste(main_title, exp_note, sep = "\n")
        }
      }
      
      combined_plot <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = length(plot_list),
                                                          top = main_title))
      return(combined_plot)
    }
  } else {
    stop("No valid plots could be created")
  }
}