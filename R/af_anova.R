# Required packages
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(afex)

#' Analyze Extremism Changes Across Wave Pairs and Population Groups
#'
#' This function performs mixed-model factorial ANOVA to compare extremism levels
#' between consecutive survey waves across different population groups. For panel
#' waves (pairs 3 & 4), it uses repeated measures ANOVA with matched respondent IDs.
#' Additional categorical control variables (like gender, age_group) can be included as factors.
#'
#' @param df A data frame containing the survey data with respondent_id column
#' @param extremism_var Character string. Name of the extremism variable
#' @param group_var Character string. Name of the population group variable (3 levels expected). If NULL, analyzes overall change without groups.
#' @param wave_var Character string. Name of the wave variable (default: "Wave")
#' @param respondent_id_var Character string. Name of respondent ID variable (default: "respondent_id")
#' @param control_vars Character vector. Names of additional categorical control variables to include as factors (default: NULL)
#' @param panel_pairs Numeric vector. Which pairs are panel waves (default: c(3, 4))
#' @param alpha Numeric. Significance level for statistical tests (default: 0.05)
#' @param plot_title Character string. Title for the plot (default: auto-generated)
#' @param event_names Character vector. Names of events for each pair (default: NULL)
#' @param line_width Numeric. Width of lines in plot (default: 0.8)
#' @param point_size Numeric. Size of points in plot (default: 3)
#' @param plots_per_row Numeric. Number of plots per row in facet wrap (default: 3)
#' @param use_bw Logical. Use black and white plot with different line types (default: FALSE)
#' @param color_palette Character. Color palette to use: "viridis", "default" (default: "viridis")
#' @param show_sig Logical. Add significance indicators to plot (default: FALSE)
#' 
#' @return A list containing ANOVA results, descriptive statistics, and a ggplot object
#' 
#' @examples
#' # Basic usage
#' # results <- af_analyze_extremism_waves(df, "extremism_score", "population_group")
#' 
#' # With categorical control variables
#' # results <- af_analyze_extremism_waves(df, "extremism_score", "population_group",
#' #                                       control_vars = c("gender", "age_group"))
#' # 
#' # With custom plot parameters
#' # results <- af_analyze_extremism_waves(df, "extremism_score", "population_group",
#' #                                       event_names = c("Event1", "Event2", "Event3", "Event4", "Event5"),
#' #                                       plots_per_row = 5, use_bw = TRUE, line_width = 0.6)
#' # 
#' # Overall change without groups
#' # results <- af_analyze_extremism_waves(df, "extremism_score", group_var = NULL)
#' 
#' @export
af_analyze_extremism_waves <- function(df, extremism_var, group_var = NULL, wave_var = "Wave", 
                                       respondent_id_var = "respondent_id", 
                                       control_vars = NULL, panel_pairs = c(3, 4),
                                       alpha = 0.05, plot_title = NULL,
                                       event_names = NULL, line_width = 0.8, point_size = 3,
                                       plots_per_row = 3, use_bw = FALSE, color_palette = "viridis",
                                       show_sig=FALSE) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data frame")
  }
  
  required_vars <- c(extremism_var, wave_var, respondent_id_var)
  if (!is.null(group_var)) {
    required_vars <- c(required_vars, group_var)
  }
  if (!is.null(control_vars)) {
    required_vars <- c(required_vars, control_vars)
  }
  
  missing_vars <- required_vars[!required_vars %in% names(df)]
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables in df:", paste(missing_vars, collapse = ", ")))
  }
  
  if (!is.numeric(df[[extremism_var]])) {
    stop(paste("Variable", extremism_var, "must be numeric"))
  }
  
  # Check control variables are categorical or can be converted to factors
  if (!is.null(control_vars)) {
    for (var in control_vars) {
      if (!is.factor(df[[var]]) && !is.character(df[[var]]) && !is.numeric(df[[var]])) {
        stop(paste("Control variable", var, "must be categorical (factor, character, or numeric that can be converted to factor)"))
      }
    }
  }
  
  if (!is.null(group_var) && length(unique(df[[group_var]])) != 3) {
    stop("Expected exactly 3 groups in group_var when group_var is provided")
  }
  
  if (!all(panel_pairs %in% 1:5)) {
    stop("panel_pairs must be integers between 1 and 5")
  }
  
  if (!is.null(event_names) && length(event_names) != 5) {
    stop("event_names must be NULL or a vector of length 5")
  }
  
  if (!color_palette %in% c("viridis", "default")) {
    stop("color_palette must be 'viridis' or 'default'")
  }
  
  if (!is.logical(show_sig)) {
    stop("show_sig must be TRUE or FALSE")
  }
  
  # Create wave pairs - ensure proper ordering of categorical waves
  if (is.factor(df[[wave_var]])) {
    waves <- levels(df[[wave_var]])
    if (length(waves) != 6) {
      stop(paste("Expected exactly 6 factor levels in", wave_var, "but found", length(waves)))
    }
  } else {
    waves <- sort(unique(df[[wave_var]]))
    if (length(waves) != 6) {
      stop(paste("Expected exactly 6 unique values in", wave_var, "but found", length(waves)))
    }
    # Convert to factor with proper ordering
    df[[wave_var]] <- factor(df[[wave_var]], levels = waves, ordered = TRUE)
  }
  
  # Create wave pairs using the waves variable
  wave_pairs <- list(
    pair1 = c(waves[1], waves[2]),
    pair2 = c(waves[2], waves[3]),
    pair3 = c(waves[3], waves[4]),
    pair4 = c(waves[4], waves[5]),
    pair5 = c(waves[5], waves[6])
  )
  
  # Initialize results storage
  anova_results <- list()
  descriptive_stats <- list()
  plot_data_list <- list()
  
  # Analyze each wave pair
  for (i in 1:5) {
    pair_name <- paste0("pair", i)
    current_waves <- wave_pairs[[i]]
    is_panel <- i %in% panel_pairs
    
    # Filter data for current wave pair
    if (is_panel) {
      # For panel waves: keep only respondents who appear in both waves
      wave1_ids <- df %>%
        filter(!!sym(wave_var) == current_waves[1]) %>%
        pull(!!sym(respondent_id_var))
      
      wave2_ids <- df %>%
        filter(!!sym(wave_var) == current_waves[2]) %>%
        pull(!!sym(respondent_id_var))
      
      matched_ids <- intersect(wave1_ids, wave2_ids)
      
      # Create list of variables to check for missing values
      vars_to_check <- c(extremism_var)
      if (!is.null(group_var)) {
        vars_to_check <- c(vars_to_check, group_var)
      }
      if (!is.null(control_vars)) {
        vars_to_check <- c(vars_to_check, control_vars)
      }
      
      pair_data <- df %>%
        filter(!!sym(wave_var) %in% current_waves,
               !!sym(respondent_id_var) %in% matched_ids) %>%
        filter(if_all(all_of(vars_to_check), ~ !is.na(.)))
      
      # Check for duplicate respondent IDs within waves (data quality issue)
      duplicates_check <- pair_data %>%
        group_by(!!sym(respondent_id_var), !!sym(wave_var)) %>%
        summarise(n_obs = n(), .groups = "drop") %>%
        filter(n_obs > 1)
      
      if (nrow(duplicates_check) > 0) {
        warning(paste("Found", nrow(duplicates_check), "respondents with multiple observations within the same wave in", pair_name, ". Keeping first observation only."))
        pair_data <- pair_data %>%
          group_by(!!sym(respondent_id_var), !!sym(wave_var)) %>%
          slice_head(n = 1) %>%
          ungroup()
      }
      
      # Check for respondents who changed groups between waves (only if group_var provided)
      if (!is.null(group_var)) {
        group_changes <- pair_data %>%
          group_by(!!sym(respondent_id_var)) %>%
          summarise(
            n_waves = n(),
            n_groups = n_distinct(!!sym(group_var)),
            .groups = "drop"
          ) %>%
          filter(n_waves == 2, n_groups > 1)
        
        if (nrow(group_changes) > 0) {
          warning(paste("Found", nrow(group_changes), "respondents who changed groups between waves in", pair_name, ". Excluding these respondents."))
          pair_data <- pair_data %>%
            filter(!(!!sym(respondent_id_var) %in% group_changes[[respondent_id_var]]))
        }
      }
      
      # Also check control variables for consistency (if provided)
      if (!is.null(control_vars)) {
        for (var in control_vars) {
          control_changes <- pair_data %>%
            group_by(!!sym(respondent_id_var)) %>%
            summarise(
              n_waves = n(),
              n_values = n_distinct(!!sym(var)),
              .groups = "drop"
            ) %>%
            filter(n_waves == 2, n_values > 1)
          
          if (nrow(control_changes) > 0) {
            warning(paste("Found", nrow(control_changes), "respondents who changed", var, "between waves in", pair_name, ". Excluding these respondents."))
            pair_data <- pair_data %>%
              filter(!(!!sym(respondent_id_var) %in% control_changes[[respondent_id_var]]))
          }
        }
      }
      
      # Now create factors
      pair_data <- pair_data %>%
        mutate(
          wave_factor = factor(!!sym(wave_var), levels = current_waves, 
                               labels = c("Pre_Event", "Post_Event")),
          respondent_id_factor = factor(!!sym(respondent_id_var))
        )
      
      if (!is.null(group_var)) {
        pair_data$group_factor <- factor(pair_data[[group_var]])
      }
      
      # Convert control variables to factors
      if (!is.null(control_vars)) {
        for (var in control_vars) {
          pair_data[[paste0(var, "_factor")]] <- factor(pair_data[[var]])
        }
      }
      
      # Final check for complete pairs
      paired_check <- pair_data %>%
        group_by(!!sym(respondent_id_var)) %>%
        summarise(n_waves = n(), .groups = "drop") %>%
        filter(n_waves == 2)
      
      if (nrow(paired_check) == 0) {
        warning(paste("No valid matched pairs found for panel", pair_name, "after data cleaning"))
        next
      }
      
      # Keep only complete pairs
      pair_data <- pair_data %>%
        filter(!!sym(respondent_id_var) %in% paired_check[[respondent_id_var]])
      
    } else {
      # For cross-sectional waves: use all available data
      vars_to_check <- c(extremism_var)
      if (!is.null(group_var)) {
        vars_to_check <- c(vars_to_check, group_var)
      }
      if (!is.null(control_vars)) {
        vars_to_check <- c(vars_to_check, control_vars)
      }
      
      pair_data <- df %>%
        filter(!!sym(wave_var) %in% current_waves) %>%
        mutate(
          wave_factor = factor(!!sym(wave_var), levels = current_waves, 
                               labels = c("Pre_Event", "Post_Event"))
        )
      
      if (!is.null(group_var)) {
        pair_data$group_factor <- factor(pair_data[[group_var]])
      }
      
      # Convert control variables to factors
      if (!is.null(control_vars)) {
        for (var in control_vars) {
          pair_data[[paste0(var, "_factor")]] <- factor(pair_data[[var]])
        }
      }
      
      pair_data <- pair_data %>%
        filter(if_all(all_of(vars_to_check), ~ !is.na(.)))
    }
    
    if (nrow(pair_data) == 0) {
      warning(paste("No data available for", pair_name))
      next
    }
    
    # Perform ANOVA (different approach for panel vs cross-sectional)
    tryCatch({
      if (is_panel) {
        # Repeated measures ANOVA for panel data
        if (!is.null(group_var)) {
          if (!is.null(control_vars)) {
            # Create list of between-subjects factors
            control_factors <- paste0(control_vars, "_factor")
            between_factors <- c("group_factor", control_factors)
            
            anova_model <- afex::aov_ez(
              id = respondent_id_var,
              dv = extremism_var,
              data = pair_data,
              within = "wave_factor",
              between = between_factors,
              type = 3
            )
          } else {
            anova_model <- afex::aov_ez(
              id = respondent_id_var,
              dv = extremism_var,
              data = pair_data,
              within = "wave_factor",
              between = "group_factor",
              type = 3
            )
          }
        } else {
          # No group variable - just within-subjects wave effect
          anova_model <- afex::aov_ez(
            id = respondent_id_var,
            dv = extremism_var,
            data = pair_data,
            within = "wave_factor",
            type = 3
          )
        }
        
        anova_summary <- anova_model$anova_table
        
        # Post hoc tests for repeated measures
        emm_wave <- emmeans(anova_model, ~ wave_factor)
        if (!is.null(group_var)) {
          emm_group <- emmeans(anova_model, ~ group_factor)
          emm_interaction <- emmeans(anova_model, ~ wave_factor * group_factor)
          posthoc_group <- pairs(emm_group, adjust = "tukey")
        } else {
          emm_group <- NULL
          emm_interaction <- NULL
          posthoc_group <- NULL
        }
        
        posthoc_wave <- pairs(emm_wave, adjust = "tukey")
        
        # Calculate effect sizes for afex output
        eta_squared <- anova_summary$ges  # Generalized eta squared from afex
        
      } else {
        # Multi-factor ANOVA for cross-sectional data
        if (!is.null(group_var)) {
          if (!is.null(control_vars)) {
            # Build formula with control factors
            control_factors <- paste0(control_vars, "_factor")
            main_effects <- c("wave_factor", "group_factor", control_factors)
            interactions <- "wave_factor:group_factor"
            formula_terms <- c(main_effects, interactions)
            anova_formula <- reformulate(formula_terms, extremism_var)
          } else {
            anova_formula <- reformulate(c("wave_factor", "group_factor", "wave_factor:group_factor"), 
                                         extremism_var)
          }
        } else {
          # No group variable - just wave effect
          if (!is.null(control_vars)) {
            control_factors <- paste0(control_vars, "_factor")
            main_effects <- c("wave_factor", control_factors)
            anova_formula <- reformulate(main_effects, extremism_var)
          } else {
            anova_formula <- reformulate("wave_factor", extremism_var)
          }
        }
        
        anova_model <- aov(anova_formula, data = pair_data)
        anova_summary <- Anova(anova_model, type = "III")
        
        # Calculate effect sizes
        ss_total <- sum(anova_summary$`Sum Sq`)
        eta_squared <- anova_summary$`Sum Sq` / ss_total
        
        # Post hoc tests
        emm_wave <- emmeans(anova_model, ~ wave_factor)
        if (!is.null(group_var)) {
          emm_group <- emmeans(anova_model, ~ group_factor)
          emm_interaction <- emmeans(anova_model, ~ wave_factor * group_factor)
          posthoc_group <- pairs(emm_group, adjust = "tukey")
        } else {
          emm_group <- NULL
          emm_interaction <- NULL
          posthoc_group <- NULL
        }
        
        posthoc_wave <- pairs(emm_wave, adjust = "tukey")
      }
      
      # Extract significance levels for plotting
      if (is_panel) {
        wave_p <- if("wave_factor" %in% rownames(anova_summary)) 
          anova_summary["wave_factor", "Pr(>F)"] else NA
        group_p <- if(!is.null(group_var) && "group_factor" %in% rownames(anova_summary)) 
          anova_summary["group_factor", "Pr(>F)"] else NA
        interaction_p <- if(!is.null(group_var) && any(grepl("wave_factor:group_factor", rownames(anova_summary)))) {
          anova_summary[grep("wave_factor:group_factor", rownames(anova_summary))[1], "Pr(>F)"]
        } else NA
      } else {
        wave_p <- if("wave_factor" %in% rownames(anova_summary)) 
          anova_summary["wave_factor", "Pr(>F)"] else NA
        group_p <- if(!is.null(group_var) && "group_factor" %in% rownames(anova_summary)) 
          anova_summary["group_factor", "Pr(>F)"] else NA
        interaction_p <- if(!is.null(group_var) && "wave_factor:group_factor" %in% rownames(anova_summary)) 
          anova_summary["wave_factor:group_factor", "Pr(>F)"] else NA
      }
      
      anova_results[[pair_name]] <- list(
        model = anova_model,
        anova_table = anova_summary,
        eta_squared = eta_squared,
        posthoc_wave = posthoc_wave,
        posthoc_group = posthoc_group,
        emmeans_interaction = emm_interaction,
        is_panel = is_panel,
        n_matched_pairs = if(is_panel) nrow(paired_check) else NA,
        control_vars = control_vars,
        formula_used = if(is_panel) NA else paste(deparse(anova_formula), collapse = " "),
        wave_p = wave_p,
        group_p = group_p,
        interaction_p = interaction_p
      )
      
      # Calculate descriptive statistics
      if (is_panel) {
        # For panel data, calculate by matched pairs
        if (!is.null(group_var)) {
          desc_stats <- pair_data %>%
            group_by(wave_factor, group_factor) %>%
            summarise(
              n = n(),
              mean = mean(!!sym(extremism_var), na.rm = TRUE),
              sd = sd(!!sym(extremism_var), na.rm = TRUE),
              se = sd / sqrt(n),
              .groups = "drop"
            ) %>%
            mutate(pair = pair_name,
                   wave_pair = paste(current_waves, collapse = " → "),
                   analysis_type = "Panel (Repeated Measures)")
        } else {
          desc_stats <- pair_data %>%
            group_by(wave_factor) %>%
            summarise(
              n = n(),
              mean = mean(!!sym(extremism_var), na.rm = TRUE),
              sd = sd(!!sym(extremism_var), na.rm = TRUE),
              se = sd / sqrt(n),
              .groups = "drop"
            ) %>%
            mutate(pair = pair_name,
                   wave_pair = paste(current_waves, collapse = " → "),
                   analysis_type = if(is_panel) "Panel (Repeated Measures)" else "Cross-sectional",
                   group_factor = "Overall")
        }
      } else {
        # For cross-sectional data
        if (!is.null(group_var)) {
          desc_stats <- pair_data %>%
            group_by(wave_factor, group_factor) %>%
            summarise(
              n = n(),
              mean = mean(!!sym(extremism_var), na.rm = TRUE),
              sd = sd(!!sym(extremism_var), na.rm = TRUE),
              se = sd / sqrt(n),
              .groups = "drop"
            ) %>%
            mutate(pair = pair_name,
                   wave_pair = paste(current_waves, collapse = " → "),
                   analysis_type = "Cross-sectional")
        } else {
          desc_stats <- pair_data %>%
            group_by(wave_factor) %>%
            summarise(
              n = n(),
              mean = mean(!!sym(extremism_var), na.rm = TRUE),
              sd = sd(!!sym(extremism_var), na.rm = TRUE),
              se = sd / sqrt(n),
              .groups = "drop"
            ) %>%
            mutate(pair = pair_name,
                   wave_pair = paste(current_waves, collapse = " → "),
                   analysis_type = "Cross-sectional",
                   group_factor = "Overall")
        }
      }
      
      # Add event names if provided
      if (!is.null(event_names)) {
        desc_stats$event_name <- event_names[i]
        desc_stats$wave_pair_with_event <- paste0(event_names[i], "\n", desc_stats$wave_pair)
      } else {
        desc_stats$wave_pair_with_event <- desc_stats$wave_pair
      }
      
      descriptive_stats[[pair_name]] <- desc_stats
      plot_data_list[[pair_name]] <- desc_stats
      
    }, error = function(e) {
      warning(paste("ANOVA failed for", pair_name, ":", e$message))
    })
  }
  
  # Combine plot data
  if (length(plot_data_list) > 0) {
    plot_data <- bind_rows(plot_data_list)
    
    # Add significance indicators if requested
    if (show_sig) {
      sig_data <- data.frame()
      for (pair_name in names(anova_results)) {
        if (!is.null(anova_results[[pair_name]])) {
          wave_p <- anova_results[[pair_name]]$wave_p
          group_p <- anova_results[[pair_name]]$group_p
          interaction_p <- anova_results[[pair_name]]$interaction_p
          
          # Create significance stars
          wave_sig <- ifelse(is.na(wave_p), "", 
                             ifelse(wave_p < 0.001, "***",
                                    ifelse(wave_p < 0.01, "**", 
                                           ifelse(wave_p < 0.05, "*", ""))))
          
          interaction_sig <- ifelse(is.na(interaction_p), "", 
                                    ifelse(interaction_p < 0.001, "***",
                                           ifelse(interaction_p < 0.01, "**", 
                                                  ifelse(interaction_p < 0.05, "*", ""))))
          
          pair_sig <- data.frame(
            pair = pair_name,
            wave_sig = wave_sig,
            interaction_sig = interaction_sig,
            stringsAsFactors = FALSE
          )
          sig_data <- rbind(sig_data, pair_sig)
        }
      }
      
      # Merge with plot data
      plot_data <- merge(plot_data, sig_data, by = "pair", all.x = TRUE)
    }
    
    # Reorder pairs to go 1, 2, 3, 4, 5
    plot_data$pair_order <- as.numeric(gsub("pair", "", plot_data$pair))
    plot_data <- plot_data %>% arrange(pair_order)
    
    # Ensure proper factor ordering for facets
    if (!is.null(event_names)) {
      plot_data$wave_pair_with_event <- factor(plot_data$wave_pair_with_event, 
                                               levels = unique(plot_data$wave_pair_with_event[order(plot_data$pair_order)]))
    } else {
      plot_data$wave_pair <- factor(plot_data$wave_pair, 
                                    levels = unique(plot_data$wave_pair[order(plot_data$pair_order)]))
    }
    
    # Set up colors and line types
    if (!is.null(group_var)) {
      n_groups <- length(unique(plot_data$group_factor))
      
      if (use_bw) {
        colors <- rep("black", n_groups)
        line_types <- c("solid", "dashed", "dotted")[1:n_groups]
      } else {
        if (color_palette == "viridis") {
          colors <- viridis::viridis(n_groups, end = 0.9)
        } else {
          colors <- c("#2E86AB", "#A23B72", "#F18F01")[1:n_groups]  # Default colors
        }
        line_types <- rep("solid", n_groups)
      }
    } else {
      colors <- if(use_bw) "black" else "#2E86AB"
      line_types <- "solid"
    }
    
    if (is.null(plot_title)) {
      if (!is.null(group_var)) {
        plot_title <- paste("Extremism Levels Across Wave Pairs by", group_var)
      } else {
        plot_title <- paste("Overall Extremism Levels Across Wave Pairs")
      }
    }
    
    # Create the plot
    if (!is.null(group_var)) {
      p <- ggplot(plot_data, aes(x = wave_factor, y = mean, color = group_factor, 
                                 linetype = group_factor, group = group_factor)) +
        geom_line(linewidth = line_width, alpha = 0.8) +
        geom_point(size = point_size) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1, alpha = 0.7) +
        scale_color_manual(values = colors, name = gsub("_", " ", group_var)) +
        scale_linetype_manual(values = line_types, name = gsub("_", " ", group_var))
    } else {
      p <- ggplot(plot_data, aes(x = wave_factor, y = mean, group = 1)) +
        geom_line(linewidth = line_width, alpha = 0.8, color = colors) +
        geom_point(size = point_size, color = colors) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1, alpha = 0.7, color = colors)
    }
    
    # Add facets and theming
    facet_var <- if(!is.null(event_names)) "wave_pair_with_event" else "wave_pair"
    
    # Add significance indicators if requested
    if (show_sig && exists("sig_data")) {
      # Create annotation data
      annotation_data <- plot_data %>%
        group_by(pair, wave_pair_with_event) %>%
        summarise(
          max_y = max(mean + se, na.rm = TRUE),
          wave_sig = first(wave_sig),
          interaction_sig = first(interaction_sig),
          .groups = "drop"
        ) %>%
        mutate(
          y_pos = max_y + 0.1 * max_y,
          sig_text = paste0(
            ifelse(wave_sig != "", paste("Wave:", wave_sig), ""),
            ifelse(interaction_sig != "" & wave_sig != "", " | ", ""),
            ifelse(interaction_sig != "", paste("Interaction:", interaction_sig), "")
          )
        ) %>%
        filter(sig_text != "")
      
      if (nrow(annotation_data) > 0) {
        p <- p + geom_text(data = annotation_data, 
                           aes(x = 1.5, y = y_pos, label = sig_text),
                           inherit.aes = FALSE, size = 3, hjust = 0.5, color = "red")
      }
    }
    
    p <- p +
      facet_wrap(as.formula(paste("~", facet_var)), scales = "free_x", ncol = plots_per_row) +
      labs(
        title = plot_title,
        subtitle = "Change in extremism between consecutive waves across population groups",
        x = "Wave (Pre/Post Event)",
        y = paste("Mean", extremism_var),
        caption = "Error bars represent standard errors"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.title = element_text(size = 11, face = "bold"),
        legend.position = "bottom", 
        strip.text = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank()
      )
  } else {
    p <- NULL
    warning("No data available for plotting")
  }
  
  # Generate results summary text
  results_text <- af_generate_results_text(anova_results, descriptive_stats, extremism_var, group_var, control_vars)
  
  # Return comprehensive results
  return(list(
    anova_results = anova_results,
    descriptive_stats = descriptive_stats,
    plot = p,
    results_text = results_text,
    summary_data = if(exists("plot_data")) plot_data else NULL
  ))
}

#' Generate Results Text Summary
#'
#' Internal function to generate formatted results text similar to academic papers
#'
#' @param anova_results List of ANOVA results for each wave pair
#' @param descriptive_stats List of descriptive statistics
#' @param extremism_var Name of the extremism variable
#' @param group_var Name of the group variable
#' @param control_vars Names of control variables (if any)
#' 
#' @return Character string with formatted results
af_generate_results_text <- function(anova_results, descriptive_stats, extremism_var, group_var = NULL, control_vars = NULL) {
  
  if (length(anova_results) == 0) {
    return("No valid ANOVA results to report.")
  }
  
  # results_text <- paste0("## Analysis of ", extremism_var, " Across Wave Pairs")
  # if (!is.null(group_var)) {
  #   results_text <- paste0(results_text, " and ", group_var)
  # }
  # if (!is.null(control_vars)) {
  #   results_text <- paste0(results_text, "\n### Control Variables: ", paste(control_vars, collapse = ", "))
  # }
  # results_text <- paste0(results_text, "\n\n")
  
  results_text <- "\n\n"
  
  for (pair_name in names(anova_results)) {
    pair_results <- anova_results[[pair_name]]
    desc_stats <- descriptive_stats[[pair_name]]
    
    if (is.null(pair_results) || is.null(desc_stats)) next
    
    anova_table <- pair_results$anova_table
    eta_sq <- pair_results$eta_squared
    is_panel <- pair_results$is_panel
    
    results_text <- paste0(results_text, "### ", toupper(pair_name))
    if (is_panel) {
      results_text <- paste0(results_text, " (PANEL - REPEATED MEASURES)")
      if (!is.na(pair_results$n_matched_pairs)) {
        results_text <- paste0(results_text, " - ", pair_results$n_matched_pairs, " matched pairs")
      }
    } else {
      results_text <- paste0(results_text, " (CROSS-SECTIONAL)")
    }
    results_text <- paste0(results_text, "\n\n")
    
    if (is_panel) {
      # For afex repeated measures output
      wave_row <- which(rownames(anova_table) == "wave_factor")
      
      if (length(wave_row) > 0) {
        wave_effect <- anova_table[wave_row, ]
        results_text <- paste0(results_text, 
                               sprintf("**Main effect of Wave**: F(%s, %s) = %.2f, p = %.3f, ηG² = %.3f\n",
                                       wave_effect$`num Df`, wave_effect$`den Df`, 
                                       wave_effect$`F`, wave_effect$`Pr(>F)`, eta_sq[wave_row]))
      }
      
      if (!is.null(group_var)) {
        group_row <- which(rownames(anova_table) == "group_factor") 
        int_row <- which(grepl("wave_factor:group_factor", rownames(anova_table)))
        
        if (length(group_row) > 0) {
          group_effect <- anova_table[group_row, ]
          results_text <- paste0(results_text,
                                 sprintf("**Main effect of %s**: F(%s, %s) = %.2f, p = %.3f, ηG² = %.3f\n",
                                         group_var, group_effect$`num Df`, group_effect$`den Df`,
                                         group_effect$`F`, group_effect$`Pr(>F)`, eta_sq[group_row]))
        }
        
        if (length(int_row) > 0) {
          interaction_effect <- anova_table[int_row, ]
          results_text <- paste0(results_text,
                                 sprintf("**Interaction effect**: F(%s, %s) = %.2f, p = %.3f, ηG² = %.3f\n",
                                         interaction_effect$`num Df`, interaction_effect$`den Df`,
                                         interaction_effect$`F`, interaction_effect$`Pr(>F)`, eta_sq[int_row]))
        }
      }
      results_text <- paste0(results_text, "\n")
      
    } else {
      # For standard ANOVA output
      wave_effect <- anova_table["wave_factor", ]
      wave_eta <- round(eta_sq[which(rownames(anova_table) == "wave_factor")], 3)
      
      # Wave effect
      results_text <- paste0(results_text, 
                             sprintf("**Main effect of Wave**: F(%d, %d) = %.2f, p = %.3f, η²p = %.3f\n",
                                     wave_effect$Df, wave_effect$Df[length(wave_effect$Df)], 
                                     wave_effect$`F value`, wave_effect$`Pr(>F)`, wave_eta))
      
      if (!is.null(group_var)) {
        group_effect <- anova_table["group_factor", ]
        interaction_effect <- anova_table["wave_factor:group_factor", ]
        
        group_eta <- round(eta_sq[which(rownames(anova_table) == "group_factor")], 3)
        int_eta <- round(eta_sq[which(rownames(anova_table) == "wave_factor:group_factor")], 3)
        
        # Group effect  
        results_text <- paste0(results_text,
                               sprintf("**Main effect of %s**: F(%d, %d) = %.2f, p = %.3f, η²p = %.3f\n",
                                       group_var, group_effect$Df, group_effect$Df[length(group_effect$Df)],
                                       group_effect$`F value`, group_effect$`Pr(>F)`, group_eta))
        
        # Interaction effect
        results_text <- paste0(results_text,
                               sprintf("**Interaction effect**: F(%d, %d) = %.2f, p = %.3f, η²p = %.3f\n",
                                       interaction_effect$Df, interaction_effect$Df[length(interaction_effect$Df)],
                                       interaction_effect$`F value`, interaction_effect$`Pr(>F)`, int_eta))
      }
      results_text <- paste0(results_text, "\n")
    }
    
    # Add information about control variables if used
    if (!is.null(control_vars)) {
      results_text <- paste0(results_text, "**Control Variables**: ", paste(control_vars, collapse = ", "), "\n")
      if (!is_panel && !is.na(pair_results$formula_used)) {
        results_text <- paste0(results_text, "**Model Formula**: ", pair_results$formula_used, "\n")
      }
      results_text <- paste0(results_text, "\n")
    }
    
    # Add descriptive statistics
    results_text <- paste0(results_text, "**Descriptive Statistics:**\n")
    for (i in 1:nrow(desc_stats)) {
      row <- desc_stats[i, ]
      if (!is.null(group_var)) {
        results_text <- paste0(results_text,
                               sprintf("- %s %s: M = %.2f, SE = %.2f (n = %d)\n",
                                       row$group_factor, row$wave_factor, row$mean, row$se, row$n))
      } else {
        results_text <- paste0(results_text,
                               sprintf("- %s: M = %.2f, SE = %.2f (n = %d)\n",
                                       row$wave_factor, row$mean, row$se, row$n))
      }
    }
    results_text <- paste0(results_text, "\n")
  }
  
  return(results_text)
}

#' Extract Effect Data from Multiple ANOVA Results
#'
#' This function extracts standardized effect data from multiple af_analyze_extremism_waves results
#' to prepare data for effect plotting. It calculates change scores and determines significance.
#'
#' @param results_list List of results objects from af_analyze_extremism_waves
#' @param extremism_labels Character vector. Labels for each extremism dimension (same order as results_list)
#' @param event_names Character vector. Names of events for each pair (length 5)
#' @param df Data frame. Original data used in the analyses
#' @param group_var Character string. Name of the political group variable
#' @param wave_var Character string. Name of the wave variable (default: "Wave")
#' @param respondent_id_var Character string. Name of respondent ID variable (default: "respondent_id")
#' @param panel_pairs Numeric vector. Which pairs are panel waves (default: c(3, 4))
#' @param alpha Numeric. Significance level (default: 0.05)
#' 
#' @return Data frame with standardized effect data
#' 
#' @examples
#' # effect_data <- af_extract_effect_data(
#' #   results_list = list(results_c, results_b, results_s, results_o),
#' #   extremism_labels = c("Cognitive", "Behavioral", "Social", "Overall"),
#' #   event_names = c("Terror", "Gov Fall", "Judicial", "Dismissal", "War"),
#' #   df = df,
#' #   group_var = "pe_left_center_right"
#' # )
#' 
#' @export
af_extract_effect_data <- function(results_list, extremism_labels, event_names, df, 
                                   group_var, wave_var = "Wave", 
                                   respondent_id_var = "respondent_id", 
                                   panel_pairs = c(3, 4), alpha = 0.05) {
  
  # Input validation
  if (!is.list(results_list) || length(results_list) != 4) {
    stop("results_list must be a list of 4 results objects")
  }
  
  if (length(extremism_labels) != 4) {
    stop("extremism_labels must have 4 elements")
  }
  
  if (length(event_names) != 5) {
    stop("event_names must have 5 elements")
  }
  
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  required_vars <- c(group_var, wave_var, respondent_id_var)
  missing_vars <- required_vars[!required_vars %in% names(df)]
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables in df:", paste(missing_vars, collapse = ", ")))
  }
  
  # Get unique group values
  group_levels <- levels(factor(df[[group_var]]))
  if (length(group_levels) != 3) {
    stop("Expected exactly 3 levels in group_var")
  }
  
  # Initialize results storage
  effect_data <- data.frame()
  
  # Process each extremism dimension
  for (dim_idx in 1:4) {
    results <- results_list[[dim_idx]]
    extremism_label <- extremism_labels[dim_idx]
    
    if (is.null(results) || is.null(results$descriptive_stats)) {
      warning(paste("No valid results found for", extremism_label))
      next
    }
    
    # Process each wave pair
    for (pair_idx in 1:5) {
      pair_name <- paste0("pair", pair_idx)
      event_name <- event_names[pair_idx]
      is_panel <- pair_idx %in% panel_pairs
      
      # Get descriptive statistics for this pair
      if (!pair_name %in% names(results$descriptive_stats)) {
        warning(paste("No data for", pair_name, "in", extremism_label))
        next
      }
      
      desc_stats <- results$descriptive_stats[[pair_name]]
      
      if (is.null(desc_stats) || nrow(desc_stats) == 0) {
        next
      }
      
      # Calculate effects for each group
      for (group in group_levels) {
        # Get pre and post data for this group
        if ("group_factor" %in% names(desc_stats)) {
          # Data with groups
          pre_data <- desc_stats[desc_stats$group_factor == group & 
                                   desc_stats$wave_factor == "Pre_Event", ]
          post_data <- desc_stats[desc_stats$group_factor == group & 
                                    desc_stats$wave_factor == "Post_Event", ]
        } else {
          # Overall data (no groups) - use for all groups
          pre_data <- desc_stats[desc_stats$wave_factor == "Pre_Event", ]
          post_data <- desc_stats[desc_stats$wave_factor == "Post_Event", ]
        }
        
        if (nrow(pre_data) == 0 || nrow(post_data) == 0) {
          next
        }
        
        # Calculate effect size (Post - Pre)
        effect_size <- post_data$mean[1] - pre_data$mean[1]
        
        # Calculate standard error of the difference
        if (is_panel) {
          # For panel data, we need to account for correlation
          # Use pooled SE as approximation (conservative)
          se_diff <- sqrt(pre_data$se[1]^2 + post_data$se[1]^2)
        } else {
          # For cross-sectional data, SEs are independent
          se_diff <- sqrt(pre_data$se[1]^2 + post_data$se[1]^2)
        }
        
        # Calculate confidence interval
        t_crit <- qt(1 - alpha/2, df = min(pre_data$n[1], post_data$n[1]) - 1)
        ci_lower <- effect_size - t_crit * se_diff
        ci_upper <- effect_size + t_crit * se_diff
        
        # Determine significance based on confidence intervals
        significant <- !((ci_lower <= 0) & (ci_upper >= 0))
        
        # Get p-value from ANOVA results for reference (but don't use for significance)
        p_value <- NA
        if (pair_name %in% names(results$anova_results)) {
          anova_res <- results$anova_results[[pair_name]]
          if (!is.null(anova_res$wave_p)) {
            p_value <- anova_res$wave_p
          }
        }
        
        # Create significance stars based on CI-based significance only
        if (significant) {
          # For significant effects, assign stars based on effect size magnitude as proxy
          abs_effect <- abs(effect_size)
          stars <- ifelse(abs_effect > 0.3, "***",
                          ifelse(abs_effect > 0.15, "**", "*"))
        } else {
          stars <- ""
        }
        
        # Add to results
        effect_row <- data.frame(
          extremism_type = extremism_label,
          political_group = group,
          event = event_name,
          pair_number = pair_idx,
          effect_size = effect_size,
          se = se_diff,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          p_value = p_value,
          significant = significant,
          stars = stars,
          data_type = ifelse(is_panel, "Panel", "Cross-sectional"),
          pre_mean = pre_data$mean[1],
          post_mean = post_data$mean[1],
          pre_n = pre_data$n[1],
          post_n = post_data$n[1],
          stringsAsFactors = FALSE
        )
        
        effect_data <- rbind(effect_data, effect_row)
      }
    }
  }
  
  # Ensure proper factor ordering
  effect_data$extremism_type <- factor(effect_data$extremism_type, 
                                       levels = extremism_labels)
  effect_data$political_group <- factor(effect_data$political_group, 
                                        levels = group_levels)
  effect_data$event <- factor(effect_data$event, levels = event_names)
  
  return(effect_data)
}

#' Create Effect Plot from Standardized Effect Data
#'
#' This function creates a publication-ready effect plot showing change scores
#' with confidence intervals and significance indicators across multiple dimensions.
#'
#' @param effect_data Data frame. Output from af_extract_effect_data()
#' @param plot_title Character string. Main title for the plot (default: auto-generated)
#' @param plot_subtitle Character string. Subtitle for the plot (default: auto-generated)
#' @param plot_note Character string. Note to add below the plot (default: NULL)
#' @param line_width Numeric. Width of error bar lines (default: 0.8)
#' @param point_size Numeric. Size of effect points (default: 3)
#' @param font_size_axes Numeric. Font size for axis text (default: 10)
#' @param font_size_labels Numeric. Font size for axis labels (default: 12)
#' @param font_size_title Numeric. Font size for title (default: 14)
#' @param use_bw Logical. Use black and white mode with line types (default: FALSE)
#' @param color_palette Character. Color palette: "viridis", "default" (default: "viridis")
#' @param show_stars Logical. Show significance stars above points (default: TRUE)
#' @param ncol_facets Numeric. Number of columns for facet wrap (default: 5)
#' 
#' @return ggplot2 object
#' 
#' @examples
#' # p <- af_plot_effects(
#' #   effect_data = effect_data,
#' #   plot_title = "Political Extremism Response to Events",
#' #   plot_note = "Error bars represent 95% confidence intervals",
#' #   use_bw = FALSE
#' # )
#' 
#' @export
af_plot_effects <- function(effect_data, plot_title = NULL, plot_subtitle = NULL, 
                            plot_note = NULL, line_width = 0.8, point_size = 3,
                            font_size_axes = 10, font_size_labels = 12, 
                            font_size_title = 14, use_bw = FALSE, 
                            color_palette = "viridis", show_stars = TRUE,
                            ncol_facets = 5) {
  
  # Input validation
  if (!is.data.frame(effect_data)) {
    stop("effect_data must be a data frame")
  }
  
  required_cols <- c("extremism_type", "political_group", "event", "effect_size", 
                     "se", "ci_lower", "ci_upper", "significant", "stars", "data_type")
  missing_cols <- required_cols[!required_cols %in% names(effect_data)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in effect_data:", paste(missing_cols, collapse = ", ")))
  }
  
  if (!color_palette %in% c("viridis", "default")) {
    stop("color_palette must be 'viridis' or 'default'")
  }
  
  # Create y-axis positions for groups within extremism types
  effect_data$y_pos <- as.numeric(effect_data$extremism_type) * 3 - 2 + 
    as.numeric(effect_data$political_group) - 1
  
  # Set up colors and shapes
  n_groups <- length(unique(effect_data$political_group))
  
  if (use_bw) {
    colors <- c("black", "grey40", "grey70")[1:n_groups]
    line_types <- c("solid", "dashed", "dotted")[1:n_groups]
  } else {
    if (color_palette == "viridis") {
      colors <- viridis::viridis(n_groups, end = 0.9)
    } else {
      colors <- c("#2E86AB", "#A23B72", "#F18F01")[1:n_groups]
    }
    line_types <- rep("solid", n_groups)
  }
  
  # Set point shapes
  shapes <- c("Panel" = 16, "Cross-sectional" = 17)  # Circle for panel, triangle for cross-sectional
  
  # Default titles
  if (is.null(plot_title)) {
    plot_title <- "Political Group Responses to Events Across Extremism Dimensions"
  }
  
  if (is.null(plot_subtitle)) {
    plot_subtitle <- "Effect sizes (Post-Event - Pre-Event) with 95% confidence intervals"
  }
  
  # Create the plot
  p <- ggplot(effect_data, aes(x = effect_size, y = y_pos, 
                               color = political_group, 
                               linetype = political_group,
                               shape = data_type)) +
    
    # Add reference line at x = 0
    geom_vline(xintercept = 0, linetype = "dotted", color = "black", alpha = 0.7) +
    
    # Add error bars
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                   height = 0.2, linewidth = line_width, alpha = 0.8) +
    
    # Add points
    geom_point(size = point_size, alpha = 0.9) +
    
    # Set colors and line types
    scale_color_manual(values = colors, name = "Political Group") +
    scale_linetype_manual(values = line_types, name = "Political Group") +
    scale_shape_manual(values = shapes, name = "Data Type") +
    
    # Set y-axis
    scale_y_continuous(
      breaks = c(1.5, 4.5, 7.5, 10.5),
      labels = levels(effect_data$extremism_type),
      limits = c(0.5, 12.5)
    ) +
    
    # Facet by event
    facet_wrap(~ event, ncol = ncol_facets, scales = "free_x") +
    
    # Labels and theme
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Effect Size (Post - Pre)",
      y = "Extremism Dimension",
      caption = plot_note
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = font_size_title, face = "bold"),
      plot.subtitle = element_text(size = font_size_labels),
      axis.title = element_text(size = font_size_labels),
      axis.text = element_text(size = font_size_axes),
      legend.title = element_text(size = font_size_labels, face = "bold"),
      legend.text = element_text(size = font_size_axes),
      strip.text = element_text(size = font_size_axes, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  # Add effect values and significance stars if requested
  if (show_stars) {
    # Create labels with effect size and stars
    effect_data$effect_label <- paste0(sprintf("%.2f", effect_data$effect_size), 
                                       ifelse(effect_data$stars != "", 
                                              paste0(" ", effect_data$stars), ""))
    
    p <- p + geom_text(data = effect_data, 
                       aes(x = effect_size, y = y_pos + 0.15, label = effect_label),
                       inherit.aes = FALSE, size = 2.5, color = "black", 
                       fontface = "bold")
  }
  
  return(p)
}

#' Extract Overall Effect Data Across Political Groups
#'
#' This function extracts overall effects (main wave effects) from multiple 
#' af_analyze_extremism_waves results, ignoring group differences.
#'
#' @param results_list List of results objects from af_analyze_extremism_waves
#' @param extremism_labels Character vector. Labels for each extremism dimension
#' @param event_names Character vector. Names of events for each pair (length 5)
#' @param alpha Numeric. Significance level (default: 0.05)
#' 
#' @return Data frame with overall effect data
#' 
#' @examples
#' # overall_effects <- af_extract_overall_effects(
#' #   results_list = list(results_c, results_b, results_s, results_o),
#' #   extremism_labels = c("Cognitive", "Behavioral", "Social", "Overall"),
#' #   event_names = event_names
#' # )
#' 
#' @export
af_extract_overall_effects <- function(results_list, extremism_labels, event_names, alpha = 0.05) {
  
  # Input validation
  if (!is.list(results_list) || length(results_list) != 4) {
    stop("results_list must be a list of 4 results objects")
  }
  
  if (length(extremism_labels) != 4) {
    stop("extremism_labels must have 4 elements")
  }
  
  if (length(event_names) != 5) {
    stop("event_names must have 5 elements")
  }
  
  # Initialize results storage
  overall_data <- data.frame()
  
  # Process each extremism dimension
  for (dim_idx in 1:4) {
    results <- results_list[[dim_idx]]
    extremism_label <- extremism_labels[dim_idx]
    
    if (is.null(results) || is.null(results$descriptive_stats) || is.null(results$anova_results)) {
      warning(paste("No valid results found for", extremism_label))
      next
    }
    
    # Process each wave pair
    for (pair_idx in 1:5) {
      pair_name <- paste0("pair", pair_idx)
      event_name <- event_names[pair_idx]
      
      # Get descriptive statistics for this pair
      if (!pair_name %in% names(results$descriptive_stats)) {
        warning(paste("No data for", pair_name, "in", extremism_label))
        next
      }
      
      desc_stats <- results$descriptive_stats[[pair_name]]
      anova_res <- results$anova_results[[pair_name]]
      
      if (is.null(desc_stats) || nrow(desc_stats) == 0 || is.null(anova_res)) {
        next
      }
      
      # Calculate overall means across all groups
      if ("group_factor" %in% names(desc_stats)) {
        # Data with groups - calculate weighted average
        pre_data <- desc_stats[desc_stats$wave_factor == "Pre_Event", ]
        post_data <- desc_stats[desc_stats$wave_factor == "Post_Event", ]
        
        # Weighted means
        pre_mean <- weighted.mean(pre_data$mean, pre_data$n)
        post_mean <- weighted.mean(post_data$mean, post_data$n)
        
        # Combined sample sizes
        pre_n <- sum(pre_data$n)
        post_n <- sum(post_data$n)
        
        # Pooled standard error (approximation)
        pre_var <- weighted.mean(pre_data$sd^2, pre_data$n)
        post_var <- weighted.mean(post_data$sd^2, post_data$n)
        pooled_se <- sqrt(pre_var/pre_n + post_var/post_n)
        
      } else {
        # Overall data (no groups)
        pre_data <- desc_stats[desc_stats$wave_factor == "Pre_Event", ]
        post_data <- desc_stats[desc_stats$wave_factor == "Post_Event", ]
        
        pre_mean <- pre_data$mean[1]
        post_mean <- post_data$mean[1]
        pre_n <- pre_data$n[1]
        post_n <- post_data$n[1]
        pooled_se <- sqrt(pre_data$se[1]^2 + post_data$se[1]^2)
      }
      
      # Calculate effect size (Post - Pre)
      effect_size <- post_mean - pre_mean
      
      # Calculate confidence interval
      df_approx <- min(pre_n, post_n) - 1
      t_crit <- qt(1 - alpha/2, df = df_approx)
      ci_lower <- effect_size - t_crit * pooled_se
      ci_upper <- effect_size + t_crit * pooled_se
      
      # Determine significance based on confidence intervals
      significant <- !((ci_lower <= 0) & (ci_upper >= 0))
      
      # Get main wave effect p-value from ANOVA
      wave_p <- NA
      if (!is.null(anova_res$wave_p)) {
        wave_p <- anova_res$wave_p
        # Use ANOVA p-value for more accurate significance if available
        significant <- wave_p < alpha
      }
      
      # Create significance stars
      if (significant) {
        if (!is.na(wave_p)) {
          stars <- ifelse(wave_p < 0.001, "***",
                          ifelse(wave_p < 0.01, "**",
                                 ifelse(wave_p < 0.05, "*", "")))
        } else {
          # Fallback to effect size magnitude
          abs_effect <- abs(effect_size)
          stars <- ifelse(abs_effect > 0.3, "***",
                          ifelse(abs_effect > 0.15, "**", "*"))
        }
      } else {
        stars <- ""
      }
      
      # Determine data type
      is_panel <- ifelse(!is.null(anova_res$is_panel), anova_res$is_panel, FALSE)
      data_type <- ifelse(is_panel, "Panel", "Cross-sectional")
      
      # Add to results
      effect_row <- data.frame(
        extremism_type = extremism_label,
        event = event_name,
        pair_number = pair_idx,
        effect_size = effect_size,
        se = pooled_se,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        p_value = wave_p,
        significant = significant,
        stars = stars,
        data_type = data_type,
        pre_mean = pre_mean,
        post_mean = post_mean,
        pre_n = pre_n,
        post_n = post_n,
        stringsAsFactors = FALSE
      )
      
      overall_data <- rbind(overall_data, effect_row)
    }
  }
  
  # Ensure proper factor ordering
  overall_data$extremism_type <- factor(overall_data$extremism_type, levels = extremism_labels)
  overall_data$event <- factor(overall_data$event, levels = event_names)
  
  return(overall_data)
}


#' Create Overall Effect Plot (No Political Group Separation)
#'
#' This function creates a simplified effect plot showing overall effects
#' across extremism dimensions without political group separation.
#'
#' @param overall_data Data frame. Output from af_extract_overall_effects()
#' @param plot_title Character string. Main title for the plot (default: auto-generated)
#' @param plot_subtitle Character string. Subtitle for the plot (default: auto-generated)
#' @param plot_note Character string. Note to add below the plot (default: NULL)
#' @param line_width Numeric. Width of error bar lines (default: 1.0)
#' @param point_size Numeric. Size of effect points (default: 4)
#' @param font_size_axes Numeric. Font size for axis text (default: 11)
#' @param font_size_labels Numeric. Font size for axis labels (default: 13)
#' @param font_size_title Numeric. Font size for title (default: 15)
#' @param use_bw Logical. Use black and white mode (default: FALSE)
#' @param point_color Character. Color for points (default: "#2E86AB")
#' @param show_values Logical. Show effect values and stars above points (default: TRUE)
#' @param ncol_facets Numeric. Number of columns for facet wrap (default: 5)
#' 
#' @return ggplot2 object
#' 
#' @examples
#' # p <- af_plot_overall_effects(
#' #   overall_data = overall_effects,
#' #   plot_title = "Overall Political Extremism Response to Events",
#' #   plot_note = "Error bars represent 95% confidence intervals"
#' # )
#' 
#' @export
af_plot_overall_effects <- function(overall_data, plot_title = NULL, plot_subtitle = NULL, 
                                    plot_note = NULL, line_width = 1.0, point_size = 4,
                                    font_size_axes = 11, font_size_labels = 13, 
                                    font_size_title = 15, use_bw = FALSE, 
                                    point_color = "#2E86AB", show_values = TRUE,
                                    ncol_facets = 5) {
  
  # Input validation
  if (!is.data.frame(overall_data)) {
    stop("overall_data must be a data frame")
  }
  
  required_cols <- c("extremism_type", "event", "effect_size", "se", "ci_lower", 
                     "ci_upper", "significant", "stars", "data_type")
  missing_cols <- required_cols[!required_cols %in% names(overall_data)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in overall_data:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create y-axis positions (one per extremism type)
  overall_data$y_pos <- as.numeric(overall_data$extremism_type)
  
  # Set up colors and shapes
  if (use_bw) {
    colors <- "black"
    line_color <- "black"
  } else {
    colors <- point_color
    line_color <- point_color
  }
  
  # Set point shapes based on data type
  shapes <- c("Panel" = 16, "Cross-sectional" = 17)  # Circle for panel, triangle for cross-sectional
  
  # Default titles
  if (is.null(plot_title)) {
    plot_title <- "Overall Political Extremism Response to Events"
  }
  
  if (is.null(plot_subtitle)) {
    plot_subtitle <- "Overall effect sizes (Post-Event - Pre-Event) with 95% confidence intervals"
  }
  
  # Create the plot
  p <- ggplot(overall_data, aes(x = effect_size, y = y_pos, shape = data_type)) +
    
    # Add reference line at x = 0
    geom_vline(xintercept = 0, linetype = "dotted", color = "black", alpha = 0.7) +
    
    # Add error bars
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                   height = 0.15, linewidth = line_width, alpha = 0.8, color = line_color) +
    
    # Add points
    geom_point(size = point_size, alpha = 0.9, color = colors) +
    
    # Set point shapes
    scale_shape_manual(values = shapes, name = "Data Type") +
    
    # Set y-axis
    scale_y_continuous(
      breaks = 1:4,
      labels = levels(overall_data$extremism_type),
      limits = c(0.5, 4.5)
    ) +
    
    # Facet by event
    facet_wrap(~ event, ncol = ncol_facets, scales = "free_x") +
    
    # Labels and theme
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Effect Size (Post - Pre)",
      y = "Extremism Dimension",
      caption = plot_note
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = font_size_title, face = "bold"),
      plot.subtitle = element_text(size = font_size_labels),
      axis.title = element_text(size = font_size_labels),
      axis.text = element_text(size = font_size_axes),
      legend.title = element_text(size = font_size_labels, face = "bold"),
      legend.text = element_text(size = font_size_axes),
      strip.text = element_text(size = font_size_axes, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  # Add effect values and significance stars if requested
  if (show_values) {
    # Create labels with effect size and stars
    overall_data$effect_label <- paste0(sprintf("%.2f", overall_data$effect_size), 
                                        ifelse(overall_data$stars != "", 
                                               paste0(" ", overall_data$stars), ""))
    
    p <- p + geom_text(data = overall_data, 
                       aes(x = effect_size, y = y_pos + 0.15, label = effect_label),
                       inherit.aes = FALSE, size = 3.5, color = "red", 
                       fontface = "bold")
  }
  
  return(p)
}