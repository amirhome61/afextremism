#' Calculate Relative Extremism Change Index (RECI)
#'
#' @description
#' Calculates the Relative Extremism Change Index (RECI), which measures the relationship 
#' between community extremism level changes and population normative point changes.
#' RECI indicates whether a community's internal extremist composition is growing or 
#' shrinking relative to population shifts.
#'
#' @param current_el Numeric. Current extremism level value.
#' @param previous_el Numeric. Previous extremism level value.
#' @param current_np Numeric. Current normative point value.
#' @param previous_np Numeric. Previous normative point value.
#' @param standardize Logical. Whether to return the standardized RECI-S value. Default is TRUE.
#'
#' @return A list containing:
#'   \item{reci}{The basic RECI value}
#'   \item{reci_s}{The standardized RECI-S value (if standardize=TRUE)}
#'   \item{el_percent_change}{Percentage change in extremism level}
#'   \item{np_percent_change}{Percentage change in normative point}
#'   \item{category}{Categorical interpretation of the RECI value}
#'
#' @details
#' RECI = EL_percent_change - NP_percent_change
#' RECI-S = RECI / (|EL_percent_change| + |NP_percent_change| + 1)
#'
#' Interpretation of RECI:
#' - RECI > 0: Community extremism is growing faster than the population norm
#' - RECI = 0: Community extremism is changing at same rate as population norm
#' - RECI < 0: Community extremism is growing slower than the population norm
#'
#' @examples
#' # Calculate RECI for cognitive dimension between two waves
#' af_calculate_reci(25.5, 20.1, 1.8, 1.5)
#'
#' # Calculate RECI without standardization
#' af_calculate_reci(25.5, 20.1, 1.8, 1.5, standardize = FALSE)
#'
#' @export
af_calculate_reci <- function(current_el, previous_el, current_np, previous_np, standardize = TRUE) {
  # Input validation
  if (!is.numeric(current_el) || !is.numeric(previous_el) || 
      !is.numeric(current_np) || !is.numeric(previous_np)) {
    stop("All input values must be numeric")
  }
  
  if (previous_el == 0) {
    warning("previous_el is zero, cannot calculate percentage change")
    el_percent_change <- NA
  } else {
    el_percent_change <- (current_el - previous_el) / previous_el * 100
  }
  
  if (previous_np == 0) {
    warning("previous_np is zero, cannot calculate percentage change")
    np_percent_change <- NA
  } else {
    np_percent_change <- (current_np - previous_np) / previous_np * 100
  }
  
  # Calculate basic RECI
  reci <- el_percent_change - np_percent_change
  
  # Calculate standardized RECI if requested
  reci_s <- NA
  if (standardize && !is.na(el_percent_change) && !is.na(np_percent_change)) {
    reci_s <- reci / (abs(el_percent_change) + abs(np_percent_change) + 1)
  }
  
  # Determine category based on RECI value
  category <- NA
  if (!is.na(reci)) {
    category <- if (reci > 10) {
      "Substantial Increase in Relative Extremism"
    } else if (reci > 5) {
      "Moderate Increase in Relative Extremism"
    } else if (reci > 2) {
      "Slight Increase in Relative Extremism"
    } else if (reci >= -2) {
      "Stable Relative Extremism"
    } else if (reci >= -5) {
      "Slight Decrease in Relative Extremism"
    } else if (reci >= -10) {
      "Moderate Decrease in Relative Extremism"
    } else {
      "Substantial Decrease in Relative Extremism"
    }
  }
  
  # Return all calculated values as a list
  result <- list(
    reci = reci,
    reci_s = reci_s,
    el_percent_change = el_percent_change,
    np_percent_change = np_percent_change,
    category = category
  )
  
  return(result)
}

#' Calculate RECI for Multiple Communities and Dimensions
#'
#' @description
#' Applies the Relative Extremism Change Index (RECI) calculation across multiple
#' communities and extremism dimensions by comparing indices between two time periods.
#'
#' @param current_data Data frame containing current period extremism indices.
#' @param previous_data Data frame containing previous period extremism indices.
#' @param community_variable Character. Name of the column identifying communities.
#' @param communities Character vector of community names to analyze. If NULL, all communities will be analyzed.
#' @param dimensions Character vector of dimension prefixes (e.g., "c", "b", "s", "o") to analyze.
#'                   Default is c("c", "b", "s", "o") for cognitive, behavioral, social, and overall.
#' @param standardize Logical. Whether to calculate standardized RECI-S values. Default is TRUE.
#'
#' @return A data frame with RECI values for each community and dimension combination with columns:
#'   \item{community}{The community name}
#'   \item{dimension}{The dimension analyzed (cognitive, behavioral, social, overall)}
#'   \item{current_el}{Current extremism level value}
#'   \item{previous_el}{Previous extremism level value}
#'   \item{current_np}{Current normative point value}
#'   \item{previous_np}{Previous normative point value}
#'   \item{el_percent_change}{Percentage change in extremism level}
#'   \item{np_percent_change}{Percentage change in normative point}
#'   \item{reci}{Basic RECI value}
#'   \item{reci_s}{Standardized RECI-S value (if standardize=TRUE)}
#'   \item{category}{Categorical interpretation of the RECI value}
#'
#' @details
#' This function takes two data frames representing different time periods and calculates
#' RECI values for each community-dimension combination. It extracts extremism levels (el)
#' and normative points (np) for each dimension and applies the af_calculate_reci function.
#'
#' The function expects data frames with columns named following the pattern:
#' - <dimension>el_c (e.g., cel_c, bel_c, sel_c, oel_c) for community extremism levels
#' - <dimension>np_p (e.g., cnp_p, bnp_p, snp_p, onp_p) for population normative points
#' - column identifying the community
#'
#' @examples
#' # Assuming 'wave1_data' and 'wave2_data' are data frames with extremism indices
#' results <- af_calculate_community_reci(wave2_data, wave1_data, "community")
#'
#' # For specific communities and dimensions
#' results <- af_calculate_community_reci(
#'   wave2_data, wave1_data, "community",
#'   communities = c("Secular", "Religious"),
#'   dimensions = c("c", "o")
#' )
#'
#' @export
af_calculate_community_reci <- function(current_data, previous_data, 
                                        community_variable, 
                                        communities = NULL, 
                                        dimensions = c("c", "b", "s", "o"), 
                                        standardize = TRUE) {
  # Input validation
  if (!is.data.frame(current_data) || !is.data.frame(previous_data)) {
    stop("current_data and previous_data must be data frames")
  }
  
  # Check if required columns exist in the data frames
  required_cols <- c(community_variable)
  for (dim in dimensions) {
    required_cols <- c(required_cols, paste0(dim, "el_c"), paste0(dim, "np_p"))
  }
  
  if (!all(required_cols %in% colnames(current_data)) || 
      !all(required_cols %in% colnames(previous_data))) {
    stop("Required columns missing in data frames. Need 'community_variable' and dimension columns.")
  }
  
  # Get list of communities if not provided
  if (is.null(communities)) {
    communities <- unique(current_data[[community_variable]][!is.na(current_data[[community_variable]])])
  }
  
  # Check if all specified communities exist in the data
  if (!all(communities %in% current_data[[community_variable]]) || 
      !all(communities %in% previous_data[[community_variable]])) {
    stop("Not all specified communities exist in both data frames")
  }
  
  # Initialize results data frame
  results <- data.frame()
  
  # Loop through communities and dimensions
  for (comm in communities) {
    # For filtering data for current community
    current_comm_data <- if (is.na(comm)) {
      current_data[is.na(current_data[[community_variable]]), ]
    } else {
      current_data[!is.na(current_data[[community_variable]]) & current_data[[community_variable]] == comm, ]
    }
    
    previous_comm_data <- if (is.na(comm)) {
      previous_data[is.na(previous_data[[community_variable]]), ]
    } else {
      previous_data[!is.na(previous_data[[community_variable]]) & previous_data[[community_variable]] == comm, ]
    }
    
    # Skip if no matching data
    if (nrow(current_comm_data) == 0 || nrow(previous_comm_data) == 0) {
      warning(paste("No data found for community:", comm))
      next
    }
    
    # For getting population normative points
    current_np_data <- current_data[is.na(current_data[[community_variable]]), ]
    previous_np_data <- previous_data[is.na(previous_data[[community_variable]]), ]
    
    # Loop through dimensions
    for (dim in dimensions) {
      el_col <- paste0(dim, "el_c")
      np_col <- paste0(dim, "np_p")
      
      # Extract values
      current_el <- current_comm_data[[el_col]][1]
      previous_el <- previous_comm_data[[el_col]][1]
      current_np <- current_np_data[[np_col]][1]
      previous_np <- previous_np_data[[np_col]][1]
      
      # Skip if any values are NA
      if (any(is.na(c(current_el, previous_el, current_np, previous_np)))) {
        warning(paste("Missing values for dimension", dim, "in community", comm))
        next
      }
      
      # Calculate RECI
      reci_result <- af_calculate_reci(
        current_el, previous_el, current_np, previous_np, standardize
      )
      
      # Map dimension code to full name
      dimension_name <- switch(dim,
                               "c" = "cognitive",
                               "b" = "behavioral",
                               "s" = "social",
                               "o" = "overall",
                               dim)
      
      # Add to results
      result_row <- data.frame(
        community = comm,
        dimension = dimension_name,
        current_el = current_el,
        previous_el = previous_el,
        current_np = current_np,
        previous_np = previous_np,
        el_percent_change = reci_result$el_percent_change,
        np_percent_change = reci_result$np_percent_change,
        reci = reci_result$reci,
        reci_s = reci_result$reci_s,
        category = reci_result$category
      )
      
      results <- rbind(results, result_row)
    }
  }
  
  return(results)
}

#' Create a RECI Heatmap Visualization
#'
#' @description
#' Creates a heatmap visualization of Relative Extremism Change Index (RECI) values
#' across different communities and wave transitions.
#'
#' @param reci_data Data frame containing RECI values as produced by af_calculate_community_reci.
#' @param wave_transitions Character vector specifying the wave transitions to include.
#' @param use_standardized Logical. Whether to use standardized RECI-S values. Default is TRUE.
#' @param communities Character vector of community names to include. If NULL, all communities will be included.
#' @param dimensions Character vector of dimensions to include. Default is all available dimensions.
#'
#' @return A ggplot2 object containing the RECI heatmap.
#'
#' @details
#' This function creates a heatmap visualization with wave transitions on the x-axis,
#' communities on the y-axis, and cells colored according to RECI or RECI-S values.
#' It uses a diverging color scale centered at zero.
#'
#' The function expects a data frame with columns:
#' - community: The name of the community
#' - dimension: The extremism dimension (cognitive, behavioral, social, overall)
#' - wave_transition: The transition between waves (e.g., "First-Second")
#' - reci: The RECI value
#' - reci_s: The standardized RECI-S value
#'
#' @examples
#' # Assuming 'reci_results' is a data frame with RECI values
#' af_plot_reci_heatmap(reci_results, 
#'                      wave_transitions = c("First-Second", "Second-Third"),
#'                      dimensions = "cognitive")
#'
#' @import ggplot2
#' @export
af_plot_reci_heatmap <- function(reci_data, wave_transitions = NULL, 
                                 use_standardized = TRUE, 
                                 communities = NULL,
                                 dimensions = NULL) {
  # Input validation
  if (!is.data.frame(reci_data)) {
    stop("reci_data must be a data frame")
  }
  
  required_cols <- c("community", "dimension", "wave_transition", "reci")
  if (use_standardized) {
    required_cols <- c(required_cols, "reci_s")
  }
  
  if (!all(required_cols %in% colnames(reci_data))) {
    stop("Required columns missing in reci_data")
  }
  
  # Filter data if parameters are provided
  if (!is.null(wave_transitions)) {
    reci_data <- reci_data[reci_data$wave_transition %in% wave_transitions, ]
  }
  
  if (!is.null(communities)) {
    reci_data <- reci_data[reci_data$community %in% communities, ]
  }
  
  if (!is.null(dimensions)) {
    reci_data <- reci_data[reci_data$dimension %in% dimensions, ]
  }
  
  # Check if there's data left after filtering
  if (nrow(reci_data) == 0) {
    stop("No data remains after filtering")
  }
  
  # Determine value column based on standardization preference
  value_col <- if (use_standardized) "reci_s" else "reci"
  value_label <- if (use_standardized) "RECI-S" else "RECI"
  
  # Set up color limits based on standardization
  if (use_standardized) {
    limits <- c(-1, 1)
  } else {
    # For non-standardized, use symmetric limits based on data range
    max_abs <- max(abs(reci_data$reci), na.rm = TRUE)
    limits <- c(-max_abs, max_abs)
  }
  
  # Create the heatmap for each dimension
  require(ggplot2)
  
  # Get unique dimensions
  unique_dims <- unique(reci_data$dimension)
  
  # Initialize a list to store plots
  plot_list <- list()
  
  for (dim in unique_dims) {
    dim_data <- reci_data[reci_data$dimension == dim, ]
    
    plot <- ggplot(dim_data, aes(x = wave_transition, y = community, fill = .data[[value_col]])) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "blue", 
        mid = "white", 
        high = "red", 
        midpoint = 0,
        limits = limits,
        name = value_label
      ) +
      geom_text(aes(label = sprintf("%.2f", .data[[value_col]])), size = 3) +
      labs(
        title = paste("RECI Heatmap -", toupper(substr(dim, 1, 1)), dim),
        x = "Wave Transition",
        y = "Community"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    plot_list[[dim]] <- plot
  }
  
  return(plot_list)
}

#' Create RECI Trend Visualization
#'
#' @description
#' Creates a line plot showing RECI trends across wave transitions for different
#' dimensions and communities.
#'
#' @param reci_data Data frame containing RECI values as produced by af_calculate_community_reci.
#' @param communities Character vector of community names to include. If NULL, all communities will be included.
#' @param use_standardized Logical. Whether to use standardized RECI-S values. Default is TRUE.
#' @param separate_plots Logical. Whether to create separate plots for each community. Default is TRUE.
#'
#' @return A list of ggplot2 objects containing the RECI trend plots.
#'
#' @details
#' This function creates line plots showing RECI or RECI-S trends across wave transitions,
#' with separate lines for each dimension. It can either create separate plots for each
#' community or combine all communities into a single plot.
#'
#' The function expects a data frame with columns:
#' - community: The name of the community
#' - dimension: The extremism dimension (cognitive, behavioral, social, overall)
#' - wave_transition: The transition between waves (e.g., "First-Second")
#' - reci: The RECI value
#' - reci_s: The standardized RECI-S value
#'
#' @examples
#' # Assuming 'reci_results' is a data frame with RECI values
#' plots <- af_plot_reci_trends(reci_results)
#'
#' # To combine all communities into a single plot
#' combined_plot <- af_plot_reci_trends(reci_results, separate_plots = FALSE)
#'
#' @import ggplot2
#' @export
af_plot_reci_trends <- function(reci_data, communities = NULL, 
                                use_standardized = TRUE, 
                                separate_plots = TRUE) {
  # Input validation
  if (!is.data.frame(reci_data)) {
    stop("reci_data must be a data frame")
  }
  
  required_cols <- c("community", "dimension", "wave_transition", "reci")
  if (use_standardized) {
    required_cols <- c(required_cols, "reci_s")
  }
  
  if (!all(required_cols %in% colnames(reci_data))) {
    stop("Required columns missing in reci_data")
  }
  
  # Filter data if communities parameter is provided
  if (!is.null(communities)) {
    reci_data <- reci_data[reci_data$community %in% communities, ]
  }
  
  # Check if there's data left after filtering
  if (nrow(reci_data) == 0) {
    stop("No data remains after filtering")
  }
  
  # Determine value column based on standardization preference
  value_col <- if (use_standardized) "reci_s" else "reci"
  value_label <- if (use_standardized) "RECI-S" else "RECI"
  
  # Create the trend plots
  require(ggplot2)
  
  # Get unique communities
  unique_communities <- unique(reci_data$community)
  
  # Initialize a list to store plots
  plot_list <- list()
  
  if (separate_plots) {
    # Create separate plots for each community
    for (comm in unique_communities) {
      comm_data <- reci_data[reci_data$community == comm, ]
      
      plot <- ggplot(comm_data, aes(x = wave_transition, y = .data[[value_col]], 
                                    color = dimension, group = dimension)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        labs(
          title = paste("RECI Trends -", comm),
          x = "Wave Transition",
          y = value_label,
          color = "Dimension"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom"
        )
      
      plot_list[[comm]] <- plot
    }
  } else {
    # Create a single plot with all communities
    plot <- ggplot(reci_data, aes(x = wave_transition, y = .data[[value_col]], 
                                  color = dimension, group = interaction(dimension, community),
                                  linetype = community)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title = "RECI Trends - All Communities",
        x = "Wave Transition",
        y = value_label,
        color = "Dimension",
        linetype = "Community"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    plot_list[["combined"]] <- plot
  }
  
  return(plot_list)
}

#' Prepare Data for RECI Analysis Across Waves
#'
#' @description
#' Processes a data frame containing extremism indices from multiple waves
#' and prepares it for RECI analysis by creating wave transition pairs.
#'
#' @param data Data frame containing extremism indices from multiple waves.
#' @param wave_column Character. The name of the column containing wave identifiers. Default is "Wave".
#' @param community_variable Character. Name of the column identifying communities.
#' @param communities Character vector of community names to analyze. If NULL, all communities will be analyzed.
#' @param dimensions Character vector of dimension prefixes to analyze. Default is c("c", "b", "s", "o").
#'
#' @return A data frame containing RECI values for each community-dimension-wave transition combination.
#'
#' @details
#' This function takes a data frame with extremism indices from multiple waves,
#' identifies consecutive wave pairs, and calculates RECI values for each transition.
#' It organizes the results into a format suitable for visualization with the
#' af_plot_reci_heatmap and af_plot_reci_trends functions.
#'
#' @examples
#' # Assuming 'indices_data' is a data frame with extremism indices from multiple waves
#' reci_data <- af_prepare_reci_data(indices_data, community_variable = "community")
#'
#' @export
af_prepare_reci_data <- function(data, wave_column = "Wave", 
                                 community_variable,
                                 communities = NULL, 
                                 dimensions = c("c", "b", "s", "o")) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (!(wave_column %in% colnames(data))) {
    stop(paste("Wave column", wave_column, "not found in data"))
  }
  
  # Get unique waves and sort them
  waves <- unique(data[[wave_column]])
  waves <- sort(waves)
  
  if (length(waves) < 2) {
    stop("At least two waves are required for RECI analysis")
  }
  
  # Get list of communities if not provided
  if (is.null(communities)) {
    communities <- unique(data[[community_variable]][!is.na(data[[community_variable]])])
  }
  
  # Initialize results data frame
  results <- data.frame()
  
  # Process each consecutive wave pair
  for (i in 1:(length(waves)-1)) {
    current_wave <- waves[i+1]
    previous_wave <- waves[i]
    
    # Filter data for current and previous waves
    current_data <- data[data[[wave_column]] == current_wave, ]
    previous_data <- data[data[[wave_column]] == previous_wave, ]
    
    # Calculate RECI for this wave transition
    transition_results <- af_calculate_community_reci(
      current_data, previous_data, 
      community_variable = community_variable,
      communities = communities,
      dimensions = dimensions
    )
    
    # Add wave transition information
    transition_results$wave_transition <- paste(previous_wave, current_wave, sep = "-")
    
    # Append to results
    results <- rbind(results, transition_results)
  }
  
  return(results)
}

#' Analyze Extremism Trends Using RECI
#'
#' @description
#' Performs a comprehensive RECI analysis on extremism indices data,
#' calculating RECI values and generating visualizations.
#'
#' @param data Data frame containing extremism indices from multiple waves.
#' @param wave_column Character. The name of the column containing wave identifiers. Default is "Wave".
#' @param community_variable Character. Name of the column identifying communities.
#' @param communities Character vector of community names to analyze. If NULL, all communities will be analyzed.
#' @param dimensions Character vector of dimension prefixes to analyze. Default is c("c", "b", "s", "o").
#' @param use_standardized Logical. Whether to use standardized RECI-S values. Default is TRUE.
#' @param output_dir Character. Directory to save visualizations. If NULL, visualizations are returned but not saved.
#'
#' @return A list containing:
#'   \item{reci_data}{Data frame with calculated RECI values}
#'   \item{heatmaps}{List of RECI heatmap plots}
#'   \item{trend_plots}{List of RECI trend plots}
#'   \item{summary_stats}{Summary statistics of RECI values}
#'
#' @details
#' This function performs a complete RECI analysis workflow:
#' 1. Prepares the data for analysis by identifying wave transitions
#' 2. Calculates RECI values for each community-dimension-transition combination
#' 3. Creates heatmap visualizations showing RECI patterns
#' 4. Creates trend plots showing RECI changes over time
#' 5. Calculates summary statistics for the RECI values
#'
#' The function returns all results as a list and can optionally save visualizations.
#'
#' @examples
#' # Assuming 'indices_data' is a data frame with extremism indices from multiple waves
#' results <- af_analyze_extremism_trends(indices_data, community_variable = "community")
#'
#' # To analyze specific communities and dimensions
#' results <- af_analyze_extremism_trends(
#'   indices_data,
#'   community_variable = "community",
#'   communities = c("Secular", "Religious"),
#'   dimensions = c("c", "o")
#' )
#'
#' @import ggplot2
#' @export
af_analyze_extremism_trends <- function(data, wave_column = "Wave",
                                        community_variable,
                                        communities = NULL,
                                        dimensions = c("c", "b", "s", "o"),
                                        use_standardized = TRUE,
                                        output_dir = NULL) {
  # Prepare data for RECI analysis
  reci_data <- af_prepare_reci_data(
    data, 
    wave_column = wave_column,
    community_variable = community_variable,
    communities = communities,
    dimensions = dimensions
  )
  
  # Create heatmaps
  heatmaps <- af_plot_reci_heatmap(
    reci_data,
    use_standardized = use_standardized,
    communities = communities
  )
  
  # Create trend plots
  individual_trend_plots <- af_plot_reci_trends(
    reci_data,
    communities = communities,
    use_standardized = use_standardized,
    separate_plots = TRUE
  )
  
  combined_trend_plot <- af_plot_reci_trends(
    reci_data,
    communities = communities,
    use_standardized = use_standardized,
    separate_plots = FALSE
  )
  
  # Combine all trend plots
  trend_plots <- c(individual_trend_plots, combined = combined_trend_plot)
  
  # Calculate summary statistics
  value_col <- if (use_standardized) "reci_s" else "reci"
  
  summary_by_community <- aggregate(
    reci_data[[value_col]], 
    by = list(community = reci_data$community), 
    FUN = function(x) c(
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  )
  
  summary_by_dimension <- aggregate(
    reci_data[[value_col]], 
    by = list(dimension = reci_data$dimension), 
    FUN = function(x) c(
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  )
  
  summary_stats <- list(
    by_community = summary_by_community,
    by_dimension = summary_by_dimension,
    overall = c(
      mean = mean(reci_data[[value_col]], na.rm = TRUE),
      median = median(reci_data[[value_col]], na.rm = TRUE),
      sd = sd(reci_data[[value_col]], na.rm = TRUE),
      min = min(reci_data[[value_col]], na.rm = TRUE),
      max = max(reci_data[[value_col]], na.rm = TRUE)
    )
  )
  
  # Save visualizations if output_dir is provided
  if (!is.null(output_dir)) {
    # Create directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Save heatmaps
    for (dim in names(heatmaps)) {
      ggsave(
        file.path(output_dir, paste0("reci_heatmap_", dim, ".png")),
        plot = heatmaps[[dim]],
        width = 10,
        height = 8,
        dpi = 300
      )
    }
    
    # Save individual trend plots
    for (comm in names(individual_trend_plots)) {
      ggsave(
        file.path(output_dir, paste0("reci_trend_", gsub(" ", "_", tolower(comm)), ".png")),
        plot = individual_trend_plots[[comm]],
        width = 10,
        height = 6,
        dpi = 300
      )
    }
    
    # Save combined trend plot
    ggsave(
      file.path(output_dir, "reci_trend_combined.png"),
      plot = combined_trend_plot[["combined"]],
      width = 12,
      height = 8,
      dpi = 300
    )
    
    # Save summary statistics
    write.csv(
      as.data.frame(do.call(rbind, summary_by_community$x)),
      file.path(output_dir, "reci_summary_by_community.csv"),
      row.names = FALSE
    )
    
    write.csv(
      as.data.frame(do.call(rbind, summary_by_dimension$x)),
      file.path(output_dir, "reci_summary_by_dimension.csv"),
      row.names = FALSE
    )
  }
  
  # Return all results as a list
  return(list(
    reci_data = reci_data,
    heatmaps = heatmaps,
    trend_plots = trend_plots,
    summary_stats = summary_stats
  ))
}

#' Create RECI Comparison Table
#'
#' @description
#' Creates a formatted table showing Extremism Level (EL) changes, Normative Point (NP) changes,
#' RECI values, and interpretation patterns for each community-dimension combination.
#'
#' @param reci_data Data frame containing RECI values as produced by af_prepare_reci_data.
#' @param wave_transition Character. The specific wave transition to analyze (e.g., "First-Second").
#' @param dimensions Character vector of dimensions to include. Default is NULL (all dimensions).
#' @param use_standardized Logical. Whether to include standardized RECI-S values. Default is TRUE.
#'
#' @return A data frame formatted as a RECI comparison table.
#'
#' @details
#' This function creates a table with the following format for each cell:
#' - EL: <el_change>%
#' - NP: <np_change>%
#' - RECI: <reci_value> (reci-s value)
#' - <pattern>
#'
#' The pattern is the categorical interpretation of the RECI value.
#'
#' @examples
#' # Assuming 'reci_data' is a data frame with RECI values
#' table <- af_create_reci_table(reci_data, wave_transition = "First-Second")
#'
#' @export
af_create_reci_table <- function(reci_data, wave_transition,
                                 dimensions = NULL,
                                 use_standardized = TRUE) {
  # Input validation
  if (!is.data.frame(reci_data)) {
    stop("reci_data must be a data frame")
  }
  
  if (!(wave_transition %in% reci_data$wave_transition)) {
    stop(paste("Wave transition", wave_transition, "not found in reci_data"))
  }
  
  # Filter data for the specified wave transition
  filtered_data <- reci_data[reci_data$wave_transition == wave_transition, ]
  
  # Filter by dimensions if specified
  if (!is.null(dimensions)) {
    filtered_data <- filtered_data[filtered_data$dimension %in% dimensions, ]
  }
  
  # Check if there's data left after filtering
  if (nrow(filtered_data) == 0) {
    stop("No data remains after filtering")
  }
  
  # Get unique dimensions and communities
  unique_dimensions <- unique(filtered_data$dimension)
  unique_communities <- unique(filtered_data$community)
  
  # Initialize the result table
  result_table <- matrix(
    NA, 
    nrow = length(unique_communities), 
    ncol = length(unique_dimensions)
  )
  
  rownames(result_table) <- unique_communities
  colnames(result_table) <- unique_dimensions
  
  # Fill the table
  for (i in 1:nrow(filtered_data)) {
    row <- filtered_data[i, ]
    comm <- row$community
    dim <- row$dimension
    
    el_change <- sprintf("%.2f", row$el_percent_change)
    np_change <- sprintf("%.2f", row$np_percent_change)
    reci_value <- sprintf("%.2f", row$reci)
    
    # Include RECI-S if requested
    if (use_standardized) {
      reci_s_value <- sprintf("%.2f", row$reci_s)
      reci_text <- paste0(reci_value, " (", reci_s_value, ")")
    } else {
      reci_text <- reci_value
    }
    
    # Format the cell content
    cell_content <- paste0(
      "EL: ", el_change, "%\n",
      "NP: ", np_change, "%\n",
      "RECI: ", reci_text, "\n",
      row$category
    )
    
    # Add to the table
    comm_idx <- which(unique_communities == comm)
    dim_idx <- which(unique_dimensions == dim)
    result_table[comm_idx, dim_idx] <- cell_content
  }
  
  # Convert to data frame
  result_df <- as.data.frame(result_table, stringsAsFactors = FALSE)
  result_df <- cbind(Community = rownames(result_table), result_df)
  
  return(result_df)
}

#' Calculate Relative Extremism Change Index (RECI) for Population Data
#'
#' @description
#' Calculates the Relative Extremism Change Index (RECI) for the overall population
#' between two time periods for all extremism dimensions.
#'
#' @param current_data Data frame containing current period extremism indices.
#' @param previous_data Data frame containing previous period extremism indices.
#' @param community_variable Character. Name of the column identifying communities.
#' @param dimensions Character vector of dimension prefixes to analyze. Default is c("c", "b", "s", "o").
#' @param standardize Logical. Whether to calculate standardized RECI-S values. Default is TRUE.
#'
#' @return A data frame with RECI values for each dimension at the population level.
#'
#' @details
#' This function calculates RECI values for the overall population between two time periods.
#' For population-level analysis, EL and NP come from the same source (_p suffix columns),
#' but we maintain the RECI formula (EL_change - NP_change) for consistency.
#'
#' @examples
#' # Calculate population RECI between two waves
#' result <- af_calculate_population_reci(wave2_data, wave1_data, "community")
#'
#' @export
af_calculate_population_reci <- function(current_data, previous_data,
                                         community_variable,
                                         dimensions = c("c", "b", "s", "o"),
                                         standardize = TRUE) {
  # Input validation
  if (!is.data.frame(current_data) || !is.data.frame(previous_data)) {
    stop("current_data and previous_data must be data frames")
  }
  
  # Filter data for population (where community is NA)
  current_pop_data <- current_data[is.na(current_data[[community_variable]]), ]
  previous_pop_data <- previous_data[is.na(previous_data[[community_variable]]), ]
  
  # Check if population data exists
  if (nrow(current_pop_data) == 0 || nrow(previous_pop_data) == 0) {
    stop("Population data (rows with NA community) not found in input data")
  }
  
  # Define dimension names
  dimension_names <- c("cognitive", "behavioral", "social", "overall")
  names(dimension_names) <- dimensions
  
  # Initialize results data frame
  results <- data.frame(
    dimension = character(),
    current_el = numeric(),
    previous_el = numeric(),
    current_np = numeric(),
    previous_np = numeric(),
    el_percent_change = numeric(),
    np_percent_change = numeric(),
    reci = numeric(),
    reci_s = numeric(),
    category = character(),
    stringsAsFactors = FALSE
  )
  
  # Calculate RECI for each dimension
  for (dim in dimensions) {
    # For population, we use el_p for EL and np_p for NP
    el_col <- paste0(dim, "el_p")
    np_col <- paste0(dim, "np_p")
    
    # Extract values
    current_el <- current_pop_data[[el_col]][1]
    previous_el <- previous_pop_data[[el_col]][1]
    current_np <- current_pop_data[[np_col]][1]
    previous_np <- previous_pop_data[[np_col]][1]
    
    # Skip if any values are NA
    if (any(is.na(c(current_el, previous_el, current_np, previous_np)))) {
      warning(paste("Missing values for dimension", dim, "in population data"))
      next
    }
    
    # Calculate percent changes
    if (previous_el == 0) {
      warning(paste("previous_el is zero for dimension", dim, "- cannot calculate percentage change"))
      el_pct_change <- NA
    } else {
      el_pct_change <- (current_el - previous_el) / previous_el * 100
    }
    
    if (previous_np == 0) {
      warning(paste("previous_np is zero for dimension", dim, "- cannot calculate percentage change"))
      np_pct_change <- NA
    } else {
      np_pct_change <- (current_np - previous_np) / previous_np * 100
    }
    
    # Calculate RECI
    reci <- el_pct_change - np_pct_change
    
    # Calculate standardized RECI if requested
    reci_s <- NA
    if (standardize && !is.na(el_pct_change) && !is.na(np_pct_change)) {
      reci_s <- reci / (abs(el_pct_change) + abs(np_pct_change) + 1)
    }
    
    # Determine category based on RECI value
    category <- NA
    if (!is.na(reci)) {
      category <- if (reci > 10) {
        "Substantial Increase in Relative Extremism"
      } else if (reci > 5) {
        "Moderate Increase in Relative Extremism"
      } else if (reci > 2) {
        "Slight Increase in Relative Extremism"
      } else if (reci >= -2) {
        "Stable Relative Extremism"
      } else if (reci >= -5) {
        "Slight Decrease in Relative Extremism"
      } else if (reci >= -10) {
        "Moderate Decrease in Relative Extremism"
      } else {
        "Substantial Decrease in Relative Extremism"
      }
    }
    
    # Add to results
    dim_name <- dimension_names[dim]
    results <- rbind(results, data.frame(
      dimension = dim_name,
      current_el = current_el,
      previous_el = previous_el,
      current_np = current_np,
      previous_np = previous_np,
      el_percent_change = el_pct_change,
      np_percent_change = np_pct_change,
      reci = reci,
      reci_s = reci_s,
      category = category,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

#' Prepare Population RECI Data Across Waves
#'
#' @description
#' Processes a data frame containing extremism indices from multiple waves
#' and calculates population-level RECI for each wave transition.
#'
#' @param data Data frame containing extremism indices from multiple waves.
#' @param wave_column Character. The name of the column containing wave identifiers. Default is "Wave".
#' @param community_variable Character. Name of the column identifying communities.
#' @param dimensions Character vector of dimension prefixes to analyze. Default is c("c", "b", "s", "o").
#'
#' @return A data frame containing population RECI values for each dimension-wave transition combination.
#'
#' @details
#' This function takes a data frame with extremism indices from multiple waves,
#' identifies consecutive wave pairs, and calculates population-level RECI values for each transition.
#'
#' @examples
#' # Assuming 'indices_data' is a data frame with extremism indices from multiple waves
#' pop_reci_data <- af_prepare_population_reci_data(indices_data, community_variable = "community")
#'
#' @export
af_prepare_population_reci_data <- function(data, wave_column = "Wave", 
                                            community_variable,
                                            dimensions = c("c", "b", "s", "o")) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (!(wave_column %in% colnames(data))) {
    stop(paste("Wave column", wave_column, "not found in data"))
  }
  
  # Get unique waves and sort them
  waves <- unique(data[[wave_column]])
  waves <- sort(waves)
  
  if (length(waves) < 2) {
    stop("At least two waves are required for RECI analysis")
  }
  
  # Initialize results data frame
  results <- data.frame()
  
  # Process each consecutive wave pair
  for (i in 1:(length(waves)-1)) {
    current_wave <- waves[i+1]
    previous_wave <- waves[i]
    
    # Filter data for current and previous waves
    current_data <- data[data[[wave_column]] == current_wave, ]
    previous_data <- data[data[[wave_column]] == previous_wave, ]
    
    # Calculate Population RECI for this wave transition
    transition_results <- af_calculate_population_reci(
      current_data, previous_data,
      community_variable = community_variable,
      dimensions = dimensions
    )
    
    # Add wave transition information
    transition_results$wave_transition <- paste(previous_wave, current_wave, sep = "-")
    
    # Append to results
    results <- rbind(results, transition_results)
  }
  
  return(results)
}