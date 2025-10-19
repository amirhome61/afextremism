#' Calculate Political Extremism Gauge Indices
#'
#' @param df A data frame containing the survey dataset
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
#' @param dimensions Character vector of exactly 3 variable names corresponding to cognitive, behavioral, and social dimensions
#' @param threshold_type Character string specifying the threshold calculation method. Must be either "MAD", "Qn", "Sn" or "Tau". Default is "MAD".
#' @param k_factor Numeric value specifying the multiplier for MAD method. Must be positive. Default is 2.
#'
#' @return A list containing two elements:
#'   \item{df}{The original data frame with added columns for the calculated indices}
#'   \item{indices_table}{A data frame with one row per population/community combination and their calculated indices}
#'
#' @export
af_gauge_indices <- function(df, 
                             pop_var1 = NULL, pop_levels1 = NULL,
                             pop_var2 = NULL, pop_levels2 = NULL,
                             pop_var3 = NULL, pop_levels3 = NULL,
                             comm_var1 = NULL, comm_levels1 = NULL,
                             comm_var2 = NULL, comm_levels2 = NULL,
                             dimensions = c("pe_ideology", "pe_violence", "pe_intolerance"),
                             threshold_type = "MAD",
                             k_factor = 1.5) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  if (length(dimensions) != 3) {
    stop("dimensions must be a character vector of exactly 3 elements")
  }
  
  for (dim in dimensions) {
    if (!dim %in% names(df)) {
      stop(paste("Dimension variable", dim, "not found in the data frame"))
    }
    if (!is.numeric(df[[dim]])) {
      stop(paste("Dimension variable", dim, "must be numeric"))
    }
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
  
  # Validate threshold parameters
  if (!is.character(threshold_type) || length(threshold_type) != 1) {
    stop("threshold_type must be a single character string")
  }
  
  if (!threshold_type %in% c("MAD", "Sn", "Qn", "Tau" )) {
    stop("threshold_type must be either 'MAD', 'Sn', 'Qn' or 'Tau'")
  }
  
  if (!is.numeric(k_factor) || length(k_factor) != 1) {
    stop("k_factor must be a single numeric value")
  }
  
  if (is.na(k_factor)) {
    stop("k_factor cannot be NA")
  }
  
  if (k_factor <= 0) {
    stop("k_factor must be positive")
  }
  
  # Initialize results
  result_df <- df
  indices_table <- NULL  # We'll create this properly later
  
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
      filter = rep(TRUE, nrow(df))
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
          
          # Save this combination
          pop_combinations[[pop_combo_idx]] <<- list(
            vars = curr_combo,
            filter = filter_expr
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
  
  # Create a list to store all rows for the indices table
  all_indices_rows <- list()
  row_idx <- 1
  
  # Process each population combination
  for (pop_idx in seq_along(pop_combinations)) {
    pop_combo <- pop_combinations[[pop_idx]]
    pop_filter <- pop_combo$filter
    pop_df <- df[pop_filter, ]
    
    if (nrow(pop_df) == 0) {
      next  # Skip empty populations
    }
    
    # Calculate population indices using the new standalone function
    pop_indices <- af_calculate_indices(
      group_df = pop_df,
      dimensions = dimensions,
      is_community = FALSE,
      threshold_type = threshold_type,
      k_factor = k_factor
    )
    
    # Create row for indices table
    pop_row <- list()
    
    # Add population variables to row
    if (!is.null(pop_combo$vars)) {
      for (var_name in names(pop_combo$vars)) {
        pop_row[[var_name]] <- pop_combo$vars[[var_name]]
      }
    }
    
    # Add population indices to row
    for (idx_name in names(pop_indices)) {
      pop_row[[idx_name]] <- pop_indices[[idx_name]]
    }
    
    # Add population size and set community size to 0
    pop_row[["np"]] <- nrow(pop_df)
    pop_row[["nc"]] <- 0
    
    # Add to indices table rows
    all_indices_rows[[row_idx]] <- pop_row
    row_idx <- row_idx + 1
    
    # Add index values to result dataframe (excluding np and nc)
    for (idx_name in names(pop_indices)) {
      result_df[pop_filter, idx_name] <- pop_indices[[idx_name]]
    }
    
    # Process communities if defined
    if (length(comm_vars) > 0) {
      # Create community combinations for this population based on specified levels
      comm_combinations <- list()
      comm_combo_idx <- 1
      
      # Function to recursively build community filter combinations with specified levels
      build_comm_combinations <- function(curr_combo = list(), curr_depth = 1) {
        if (curr_depth > length(comm_vars)) {
          if (length(curr_combo) > 0) {
            # Build filter expression
            filter_expr <- rep(TRUE, nrow(pop_df))
            vars_used <- names(curr_combo)
            
            for (var in vars_used) {
              filter_expr <- filter_expr & (pop_df[[var]] %in% curr_combo[[var]])
            }
            
            # Save this combination
            comm_combinations[[comm_combo_idx]] <<- list(
              vars = curr_combo,
              filter = filter_expr
            )
            comm_combo_idx <<- comm_combo_idx + 1
          }
          return()
        }
        
        # Current variable and its levels
        curr_var <- comm_vars[curr_depth]
        
        # Use specified levels or fallback to what's available in the population
        if (curr_depth == 1 && !is.null(comm_levels1)) {
          # Filter community levels to only include those present in the current population
          available_levels <- intersect(comm_levels1, unique(pop_df[[curr_var]]))
          if (length(available_levels) == 0) {
            available_levels <- unique(pop_df[[curr_var]])
          }
        } else if (curr_depth == 2 && !is.null(comm_levels2)) {
          # Filter community levels to only include those present in the current population
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
      
      # Process each community combination
      for (comm_idx in seq_along(comm_combinations)) {
        comm_combo <- comm_combinations[[comm_idx]]
        comm_filter_local <- comm_combo$filter
        
        # Convert local filter (based on pop_df indices) to global filter (based on df indices)
        pop_indices_in_df <- which(pop_filter)
        comm_indices_in_pop <- which(comm_filter_local)
        
        if (length(comm_indices_in_pop) == 0) {
          next  # Skip empty communities
        }
        
        comm_indices_in_df <- pop_indices_in_df[comm_indices_in_pop]
        comm_filter_global <- rep(FALSE, nrow(df))
        comm_filter_global[comm_indices_in_df] <- TRUE
        
        comm_df <- df[comm_filter_global, ]
        
        if (nrow(comm_df) == 0) {
          next  # Skip empty communities
        }
        
        # Calculate community indices using the new standalone function
        comm_indices <- af_calculate_indices(
          group_df = comm_df,
          dimensions = dimensions,
          is_community = TRUE,
          parent_indices = pop_indices,
          threshold_type = threshold_type,
          k_factor = k_factor
        )
        
        # Create row for indices table
        comm_row <- list()
        
        # Add population variables to row
        if (!is.null(pop_combo$vars)) {
          for (var_name in names(pop_combo$vars)) {
            comm_row[[var_name]] <- pop_combo$vars[[var_name]]
          }
        }
        
        # Add community variables to row
        for (var_name in names(comm_combo$vars)) {
          comm_row[[var_name]] <- comm_combo$vars[[var_name]]
        }
        
        # Add community indices to row
        for (idx_name in names(comm_indices)) {
          comm_row[[idx_name]] <- comm_indices[[idx_name]]
        }
        
        # Add population size and community size
        comm_row[["np"]] <- nrow(pop_df)
        comm_row[["nc"]] <- nrow(comm_df)
        
        # Add to indices table rows
        all_indices_rows[[row_idx]] <- comm_row
        row_idx <- row_idx + 1
        
        # Add index values to result dataframe (excluding np and nc)
        for (idx_name in names(comm_indices)) {
          result_df[comm_filter_global, idx_name] <- comm_indices[[idx_name]]
        }
      }
    }
  }
  
  # Create the indices table from the rows
  if (length(all_indices_rows) > 0) {
    # Get all possible column names
    all_cols <- unique(unlist(lapply(all_indices_rows, names)))
    
    # Create an empty data frame with all needed columns
    indices_table <- data.frame(matrix(NA, ncol = length(all_cols), nrow = length(all_indices_rows)))
    colnames(indices_table) <- all_cols
    
    # Fill in the data from our rows
    for (i in seq_along(all_indices_rows)) {
      row_data <- all_indices_rows[[i]]
      for (col in names(row_data)) {
        indices_table[i, col] <- row_data[[col]]
      }
    }
  } else {
    indices_table <- data.frame()
  }
  
  # Add Individual Inidces (for each respondent)
  result_df <- af_calculate_individual_indices(result_df, dimensions)
  
  # Return results
  return(list(
    df = result_df,
    indices_table = indices_table
  ))
}

#' Calculate Extremism Indices for a Group
#'
#' This function calculates various extremism indices (normative points, extremism points,
#' extremism levels, and extremism ranks) for a given group of observations. It can handle
#' both population-level calculations (comparing to own thresholds) and community-level
#' calculations (comparing to parent population thresholds).
#'
#' @param group_df A data frame containing the group data for which to calculate indices.
#'   Must contain the three dimension columns specified in dimensions parameter.
#' @param dimensions Character vector of exactly 3 variable names corresponding to 
#'   cognitive, behavioral, and social dimensions. All must be numeric columns in group_df.
#' @param is_community Logical value indicating whether this is a community calculation.
#'   If TRUE, parent_indices must be provided. Default is FALSE.
#' @param parent_indices Named list containing parent population indices. Required when
#'   is_community = TRUE. Must contain elements: cep_p, bep_p, sep_p, oep_p.
#' @param threshold_type Character string specifying the threshold calculation method.
#'   Must be either "MAD", "Sn", "Qn" or "Tau". Default is "MAD".
#' @param k_factor Numeric value specifying the multiplier for MAD method.
#'   Must be positive. Default is 2.
#'
#' @return Named list containing calculated indices with appropriate suffix (_p for 
#'   population, _c for community):
#'   \itemize{
#'     \item \strong{Normative Points}: cnp, bnp, snp, onp (medians of dimensions)
#'     \item \strong{Extremism Points}: cep, bep, sep, oep (threshold values)
#'     \item \strong{Extremism Levels}: cel, bel, sel, oel (percentage exceeding thresholds)
#'     \item \strong{Extremism Ranks}: er1, er2, er3 (percentage exceeding 1+, 2+, 3+ thresholds)
#'   }
#'
#' @details
#' The function calculates:
#' \itemize{
#'   \item \strong{Normative Points}: Median values for each dimension
#'   \item \strong{Extremism Points}: Threshold values using specified method (MAD, Sn, Qn or Tau)
#'   \item \strong{Extremism Levels}: Percentage of observations exceeding each threshold
#'   \item \strong{Extremism Ranks}: Percentage exceeding multiple thresholds simultaneously
#' }
#' 
#' For population calculations (is_community = FALSE), thresholds are calculated from
#' the group's own data. For community calculations (is_community = TRUE), thresholds
#' are taken from the parent population indices.
#'
#' @examples
#' comm_indices <- af_calculate_indices(
#'   group_df = comm_subset,
#'   dimensions = c("pe_ideology", "pe_violence", "pe_intolerance"),
#'   is_community = TRUE,
#'   parent_indices = pop_indices,
#'   threshold_type = "MAD"
#' )
#'
#' @export
af_calculate_indices <- function(group_df, 
                                 dimensions, 
                                 is_community = FALSE, 
                                 parent_indices = NULL,
                                 threshold_type = "MAD",
                                 k_factor = 1.5) {
  
  if (!is.data.frame(group_df)) {
    stop("group_df must be a data frame")
  }
  
  # Input validation for parent_indices when community calculation
  if (is_community) {
    if (is.null(parent_indices)) {
      stop("parent_indices must be provided when is_community = TRUE")
    }
    
    # Update the validation to include all required indices
    required_indices <- c("cnp_p", "bnp_p", "snp_p", "onp_p", "cep_p", "bep_p", "sep_p", "oep_p")
    missing_indices <- required_indices[!required_indices %in% names(parent_indices)]
    
    if (length(missing_indices) > 0) {
      stop(paste("parent_indices must contain:", paste(missing_indices, collapse = ", ")))
    }
  }
  
  # Determine suffix based on whether it's population or community
  suffix <- if (is_community) "_c" else "_p"
  
  # Initialize indices list
  indices <- list()
  
  # Extract dimension values
  cog_dim <- group_df[[dimensions[1]]]
  beh_dim <- group_df[[dimensions[2]]]
  soc_dim <- group_df[[dimensions[3]]]
  ova_dim <- af_dist(group_df, dimensions) 
  
  # Filter out NA values
  valid_cog <- !is.na(cog_dim)
  valid_beh <- !is.na(beh_dim)
  valid_soc <- !is.na(soc_dim)
  valid_ova <- !is.na(ova_dim)
  
  # Check if we have enough valid data
  if (sum(valid_cog) < 2 || sum(valid_beh) < 2 || 
      sum(valid_soc) < 2 || sum(valid_ova) < 2) {
    stop("Each dimension must have at least 2 non-missing values")
  }
  
  # Calculate Normative Points (medians)
  indices[[paste0("cnp", suffix)]] <- median(cog_dim[valid_cog], na.rm = TRUE)
  indices[[paste0("bnp", suffix)]] <- median(beh_dim[valid_beh], na.rm = TRUE)
  indices[[paste0("snp", suffix)]] <- median(soc_dim[valid_soc], na.rm = TRUE)
  indices[[paste0("onp", suffix)]] <- median(ova_dim[valid_ova], na.rm = TRUE)
  
  # Calculate Extremism Points (thresholds)
  indices[[paste0("cep", suffix)]] <- af_tail_threshold(cog_dim[valid_cog], type = threshold_type, k_factor = k_factor)
  indices[[paste0("bep", suffix)]] <- af_tail_threshold(beh_dim[valid_beh], type = threshold_type, k_factor = k_factor)
  indices[[paste0("sep", suffix)]] <- af_tail_threshold(soc_dim[valid_soc], type = threshold_type, k_factor = k_factor)
  indices[[paste0("oep", suffix)]] <- af_tail_threshold(ova_dim[valid_ova], type = threshold_type, k_factor = k_factor)
  
  # Calculate Extremism Levels, Intensity, Relative Intensity and Ranks
  if (!is_community) {
    # For populations - compare to own extremism points
    cnp <- indices[[paste0("cnp", suffix)]]
    bnp <- indices[[paste0("bnp", suffix)]]
    snp <- indices[[paste0("snp", suffix)]]
    onp <- indices[[paste0("onp", suffix)]]
    
    cep <- indices[[paste0("cep", suffix)]]
    bep <- indices[[paste0("bep", suffix)]]
    sep <- indices[[paste0("sep", suffix)]]
    oep <- indices[[paste0("oep", suffix)]]
  } else {
    # For communities - compare to parent population's extremism points
    cnp <- parent_indices[["cnp_p"]]
    bnp <- parent_indices[["bnp_p"]]
    snp <- parent_indices[["snp_p"]]
    onp <- parent_indices[["onp_p"]]
    
    cep <- parent_indices[["cep_p"]]
    bep <- parent_indices[["bep_p"]]
    sep <- parent_indices[["sep_p"]]
    oep <- parent_indices[["oep_p"]]
  }
  
  # Calculate extremism levels
  indices[[paste0("cel", suffix)]] <- mean(cog_dim >= cep, na.rm = TRUE) * 100
  indices[[paste0("bel", suffix)]] <- mean(beh_dim >= bep, na.rm = TRUE) * 100
  indices[[paste0("sel", suffix)]] <- mean(soc_dim >= sep, na.rm = TRUE) * 100
  indices[[paste0("oel", suffix)]] <- mean(ova_dim >= oep, na.rm = TRUE) * 100
  
  # Calculate extremism intensity
  indices[[paste0("cin", suffix)]] <- mean(cog_dim[cog_dim >= cep], na.rm = TRUE) 
  indices[[paste0("bin", suffix)]] <- mean(beh_dim[beh_dim >= bep], na.rm = TRUE)
  indices[[paste0("sin", suffix)]] <- mean(soc_dim[soc_dim >= sep], na.rm = TRUE)
  indices[[paste0("oin", suffix)]] <- mean(ova_dim[ova_dim >= oep], na.rm = TRUE)
  
  # Calculate extremism relative intensity
  indices[[paste0("cri", suffix)]] <- indices[[paste0("cin", suffix)]] / cnp
  indices[[paste0("bri", suffix)]] <- indices[[paste0("bin", suffix)]] / bnp
  indices[[paste0("sri", suffix)]] <- indices[[paste0("sin", suffix)]] / snp
  indices[[paste0("ori", suffix)]] <- indices[[paste0("oin", suffix)]] / onp
  
  # Calculate extremism ranks
  dim_exceed <- cbind(
    cog_dim >= cep,
    beh_dim >= bep,
    soc_dim >= sep
  )
  dim_exceed_count <- rowSums(dim_exceed, na.rm = TRUE)
  
  indices[[paste0("er1", suffix)]] <- mean(dim_exceed_count >= 1, na.rm = TRUE) * 100
  indices[[paste0("er2", suffix)]] <- mean(dim_exceed_count >= 2, na.rm = TRUE) * 100
  indices[[paste0("er3", suffix)]] <- mean(dim_exceed_count >= 3, na.rm = TRUE) * 100
  
  return(indices)
}

#' Calculate Individual-Level Extremism Indices
#'
#' This function calculates individual-level extremism indicators for each person in the dataset,
#' including binary extremism flags, relative extremism indices, and multi-dimensional extremism ranks.
#' All individuals are compared against population thresholds and normative points.
#'
#' @param df A data frame containing the survey data with dimension scores and population indices.
#'   Must contain columns specified in dimensions parameter and population threshold columns 
#'   (cnp_p, bnp_p, snp_p, onp_p, cep_p, bep_p, sep_p, oep_p).
#' @param dimensions Character vector of exactly 3 variable names corresponding to 
#'   cognitive, behavioral, and social dimensions. Default is c("pe_ideology", "pe_violence", "pe_intolerance").
#'
#' @return The input data frame with added individual-level indices:
#'   \item{i_cel, i_bel, i_sel, i_oel}{Binary indicators (0/1) for whether individual exceeds 
#'     cognitive, behavioral, social, or overall extremism thresholds}
#'   \item{i_cri, i_bri, i_sri, i_ori}{Relative extremism indices - individual's score divided by 
#'     the population normative point for cognitive, behavioral, social, and overall dimensions}
#'   \item{i_er1, i_er2, i_er3}{Binary indicators for exceeding 1+, 2+, or all 3 dimension thresholds}
#'
#' @details
#' The function performs row-wise calculations to:
#' \itemize{
#'   \item Compare each individual's scores to population extremism thresholds
#'   \item Calculate relative intensity by dividing scores by population normative points
#'   \item Count how many dimensions exceed thresholds for multi-dimensional extremism ranks
#' }
#' 
#' Missing values in dimensions or thresholds result in NA for that individual's indices.
#' The pe_overall column must exist in the data frame for overall calculations.
#'
#' @examples
#' # Assuming df already has dimension scores and population indices from af_gauge_indices
#' df_with_individual <- af_calculate_individual_indices(
#'   df = result_df,
#'   dimensions = c("pe_ideology", "pe_violence", "pe_intolerance")
#' )
#'
#' @export
af_calculate_individual_indices <- function(df, dimensions) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  if (length(dimensions) != 3) {
    stop("dimensions must be a character vector of exactly 3 elements")
  }
  
  for (dim in dimensions) {
    if (!dim %in% names(df)) {
      stop(paste("Dimension variable", dim, "not found in the data frame"))
    }
    if (!is.numeric(df[[dim]])) {
      stop(paste("Dimension variable", dim, "must be numeric"))
    }
  }
  
  if (!"pe_overall" %in% names(df)) {
    stop("pe_overall column must exist in the data frame")
  }
  
  # Check for required population threshold and normative columns
  required_cols <- c(
    "cnp_p", "bnp_p", "snp_p", "onp_p",
    "cep_p", "bep_p", "sep_p", "oep_p"
  )
  
  missing_cols <- required_cols[!required_cols %in% names(df)]
  if (length(missing_cols) > 0) {
    stop(paste("Required population columns missing:", paste(missing_cols, collapse = ", ")))
  }
  
  # Initialize result dataframe
  result_df <- df
  
  # Initialize new columns
  result_df$i_cel <- 0
  result_df$i_bel <- 0
  result_df$i_sel <- 0
  result_df$i_oel <- 0
  
  result_df$i_cri <- NA
  result_df$i_bri <- NA
  result_df$i_sri <- NA
  result_df$i_ori <- NA
  
  result_df$i_er1 <- 0
  result_df$i_er2 <- 0
  result_df$i_er3 <- 0
  
  # Process each individual
  for (i in 1:nrow(result_df)) {
    # Skip if any of the dimensions or thresholds are missing
    if (any(is.na(result_df[i, dimensions])) || 
        is.na(result_df[i, "cep_p"]) || 
        is.na(result_df[i, "bep_p"]) || 
        is.na(result_df[i, "sep_p"]) ||
        is.na(result_df[i, "oep_p"])) {
      next
    }
    
    # Get population threshold and normative values for this individual
    cep <- result_df[i, "cep_p"]
    bep <- result_df[i, "bep_p"]
    sep <- result_df[i, "sep_p"]
    oep <- result_df[i, "oep_p"]
    
    cnp <- result_df[i, "cnp_p"]
    bnp <- result_df[i, "bnp_p"]
    snp <- result_df[i, "snp_p"]
    onp <- result_df[i, "onp_p"]
    
    # Calculate relative extremism indices
    result_df$i_cri[i] <- result_df[i, dimensions[1]] / cnp
    result_df$i_bri[i] <- result_df[i, dimensions[2]] / bnp
    result_df$i_sri[i] <- result_df[i, dimensions[3]] / snp
    result_df$i_ori[i] <- result_df[i, "pe_overall"] / onp
    
    # Count dimensions that exceed thresholds
    dimensions_over_threshold <- 0
    
    if (result_df[i, dimensions[1]] >= cep) {
      result_df$i_cel[i] <- 1
      dimensions_over_threshold <- dimensions_over_threshold + 1
    }
    
    if (result_df[i, dimensions[2]] >= bep) {
      result_df$i_bel[i] <- 1
      dimensions_over_threshold <- dimensions_over_threshold + 1
    }
    
    if (result_df[i, dimensions[3]] >= sep) {
      result_df$i_sel[i] <- 1
      dimensions_over_threshold <- dimensions_over_threshold + 1
    }
    
    if (result_df[i, "pe_overall"] >= oep) {
      result_df$i_oel[i] <- 1
    }
    
    # Set extremism rank flags based on number of dimensions over threshold
    if (dimensions_over_threshold >= 1) result_df$i_er1[i] <- 1
    if (dimensions_over_threshold >= 2) result_df$i_er2[i] <- 1
    if (dimensions_over_threshold >= 3) result_df$i_er3[i] <- 1
  }
  
  return(result_df)
}

#' Calculate Tail Threshold Using Different Methods
#'
#' This function calculates a threshold value to identify the tail of a distribution
#' using quantile-based, MAD, robust scale estimator, or Z-score methods.
#'
#' @param scores Numeric vector of scores/values for which to calculate the threshold.
#'   Must contain at least 2 non-missing values.
#' @param type Character string specifying the method to use. Must be one of 
#'   "Quantile", "MAD", "Qn", "Sn", "Tau", or "ZScore". Default is "Quantile".
#' @param q_pct Numeric value between 0 and 1 specifying the quantile threshold
#'   for the "Quantile" method. Default is 0.85 (85th percentile).
#' @param k_factor Numeric value specifying the multiplier for MAD in the 
#'   "MAD" method. Must be positive. Default is 1.5.
#' @param z_pct Numeric value between 0 and 1 specifying the percentile threshold
#'   for the "ZScore" method (e.g., 0.85 for 85th percentile). Default is 0.85.
#'
#' @return Numeric value representing the calculated threshold.
#'
#' @details 
#' The function supports six methods:
#' \itemize{
#'   \item \strong{Quantile}: Returns the q_pct quantile of the scores
#'   \item \strong{MAD}: Returns median + k_factor * MAD, where MAD is the 
#'     median absolute deviation calculated with constant=1
#'   \item \strong{Qn}: Returns median + k_factor * Qn, where Qn is the 
#'     Rousseeuw-Croux Qn estimator (requires robustbase package)
#'   \item \strong{Sn}: Returns median + k_factor * Sn, where Sn is the 
#'     Rousseeuw-Croux Sn estimator (requires robustbase package)
#'   \item \strong{Tau}: Returns median + k_factor * Tau, where Tau is the 
#'     Tau scale estimator (requires robustbase package)
#'   \item \strong{ZScore}: Returns mean + z_score * SD, where z_score corresponds
#'     to the z_pct percentile of the standard normal distribution
#' }
#'
#' @examples
#' # Generate sample data
#' scores <- rnorm(100, mean = 50, sd = 10)
#' 
#' # Calculate 85th percentile threshold
#' af_tail_threshold(scores, type = "Quantile", q_pct = 0.85)
#' 
#' # Calculate MAD-based threshold
#' af_tail_threshold(scores, type = "MAD", k_factor = 1.5)
#' 
#' # Calculate Z-score based threshold
#' af_tail_threshold(scores, type = "ZScore", z_pct = 0.85)
#'
#' @export
af_tail_threshold <- function(scores, type = c("Quantile", "MAD", "Qn", "Sn", "Tau", "ZScore"), 
                              q_pct = 0.85, k_factor = 1.5, z_pct = 0.85) {
  
  # Input validation for scores
  if (missing(scores)) {
    stop("scores parameter is required")
  }
  
  if (!is.numeric(scores)) {
    stop("scores must be a numeric vector")
  }
  
  if (length(scores) == 0) {
    stop("scores cannot be empty")
  }
  
  # Remove NA values and check remaining length
  scores_clean <- scores[!is.na(scores)]
  
  if (length(scores_clean) == 0) {
    stop("scores contains only NA values")
  }
  
  if (length(scores_clean) < 2) {
    stop("scores must contain at least 2 non-missing values")
  }
  
  # Input validation for type
  type <- match.arg(type)
  
  # Input validation for q_pct
  if (!is.numeric(q_pct) || length(q_pct) != 1) {
    stop("q_pct must be a single numeric value")
  }
  
  if (is.na(q_pct)) {
    stop("q_pct cannot be NA")
  }
  
  if (q_pct <= 0 || q_pct >= 1) {
    stop("q_pct must be between 0 and 1 (exclusive)")
  }
  
  # Input validation for k_factor
  if (!is.numeric(k_factor) || length(k_factor) != 1) {
    stop("k_factor must be a single numeric value")
  }
  
  if (is.na(k_factor)) {
    stop("k_factor cannot be NA")
  }
  
  if (k_factor <= 0) {
    stop("k_factor must be positive")
  }
  
  # Input validation for z_pct
  if (!is.numeric(z_pct) || length(z_pct) != 1) {
    stop("z_pct must be a single numeric value")
  }
  
  if (is.na(z_pct)) {
    stop("z_pct cannot be NA")
  }
  
  if (z_pct <= 0 || z_pct >= 1) {
    stop("z_pct must be between 0 and 1 (exclusive)")
  }
  
  # Calculate threshold based on method
  if (type == "Quantile") {
    threshold <- quantile(scores_clean, q_pct, names = FALSE)
    
  } else if (type == "MAD") {
    median_val <- median(scores_clean)
    mad_val <- mad(scores_clean, constant = 1)  
    threshold <- median_val + k_factor * mad_val
    
  } else if (type == "Qn") {
    median_val <- median(scores_clean)
    qn_val <- robustbase::Qn(scores_clean, na.rm = TRUE)
    threshold <- median_val + k_factor * qn_val
    
  } else if (type == "Sn") {
    median_val <- median(scores_clean)
    sn_val <- robustbase::Sn(scores_clean, na.rm = TRUE)
    threshold <- median_val + k_factor * sn_val
    
  } else if (type == "Tau") {
    median_val <- median(scores_clean)
    tau_val <- robustbase::scaleTau2(scores_clean, na.rm = TRUE)
    threshold <- median_val + k_factor * tau_val
    
  } else if (type == "ZScore") {
    mean_val <- mean(scores_clean)
    sd_val <- sd(scores_clean)
    z_score <- qnorm(z_pct)  # Get z-score corresponding to the percentile
    threshold <- mean_val + z_score * sd_val
  }
  
  return(threshold)
}

#' @title Update Index Names Based on Community Selection
#' @description Updates index names with appropriate suffix (_p or _c) based on community selection
#' @param index_names List of index names
#' @param has_community Logical indicating whether a community variable is selected
#' @return Updated list of index names with appropriate suffixes
#' @export
af_update_index_names <- function(index_names, has_community) {
  suffix <- if (has_community) "_c" else "_p"
  
  # Create a new list with updated suffixes
  updated_names <- list()
  for (type_name in names(index_names)) {
    # Replace the last two characters (_p or _c) with the new suffix
    updated_names[[type_name]] <- gsub("_[pc]$", suffix, index_names[[type_name]])
  }
  
  return(updated_names)
}

# Helper function to get available communities based on selected populations
#' @title Get Available Communities
#' @description Retrieves available community values based on selected populations
#' @param data Data frame containing the survey data
#' @param pop_var Character string representing the population variable column name
#' @param comm_var Character string representing the community variable column name
#' @param selected_pops Vector of selected population values (optional)
#' @return Vector of available community values
#' @export
af_get_available_communities <- function(data, pop_var, comm_var, selected_pops = NULL) {
  # Check inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  if (!is.character(pop_var) || length(pop_var) != 1) {
    stop("pop_var must be a single character string")
  }
  if (!is.character(comm_var) || length(comm_var) != 1 || comm_var == "none") {
    return(NULL)  # No community variable selected
  }
  if (!pop_var %in% colnames(data) || !comm_var %in% colnames(data)) {
    stop("pop_var and comm_var must be column names in data")
  }
  
  # Filter by selected populations if provided
  if (!is.null(selected_pops) && length(selected_pops) > 0) {
    data <- data[data[[pop_var]] %in% selected_pops, ]
  }
  
  # Get unique community values
  unique_comms <- unique(data[[comm_var]])
  return(unique_comms)
}

#' Calculate ratio of community metric to population metric
#'
#' @param df A dataframe containing the extremism data
#' @param comm_var Character string representing the community variable column name
#' @param community_metric The column name of the community metric
#' @param population_metric The column name of the population metric
#' @return A dataframe with the calculated ratios
#'
#' @importFrom dplyr left_join select
af_calculate_ratio <- function(df, comm_var, community_metric, population_metric) {
  if(!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  if(!all(c(community_metric, population_metric) %in% names(df))) {
    stop("Specified metrics must be column names in the dataframe")
  }
  
  # Extract population values
  pop_values <- df %>%
    filter(is.na(!!sym(comm_var))) %>%
    select(Wave, !!sym(population_metric)) %>%
    rename(pop_value = !!sym(population_metric))
  
  # Join and calculate ratios
  result <- df %>%
    filter(!is.na(!!sym(comm_var))) %>%
    left_join(pop_values, by = "Wave") %>%
    mutate(ratio = !!sym(community_metric) / pop_value) %>%
    select(Wave, !!sym(comm_var), !!sym(community_metric), pop_value, ratio)
  
  return(result)
}

#' Calculate extremism concentration ratio
#'
#' @param df A dataframe containing the extremism data
#' @param comm_var Character string representing the community variable column name
#' @param numerator_metric The column name of the numerator metric (e.g., er3_c)
#' @param denominator_metric The column name of the denominator metric (e.g., er1_c)
#' @return A dataframe with the calculated concentration ratios
#'
#' @importFrom dplyr select filter mutate
af_calculate_concentration <- function(df, comm_var, numerator_metric, denominator_metric) {
  if(!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  if(!all(c(numerator_metric, denominator_metric) %in% names(df))) {
    stop("Specified metrics must be column names in the dataframe")
  }
  
  result <- df %>%
    filter(!is.na(!!sym(comm_var))) %>%
    mutate(concentration_ratio = !!sym(numerator_metric) / !!sym(denominator_metric)) %>%
    select(Wave, !!sym(comm_var), !!sym(numerator_metric), !!sym(denominator_metric), concentration_ratio)
  
  return(result)
}

