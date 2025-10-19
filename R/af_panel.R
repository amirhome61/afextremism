#' Perform Hausman test and provide model selection recommendation
#'
#' @param within_model A plm object with model = "within" (fixed effects)
#' @param random_model A plm object with model = "random" (random effects)
#' @param alpha Significance level for the test (default: 0.05)
#' @param verbose Logical, whether to print detailed results (default: TRUE)
#' @param force_format Character, force output format: "html", "text", or NULL for auto-detect
#'
#' @return A list containing the test statistic, p-value, recommendation, and full test results
#' @export
#'
#' @examples
#' # Assuming you have within_model and random_model
#' result <- af_hausman_test(within_model, random_model)
af_hausman_test <- function(within_model, random_model, alpha = 0.05, verbose = TRUE, force_format = NULL) {
  
  # Input validation
  if (!inherits(within_model, "plm")) {
    stop("within_model must be a plm object")
  }
  
  if (!inherits(random_model, "plm")) {
    stop("random_model must be a plm object")
  }
  
  if (within_model$args$model != "within") {
    stop("within_model must be estimated with model = 'within'")
  }
  
  if (random_model$args$model != "random") {
    stop("random_model must be estimated with model = 'random'")
  }
  
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a numeric value between 0 and 1")
  }
  
  if (!is.logical(verbose)) {
    stop("verbose must be TRUE or FALSE")
  }
  
  if (!is.null(force_format) && !force_format %in% c("html", "text")) {
    stop("force_format must be 'html', 'text', or NULL")
  }
  
  # Detect environment
  detect_rmarkdown <- function() {
    if (!is.null(force_format)) {
      return(force_format == "html")
    }
    
    # Check if we're in R Markdown - improved detection
    in_rmd <- tryCatch({
      # Check if knitr is loaded and we're in a chunk
      if (requireNamespace("knitr", quietly = TRUE)) {
        !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) ||
          !is.null(knitr::opts_chunk$get("dev")) ||
          !is.null(knitr::current_input()) ||
          isTRUE(getOption("knitr.in.progress"))
      } else {
        FALSE
      }
    }, error = function(e) FALSE)
    
    return(in_rmd)
  }
  
  is_rmarkdown <- detect_rmarkdown()
  
  # Perform Hausman test
  hausman_result <- tryCatch({
    plm::phtest(within_model, random_model)
  }, error = function(e) {
    stop("Error performing Hausman test: ", e$message)
  })
  
  # Extract key results
  test_statistic <- hausman_result$statistic
  p_value <- hausman_result$p.value
  degrees_freedom <- hausman_result$parameter
  
  # Make recommendation based on p-value
  if (p_value < alpha) {
    recommendation <- "Fixed Effects (within)"
    explanation <- paste0("Reject H0: Use fixed effects model (p < ", alpha, ")")
    decision_reason <- "The random effects assumption is violated - individual effects are correlated with regressors"
  } else {
    recommendation <- "Random Effects (random)"
    explanation <- paste0("Fail to reject H0: Use random effects model (p >= ", alpha, ")")
    decision_reason <- "The random effects assumption holds - individual effects are uncorrelated with regressors"
  }
  
  # Create results list
  results <- list(
    test_statistic = as.numeric(test_statistic),
    p_value = as.numeric(p_value),
    degrees_freedom = as.numeric(degrees_freedom),
    alpha = alpha,
    recommendation = recommendation,
    explanation = explanation,
    decision_reason = decision_reason,
    full_test = hausman_result
  )
  
  # Format significance level
  if (p_value < 0.001) {
    sig_level <- "*** (p < 0.001)"
  } else if (p_value < 0.01) {
    sig_level <- "** (p < 0.01)"
  } else if (p_value < 0.05) {
    sig_level <- "* (p < 0.05)"
  } else if (p_value < 0.1) {
    sig_level <- ". (p < 0.1)"
  } else {
    sig_level <- "(not significant)"
  }
  
  # Print results if verbose
  if (verbose) {
    if (is_rmarkdown) {
      # HTML output for R Markdown
      output <- paste0(
        "<div style='margin: 20px 0; font-family: Arial, sans-serif;'>\n",
        "<h3 style='color: #333; border-bottom: 2px solid #333; padding-bottom: 5px;'>Hausman Test Results</h3>\n",
        "<div style='margin: 15px 0;'>\n",
        "<p><strong>Hypotheses:</strong></p>\n",
        "<ul style='margin-left: 20px;'>\n",
        "<li>H<sub>0</sub>: Random effects model is consistent and efficient</li>\n",
        "<li>H<sub>1</sub>: Fixed effects model is consistent</li>\n",
        "</ul>\n",
        "</div>\n",
        "<div style='margin: 15px 0;'>\n",
        "<p><strong>Test Results:</strong></p>\n",
        "<ul style='margin-left: 20px;'>\n",
        "<li>Test statistic (χ²): <code>", round(test_statistic, 4), "</code></li>\n",
        "<li>Degrees of freedom: <code>", degrees_freedom, "</code></li>\n",
        "<li>P-value: <code>", round(p_value, 4), "</code></li>\n",
        "</ul>\n",
        "</div>\n",
        "<div style='margin: 15px 0; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #007bff;'>\n",
        "<p><strong>Decision at α = ", alpha, ":</strong></p>\n",
        "<p><strong>Recommendation:</strong> <span style='color: #007bff; font-weight: bold;'>", recommendation, "</span></p>\n",
        "<p><strong>Explanation:</strong> ", explanation, "</p>\n",
        "<p><strong>Reason:</strong> ", decision_reason, "</p>\n",
        "<p><strong>Significance:</strong> ", sig_level, "</p>\n",
        "</div>\n",
        "</div>\n"
      )
      
      cat(output)
    } else {
      # Text output for console
      output <- paste0(
        "\nHausman Test Results\n",
        "====================\n\n",
        "H0: Random effects model is consistent and efficient\n",
        "H1: Fixed effects model is consistent\n\n",
        "Test statistic (chi-squared): ", round(test_statistic, 4), "\n",
        "Degrees of freedom: ", degrees_freedom, "\n",
        "P-value: ", round(p_value, 4), "\n\n",
        "Decision at α = ", alpha, ":\n",
        "Recommendation: ", recommendation, "\n",
        "Explanation: ", explanation, "\n",
        "Reason: ", decision_reason, "\n\n",
        "Significance: ", sig_level, "\n\n"
      )
      
      cat(output)
    }
  }
  
  return(invisible(results))
}