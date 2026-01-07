#' Check Variable Correlation and Multi-collinearity
#'
#' `corr_check()` is an optional function for the `envar` package workflow. 
#' It allows you to check for variable correlation and multi-collinearity among 
#' the variables over the study area. It can be used before or after `extr_check()`.
#'
#' \strong{Citation:}\cr
#' Wei T, Simko V (2021). "R package 'corrplot': Visualization of a Correlation Matrix." GitHub.
#' https://github.com/taiyun/corrplot
#' 
#' Naimi B, Hamm NA, Groen TA, Skidmore AK, Toxopeus AG (2014). "Where is positional uncertainty a problem for species distribution modelling?" Ecography 37, 191-203.
#' https://doi.org/10.1111/j.1600-0587.2013.00205.x
#' 
#' @param x A `SpatRaster`, `data.frame`, or a list containing `data` or `extracted_df`
#'   (e.g., output from `extr_check()`).
#'
#' @return A `list` object containing:
#' \itemize{
#'   \item `data`: The input environmental data used.
#'   \item `correlation_matrix`: Pearson correlation matrix.
#'   \item `vif`: Variance Inflation Factor data frame.
#'   \item `summary`: Character vector highlighting high correlation or VIF.
#'   \item `plot_path`: Path to the saved correlation plot.
#'   \item Any additional elements from input list (e.g., `extrapolation` from `extr_check()`).
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage after environmental variable extraction
#' processed_bilayer_corr_check <- var_get(country = "Italy", crs=3035, buffer = 10) %>% 
#'   esalandcover(vars=c("ice")) %>% 
#'   chelsa(vars=c("pr"), months= 12, year=2015) %>% 
#'   corr_check()
#' 
#' # Example 2: Chain with extr_check() (corr_check before extr_check)
#' result <- var_get(country = "Italy") %>% 
#'   chelsa(vars = c("bio1", "bio12")) %>% 
#'   corr_check() %>%
#'   extr_check(calib_points = my_points)
#' 
#' # Example 3: Chain with extr_check() (extr_check before corr_check)
#' result <- var_get(country = "Italy") %>% 
#'   chelsa(vars = c("bio1", "bio12")) %>% 
#'   extr_check(calib_points = my_points) %>%
#'   corr_check()
#' }
#' @export

corr_check <- function(x) {
  
  input_data <- NULL
  input_list <- NULL
  is_list_input <- FALSE
  
  # -------------------------------------------------------------------------
  # Extract data for analysis based on input type
  # -------------------------------------------------------------------------
  
  if (inherits(x, "SpatRaster")) {
    input_data <- x
    df_analysis <- terra::as.data.frame(x, na.rm = TRUE)
    
  } else if (inherits(x, "data.frame") && !inherits(x, "sf")) {
    input_data <- x
    # Exclude non-variable columns and extrapolation columns
    cols_to_exclude <- c("ID", "X", "Y", "x", "y", "id", "strict", "combinatorial")
    df_analysis <- x[, !tolower(names(x)) %in% tolower(cols_to_exclude), drop = FALSE]
    
  } else if (is.list(x) && !inherits(x, "SpatRaster") && !inherits(x, "data.frame")) {
    is_list_input <- TRUE
    input_list <- x
    
    # Handle list input (could be from extr_check or previous corr_check)
    if ("data" %in% names(x)) {
      data_element <- x$data
    } else if ("extracted_df" %in% names(x)) {
      data_element <- x$extracted_df
    } else {
      cli::cli_abort("List input must contain 'data' or 'extracted_df' element.")
    }
    
    if (inherits(data_element, "SpatRaster")) {
      input_data <- data_element
      df_analysis <- terra::as.data.frame(data_element, na.rm = TRUE)
    } else if (inherits(data_element, "data.frame")) {
      input_data <- data_element
      # Exclude non-variable columns and extrapolation columns
      cols_to_exclude <- c("ID", "X", "Y", "x", "y", "id", "strict", "combinatorial")
      df_analysis <- data_element[, !tolower(names(data_element)) %in% tolower(cols_to_exclude), drop = FALSE]
    } else {
      cli::cli_abort("Data element must be SpatRaster or data.frame.")
    }
    
  } else {
    cli::cli_abort("Input must be SpatRaster, data.frame, or a list containing 'data' or 'extracted_df'.")
  }
  
  # -------------------------------------------------------------------------
  # Perform correlation analysis
  # -------------------------------------------------------------------------
  
  df_analysis <- stats::na.omit(df_analysis)
  
  # Remove constant variables
  vars_sd <- apply(df_analysis, 2, stats::sd)
  const <- names(vars_sd)[vars_sd == 0]
  if (length(const) > 0) {
    cli::cli_alert_warning("Removing constant variable(s): {.val {const}}")
    df_analysis <- df_analysis[, !names(df_analysis) %in% const, drop = FALSE]
  }
  
  if (ncol(df_analysis) < 2) {
    cli::cli_abort("Not enough variables for correlation analysis (need at least 2).")
  }
  
  cli::cli_alert_info("Analyzing correlation for {ncol(df_analysis)} variables...")
  
  # Correlation matrix
  cor_mat <- stats::cor(df_analysis, method = "pearson")
  
  # Plot
  plot_path <- file.path(getwd(), "Corr_plot.png")
  grDevices::png(plot_path, width = 2000, height = 2000, res = 300)
  tryCatch({
    corrplot::corrplot(cor_mat, method = "circle", type = "lower", diag = FALSE, addCoef.col = "black")
  }, error = function(e) {
    cli::cli_alert_warning("Could not create correlation plot: {e$message}")
  })
  grDevices::dev.off()
  
  # VIF calculation
  vif_val <- tryCatch({
    usdm::vif(df_analysis)
  }, error = function(e) {
    cli::cli_alert_warning("VIF calculation failed: {e$message}")
    data.frame(Variables = names(df_analysis), VIF = NA)
  })
  
  if (!inherits(vif_val, "data.frame") || nrow(vif_val) == 0) {
    vif_val <- data.frame(Variables = names(df_analysis), VIF = NA)
  }
  
  vif_val <- vif_val[order(vif_val$VIF, decreasing = TRUE), ]
  
  # Summary statistics
  cor_diag0 <- cor_mat
  diag(cor_diag0) <- 0
  high_cor <- names(which(apply(abs(cor_diag0), 1, max) > 0.6))
  high_vif <- if (!is.na(vif_val$VIF[1])) {
    as.character(vif_val$Variables[vif_val$VIF > 3])
  } else {
    character(0)
  }
  
  summary_txt <- character(0)
  if (length(high_cor) > 0) {
    summary_txt <- c(summary_txt, paste("High Cor (>0.6):", paste(high_cor, collapse = ", ")))
  }
  if (length(high_vif) > 0) {
    summary_txt <- c(summary_txt, paste("High VIF (>3):", paste(high_vif, collapse = ", ")))
  }
  if (length(summary_txt) == 0) {
    summary_txt <- "No issues detected."
  }
  
  cli::cli_alert_success("Correlation analysis completed.")
  if (length(high_cor) > 0) {
    cli::cli_alert_warning("Variables with high correlation (>0.6): {.val {high_cor}}")
  }
  if (length(high_vif) > 0) {
    cli::cli_alert_warning("Variables with high VIF (>3): {.val {high_vif}}")
  }
  
  # -------------------------------------------------------------------------
  # Prepare output
  # -------------------------------------------------------------------------
  
  if (is_list_input) {
    # Preserve existing list elements (e.g., extrapolation from extr_check)
    output <- input_list
    output$data <- input_data
    output$correlation_matrix <- cor_mat
    output$vif <- vif_val
    output$summary <- summary_txt
    output$plot_path <- plot_path
  } else {
    output <- list(
      data = input_data,
      correlation_matrix = cor_mat,
      vif = vif_val,
      summary = summary_txt,
      plot_path = plot_path
    )
  }
  
  return(output)
}