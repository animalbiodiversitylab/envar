#' Check Variable Correlation and Multicollinearity
#'
#' @description
#' This function analyzes collinearity among environmental variables using Pearson correlation 
#' and Variance Inflation Factor (VIF). It automatically handles data preparation (converting 
#' rasters to data frames, removing NAs, and dropping constant variables), generates a 
#' correlation plot saved to the working directory, and returns a summary of potential issues.
#'
#' @param x A \code{SpatRaster} (from the terra package) or a \code{data.frame} containing 
#' the environmental variables to be analyzed.
#'
#' @return A list containing four elements:
#' \itemize{
#'   \item \strong{stack}: The original input object \code{x}.
#'   \item \strong{correlation_table}: A matrix of Pearson correlation coefficients.
#'   \item \strong{vif_table}: A data frame containing VIF values, sorted in descending order.
#'   \item \strong{correlation.summary}: A character vector summarizing warnings for variables 
#'   with correlation > 0.6 or VIF > 3.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts \code{SpatRaster} inputs to a data frame.
#'   \item Removes non-numeric columns and rows containing NAs.
#'   \item Identifies and removes variables with zero variance (constant values).
#'   \item Calculates Pearson correlations and saves a plot as \code{"Corr_plot.png"} in the current working directory.
#'   \item Calculates VIF using \code{usdm::vif}.
#' }
#' 
#' @note
#' A correlation plot is saved as a high-resolution PNG file (\code{Corr_plot.png}) in your current working directory.
#'
#' @importFrom cli cli_alert_warning cli_abort cli_alert_info cli_alert_success
#' @importFrom usdm vif
#' @importFrom corrplot corrplot
#' @importFrom grDevices png dev.off colorRampPalette
#' @importFrom stats cor sd na.omit
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with a data frame
#' df <- data.frame(
#'   temp = rnorm(100, mean = 20, sd = 5),
#'   precip = rnorm(100, mean = 100, sd = 20),
#'   elev = rnorm(100, mean = 500, sd = 100)
#' )
#' 
#' # Add a correlated variable
#' df$temp_correlated <- df$temp * 0.9 + rnorm(100)
#' 
#' result <- check(df)
#' 
#' # Inspect results
#' print(result$vif_table)
#' print(result$correlation.summary)
#' }

check <- function(x) {
  
  data_input <- x
  
  # --------------------------------------------------------------------
  # 1. Data Preparation
  # --------------------------------------------------------------------
  # Convert SpatRaster to dataframe if needed
  if (inherits(x, "SpatRaster")) {
    data_input <- as.data.frame(data_input, na.rm = TRUE)
  }
  
  # Ensure data is numeric
  numeric_cols <- sapply(data_input, is.numeric)
  if (!all(numeric_cols)) {
    cli::cli_alert_warning("Removing non-numeric columns: {.val {names(data_input)[!numeric_cols]}}")
    data_input <- data_input[, numeric_cols]
  }
  
  # Remove NA rows
  initial_rows <- nrow(data_input)
  data_input <- na.omit(data_input)
  final_rows <- nrow(data_input)
  
  if (final_rows == 0) {
    cli::cli_abort("Input data contains 0 rows after removing NAs. Check layer overlap or mask alignment.")
  }
  if (final_rows < initial_rows) {
    cli::cli_alert_info("Removed {.val {initial_rows - final_rows}} rows containing NAs.")
  }
  
  # --------------------------------------------------------------------
  # 2. Check for Zero Variance (Constant columns)
  # --------------------------------------------------------------------
  # Constant variables cause cor() to produce NA, which crashes corrplot
  vars_sd <- apply(data_input, 2, sd)
  const_vars <- names(vars_sd)[vars_sd == 0]
  
  if (length(const_vars) > 0) {
    cli::cli_alert_warning("The following variables have zero variance (constant) and will be removed: {.val {const_vars}}")
    data_input <- data_input[, !names(data_input) %in% const_vars]
  }
  
  if (ncol(data_input) < 2) {
    cli::cli_abort("Need at least 2 variables with variance to calculate correlations.")
  }
  
  # --------------------------------------------------------------------
  # 3. Correlation Analysis
  # --------------------------------------------------------------------
  correlation_matrix <- cor(data_input, method = "pearson")
  
  # Plot output path
  plot_path <- file.path(getwd(), 'Corr_plot.png')
  
  # Setup Palette
  BuRd_palette <- tryCatch(khroma::color("BuRd")(9), error = function(e) {
    # Fallback if khroma isn't installed/fails
    colorRampPalette(c("navy", "white", "firebrick"))(9)
  })
  
  # Generate Plot
  png(plot_path, width = 4000, height = 4000, res = 600)
  
  tryCatch({
    corrplot::corrplot(correlation_matrix, 
                       method = "circle",  
                       col = BuRd_palette,  
                       type = "lower",  
                       tl.col = "black",  
                       tl.srt = 45, 
                       tl.cex = 1,
                       cl.cex = 0.8,
                       addCoef.col = "black", 
                       number.cex = 0.7, 
                       is.corr = TRUE,
                       diag = FALSE) 
  }, error = function(e) {
    dev.off() # Ensure device closes if plot fails
    cli::cli_abort("Error creating correlation plot: {e$message}")
  })
  
  dev.off()
  cli::cli_alert_success("Correlation plot saved to {.path {plot_path}}.")
  
  # --------------------------------------------------------------------
  # 4. VIF Analysis
  # --------------------------------------------------------------------
  
  # usdm::vif requires a dataframe or raster. 
  vif_values <- try(as.data.frame(usdm::vif(data_input)), silent = TRUE)
  
  if (inherits(vif_values, "try-error")) {
    cli::cli_alert_warning("VIF calculation failed (likely due to perfect collinearity).")
    vif_sorted <- data.frame(Variables = names(data_input), VIF = NA)
  } else {
    vif_sorted <- vif_values[order(vif_values$VIF, decreasing = TRUE), ] 
  }
  
  # --------------------------------------------------------------------
  # 5. Generate Summary / Warnings
  # --------------------------------------------------------------------
  correlation.summary <- character(0)
  
  # Check Correlation > 0.6
  # IMPORTANT: Set diagonal to 0, otherwise every variable correlates 1.0 with itself
  cor_matrix_no_diag <- correlation_matrix
  diag(cor_matrix_no_diag) <- 0
  
  high_cor_flag <- abs(cor_matrix_no_diag) > 0.6
  
  if (any(high_cor_flag)) {
    # Get names of variables involved in high correlation
    vars.cor <- rownames(high_cor_flag)[rowSums(high_cor_flag) > 0]
    
    msg <- paste0("WARNING: The following variables have a correlation higher than |0.6| with at least one other variable: ", 
                  paste(vars.cor, collapse = ", "))
    correlation.summary <- c(correlation.summary, msg)
    cli::cli_alert_warning("Found high correlations (> |0.6|). Check summary.")
  }
  
  # Check VIF > 3
  if (any(!is.na(vif_sorted$VIF) & vif_sorted$VIF > 3)) {
    vars.vif <- vif_sorted$Variables[vif_sorted$VIF > 3]
    
    msg <- paste0("WARNING: The following variables have a VIF higher than 3: ", 
                  paste(vars.vif, collapse = ", "))
    correlation.summary <- c(correlation.summary, msg)
    cli::cli_alert_warning("Found high VIF values (> 3). Check summary.")
  }
  
  if (length(correlation.summary) == 0) {
    correlation.summary <- "No high correlations or VIF issues detected."
    cli::cli_alert_success("No collinearity issues detected.")
  }
  
  # --------------------------------------------------------------------
  # Return
  # --------------------------------------------------------------------
  return(list(
    stack = x,
    correlation_table = correlation_matrix,
    vif_table = vif_sorted,
    correlation.summary = correlation.summary
  ))
}
