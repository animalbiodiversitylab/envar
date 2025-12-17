# R/corr_check.R

#' `corr_check()` is the optional end point for the **envar** package workflow. 
#' It allows to to check for variable correlation and multi-collinearity among the variables
#' over the study area.
#' How it works: the user specifies all the sources and variables to download and then after
#' using the %>% symbol it types corr_check() without any argument.
#' 
#' @return A `list` object containing:
#'   * `grid`: A template `SpatRaster` defining the resolution and extent (for polygon input).
#'   * `mask`: An `sf` object defining the exact study area boundaries (for polygon input).
#'   * `res`: The resolution multiplier used.
#'   * `bbox`: The bounding box of the study area.
#'   * `crs`: The target coordinate reference system.
#'   * `type`: The type of input ("polygon", "admin", or "point").
#'   * `is_global`: Logical, TRUE if processing global extent.
#'
#' @examples
#' \dontrun{
#' 
#' processed_bilayer_corr_check <- var_get(country = "Italy", crs=3035, buffer = 10) %>% 
#' esalandcover(vars=c("ice")) %>% 
#' chelsa(vars=c("pr"), months= 12, year=2015) %>% 
#' corr_check()
#' 
#' }
#'
#' @export
corr_check <- function(x) {
  
  input_data <- x
  
  # Extract data for analysis based on input type
  if (inherits(x, "SpatRaster")) {
    df_analysis <- as.data.frame(x, na.rm = TRUE)
  } else if (inherits(x, "data.frame")) {
    # Exclude non-variable columns
    cols_to_exclude <- c("ID", "X", "Y", "x", "y")
    df_analysis <- x[, !names(x) %in% cols_to_exclude, drop = FALSE]
  } else if (is.list(x) && "extracted_df" %in% names(x)) {
    df_analysis <- x$extracted_df[, !names(x$extracted_df) %in% c("ID", "X", "Y"), drop = FALSE]
    input_data <- x$extracted_df
  } else {
    cli::cli_abort("Input must be SpatRaster or data.frame.")
  }
  
  df_analysis <- na.omit(df_analysis)
  
  # Remove constants
  vars_sd <- apply(df_analysis, 2, sd)
  const <- names(vars_sd)[vars_sd == 0]
  if (length(const) > 0) df_analysis <- df_analysis[, !names(df_analysis) %in% const]
  
  if (ncol(df_analysis) < 2) cli::cli_abort("Not enough variables for correlation.")
  
  # Correlation
  cor_mat <- cor(df_analysis, method = "pearson")
  
  # Plot
  plot_path <- file.path(getwd(), "Corr_plot.png")
  png(plot_path, width = 2000, height = 2000, res = 300)
  tryCatch({
    corrplot::corrplot(cor_mat, method = "circle", type = "lower", diag=FALSE, addCoef.col = "black")
  }, error = function(e) {})
  dev.off()
  
  # VIF
  vif_val <- try(usdm::vif(df_analysis), silent=TRUE)
  if (inherits(vif_val, "try-error")) vif_val <- data.frame(Variables=names(df_analysis), VIF=NA)
  vif_val <- vif_val[order(vif_val$VIF, decreasing = TRUE), ]
  # Summary
  cor_diag0 <- cor_mat
  diag(cor_diag0) <- 0
  high_cor <- names(which(apply(abs(cor_diag0), 1, max) > 0.6))
  high_vif <- if (!is.na(vif_val$VIF[1])) as.character(vif_val$Variables[vif_val$VIF > 3]) else character(0)
  
  summary_txt <- character(0)
  if (length(high_cor) > 0) summary_txt <- c(summary_txt, paste("High Cor (>0.6):", paste(high_cor, collapse=", ")))
  if (length(high_vif) > 0) summary_txt <- c(summary_txt, paste("High VIF (>3):", paste(high_vif, collapse=", ")))
  if (length(summary_txt) == 0) summary_txt <- "No issues detected."
  
  return(list(
    data = input_data,
    correlation_matrix = cor_mat,
    vif = vif_val,
    summary = summary_txt,
    plot_path = plot_path
  ))
}