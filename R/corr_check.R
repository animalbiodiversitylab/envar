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
#' @param pearson Numeric or `NULL`. Threshold for the absolute Pearson correlation
#'   coefficient above which variables are flagged with a warning. By default
#'   (`NULL`) a default threshold of `0.7` is used; supply a value (e.g. `0.6`) to
#'   be warned about variable pairs whose absolute correlation exceeds it instead.
#' @param vif Numeric or `NULL`. Threshold for the Variance Inflation Factor above
#'   which variables are flagged with a warning. By default (`NULL`) a default
#'   threshold of `3` is used; supply a value (e.g. `5`) to be warned about
#'   variables whose VIF exceeds it instead.
#'
#' @details
#' Regardless of whether `pearson`/`vif` thresholds are set, the function writes
#' two files: the correlation plot (`Corr_plot.png`) and a table of VIF values
#' (`VIF_table.csv`). In an interactive R session it asks, at the console, for the
#' directory in which to store them every time it is called. In non-interactive
#' sessions (e.g. scripts or `R CMD check`) a temporary directory is used and no
#' prompt is shown. Their paths are returned as `plot_path` and `vif_path`.
#'
#' @return A `list` object containing:
#' \itemize{
#'   \item `data`: The input environmental data used.
#'   \item `correlation_matrix`: Pearson correlation matrix.
#'   \item `vif`: Variance Inflation Factor data frame.
#'   \item `summary`: Character vector highlighting high correlation or VIF, based
#'     on the supplied thresholds (or the defaults of `0.7` and `3`).
#'   \item `plot_path`: Path to the saved correlation plot.
#'   \item `vif_path`: Path to the saved VIF table.
#'   \item Any additional elements from input list (e.g., `extrapolation` from `extr_check()`).
#' }
#'
#' @examples
#' # corr_check() runs offline on the small example raster bundled with the
#' # package (a real WorldClim extract for Switzerland), so it needs no download:
#' switzerland <- terra::rast(
#'   system.file("extdata", "switzerland.tif", package = "envar")
#' )
#' cc <- corr_check(switzerland)
#' cc$summary
#'
#' # It also accepts a plain data.frame of predictor values:
#' set.seed(1)
#' example_df <- data.frame(bio1 = rnorm(40), bio12 = rnorm(40))
#' example_df$bio11 <- example_df$bio1 + rnorm(40, sd = 0.2)  # correlated with bio1
#' corr_check(example_df)$summary
#'
#' \donttest{
#' # The typical workflow chains corr_check() after downloading variables.
#' # Example 1: Basic usage after environmental variable extraction
#' processed_bilayer_corr_check <- par_set(country = "Italy", crs=3035, buffer = 10) %>% 
#'   melc(vars=c("ice")) %>% 
#'   chelsa(vars=c("pr"), months= 12, year=2015) %>% 
#'   corr_check()
#' 
#' # Example 2: Chain with extr_check() (corr_check before extr_check)
#' result <- par_set(country = "Italy") %>% 
#'   chelsa(vars = c("bio1", "bio12")) %>% 
#'   corr_check() %>%
#'   extr_check(calib_points = my_points)
#' 
#' # Example 3: Chain with extr_check() (extr_check before corr_check)
#' result <- par_set(country = "Italy") %>%
#'   chelsa(vars = c("bio1", "bio12")) %>%
#'   extr_check(calib_points = my_points) %>%
#'   corr_check()
#'
#' # Example 4: Custom thresholds for high correlation (>0.7) and VIF (>5)
#' result <- par_set(country = "Italy") %>%
#'   chelsa(vars = c("bio1", "bio12")) %>%
#'   corr_check(pearson = 0.7, vif = 5)
#' }
#' @export

corr_check <- function(x, pearson = NULL, vif = NULL) {

  # -------------------------------------------------------------------------
  # Validate threshold arguments
  # -------------------------------------------------------------------------
  if (!is.null(pearson) && (!is.numeric(pearson) || length(pearson) != 1)) {
    cli::cli_abort("{.arg pearson} must be a single numeric value or {.code NULL}.")
  }
  if (!is.null(vif) && (!is.numeric(vif) || length(vif) != 1)) {
    cli::cli_abort("{.arg vif} must be a single numeric value or {.code NULL}.")
  }

  # Fall back to default thresholds when the user does not supply them.
  pearson_thr <- if (is.null(pearson)) 0.7 else pearson
  vif_thr <- if (is.null(vif)) 3 else vif

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

  if (nrow(df_analysis) < 2) {
    cli::cli_abort(c(
      "Not enough complete observations for correlation analysis.",
      "x" = "After removing missing values, {nrow(df_analysis)} row{?s} remain.",
      "i" = "The layers/points may be all NA over the study area (e.g. a CRS mismatch or an area with no data)."
    ))
  }

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
  
  # Resolve (once per session) the directory to store the output files in.
  out_dir <- envar_corr_dir()

  # Plot
  plot_path <- file.path(out_dir, "Corr_plot.png")
  grDevices::png(plot_path, width = 2000, height = 2000, res = 300)
  tryCatch({
    corrplot::corrplot(cor_mat, method = "circle", type = "lower", diag = FALSE, addCoef.col = "black")
  }, error = function(e) {
    cli::cli_alert_warning("Could not create correlation plot: {e$message}")
  })
  grDevices::dev.off()
  cli::cli_alert_info("Correlation plot saved to {.file {plot_path}}.")
  
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

  # Store the VIF table in the directory chosen for this session.
  vif_path <- file.path(out_dir, "VIF_table.csv")
  tryCatch({
    utils::write.csv(vif_val, vif_path, row.names = FALSE)
    cli::cli_alert_info("VIF table saved to {.file {vif_path}}.")
  }, error = function(e) {
    cli::cli_alert_warning("Could not save VIF table: {e$message}")
  })

  # -------------------------------------------------------------------------
  # Summary statistics and warnings
  # -------------------------------------------------------------------------
  # Variables are flagged against the user-supplied thresholds, or the default
  # thresholds (Pearson 0.7, VIF 3) when the arguments are left as NULL.
  cor_diag0 <- cor_mat
  diag(cor_diag0) <- 0

  high_cor <- names(which(apply(abs(cor_diag0), 1, max) > pearson_thr))
  high_vif <- if (!is.na(vif_val$VIF[1])) {
    as.character(vif_val$Variables[vif_val$VIF > vif_thr])
  } else {
    character(0)
  }

  summary_txt <- character(0)
  if (length(high_cor) > 0) {
    summary_txt <- c(summary_txt, paste0("High Cor (>", pearson_thr, "): ", paste(high_cor, collapse = ", ")))
  }
  if (length(high_vif) > 0) {
    summary_txt <- c(summary_txt, paste0("High VIF (>", vif_thr, "): ", paste(high_vif, collapse = ", ")))
  }
  if (length(summary_txt) == 0) {
    summary_txt <- "No issues detected."
  }

  cli::cli_alert_success("Correlation analysis completed.")
  if (length(high_cor) > 0) {
    cli::cli_alert_warning("Variables with high correlation (>{pearson_thr}): {.val {high_cor}}")
  }
  if (length(high_vif) > 0) {
    cli::cli_alert_warning("Variables with high VIF (>{vif_thr}): {.val {high_vif}}")
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
    output$vif_path <- vif_path
  } else {
    output <- list(
      data = input_data,
      correlation_matrix = cor_mat,
      vif = vif_val,
      summary = summary_txt,
      plot_path = plot_path,
      vif_path = vif_path
    )
  }
  
  return(output)
}


#' Directory used by corr_check() to store its output files
#'
#' Determines where [corr_check()] writes `Corr_plot.png` and `VIF_table.csv`.
#' In an interactive session it asks the user, at the console, for a directory
#' every time it is called. In non-interactive sessions (scripts, `R CMD check`,
#' ...) it silently uses a temporary directory and never writes to the working
#' directory.
#'
#' @return Path to the (existing) directory in which to store the files.
#' @noRd
envar_corr_dir <- function() {
  if (interactive()) {
    # Print the explanation on its own line, then use a short single-line
    # readline() prompt so the console shows a wide input field to type into.
    cat("Directory to store correlation info (empty for working directory):\n")
    ans <- readline(prompt = "Path: ")

    # Clean the input: trim spaces, drop any surrounding single/double quotes
    # (e.g. '~/data' or "~/data"), then expand '~' to the home directory.
    ans <- trimws(ans)
    ans <- gsub("^['\"]+|['\"]+$", "", ans)
    ans <- trimws(ans)

    if (!nzchar(ans)) {
      # User confirmed (empty input at the interactive prompt): use the working
      # directory. Allowed because this only happens interactively.
      dir <- getwd()
    } else {
      dir <- path.expand(ans)
      # A relative entry (e.g. "Download") would otherwise be resolved by R
      # against the current working directory. Anchor it to the user's home
      # directory instead so files never land in an unexpected place.
      is_absolute <- grepl("^(/|\\\\|[A-Za-z]:)", dir)
      if (!is_absolute) {
        dir <- file.path(path.expand("~"), dir)
      }
    }
    cli::cli_alert_info("Correlation files will be stored in {.file {dir}}.")
  } else {
    # Never prompt and never write to the working directory in non-interactive
    # sessions (e.g. R CMD check): use a temporary directory.
    dir <- tempdir()
  }

  # Make sure the directory exists; fall back to a temp dir if it cannot be
  # created (e.g. a mistyped or non-writable path).
  if (!dir.exists(dir)) {
    created <- suppressWarnings(dir.create(dir, recursive = TRUE))
    if (!created && !dir.exists(dir)) {
      cli::cli_alert_warning(
        "Could not use {.file {dir}}; storing the files in a temporary directory instead."
      )
      dir <- tempdir()
    }
  }

  dir
}
