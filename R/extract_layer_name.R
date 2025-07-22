# R/extract_layer_name.R
#' Extract meaningful layer name from filename
#' @noRd
extract_layer_name <- function(filename) {
  # Remove extension
  name <- tools::file_path_sans_ext(filename)
  
  # Extract bio number or month
  if (grepl("bio[0-9]+", name)) {
    bio_num <- gsub(".*bio([0-9]+).*", "bio\\1", name)
    return(bio_num)
  } else if (grepl("_[0-9]{2}\\.", filename)) {
    # Monthly data
    month_num <- gsub(".*_([0-9]{2})\\.", "\\1", filename)
    var_type <- gsub(".*_(tmean|tmin|tmax|prec|tas|tasmin|tasmax|pr)_.*", "\\1", name)
    return(paste0(var_type, "_", month_num))
  }
  
  return(name)
}