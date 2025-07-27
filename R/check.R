check <- function(x) {

  data_input <- x
  
  # Convert SpatRaster to dataframe if needed
  if (inherits(x, "SpatRaster")) {
    data_input <- as.data.frame(data_input, na.rm = TRUE)
  }

  # Ensure data is numeric and remove NA rows
  data_input <- data_input[, sapply(data_input, is.numeric)]
  data_input <- na.omit(data_input)

  # Compute Pearson correlation matrix
  correlation_matrix <- cor(as.data.frame(data_input), method = "pearson")

  # Plot correlation matrix using corrplot in the right panel
  corrplot::corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

  vif_values <- as.data.frame(usdm::vif(as.data.frame(data_input)))

  # Sort VIF values in decreasing order
  vif_sorted <- vif_values[order(vif_values$VIF, decreasing=T),] 

  # Output list
  return(list(
    correlation_table = correlation_matrix,
    vif_table = vif_sorted
  ))
}

