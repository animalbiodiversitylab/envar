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

  BuRd_palette <-khroma::color("BuRd")(9)
  # Plot correlation matrix using corrplot in the right panel
  png(paste0(getwd(), '/Corr_plot_CLC.png'),
      width = 4000,
      height = 4000,
      res = 600)
  
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
           cl.lim = c(-1, 1), 
           is.corr = TRUE,
           diag = FALSE) 
  
  dev.off()
  vif_values <- as.data.frame(usdm::vif(as.data.frame(data_input)))

  # Sort VIF values in decreasing order
  vif_sorted <- vif_values[order(vif_values$VIF, decreasing=T),] 
  
  correlation.summary <- NULL
  
  if (any(correlation_matrix>abs(0.6)) | any(vif_sorted$VIF>3)) {
    
    if (any(correlation_matrix>abs(0.6))) {
      
    correlation_matrix <- correlation_matrix>abs(0.6)
    vars.cor <- colnames(correlation_matrix)[colSums(correlation_matrix == TRUE) > 0]
    correlation.summary <- paste0("WARNING: The following variables have a correlation higher than |0.6| with at least one other variable: ", 
                                  paste(vars.cor, collapse = ", "))
    
    }
    
    if (any(vif_sorted$VIF>3)) {
  
     vars.vif <- c(vif_sorted[vif_sorted$VIF>3, ]$Variables)
     
     correlation.summary <- c(correlation.summary, paste0("WARNING: The following variables have a VIF higher than 3: ", 
                                   paste(vars.vif, collapse = ", ")))
  } 
  
  }
  
  else {
    
  correlation.summary <- paste0("")
  
  }
  
  # Output list
  return(list(
    stack = x,
    correlation_table = correlation_matrix,
    vif_table = vif_sorted,
    correlation.summary = correlation.summary
  ))
}

