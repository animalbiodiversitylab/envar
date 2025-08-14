# R/chelsa.R
#' Download CHELSA data
#' @noRd
#' 
#' time_period = c("10_1981-2010", "11_2011-2040")
#' vars = c("tasmin", "bio2", "pr")
#' gcm = c("ACCESS1-0")
#' rcp=45

# var_get(country="Italy") %>% 
#   chelsa(time_period = c("1981-2010", "2011-2040"),
#          vars = c("tas", "bio", "pr"),
#          specs = list("tas" = c("min")),
#          month = c(10, 11),
#          gcm = c("ACCESS1-0"),
#          rcp=45)


chelsa <- function(vars, 
                     time_period = NULL, 
                     gcm = NULL, 
                     rcp = NULL,
                     ssp = NULL) {
    
    urls <- c()
    
    # If "bio" is in vars, ask for which numbers
    if ("bio" %in% vars) {
      bio_range <- readline(prompt = "Enter BIO numbers (e.g., 1:19 or 3:5): ")
      bio_nums <- eval(parse(text = bio_range))
      vars <- setdiff(vars, "bio") # remove generic "bio"
      vars <- c(vars, paste0("bio", bio_nums))
    }
    
    for (var in vars) {
      for (t in time_period) {
        
        years<- sub(".*?(\\d{4}(?:-\\d{4})?).*", "\\1", t)
        
        ## ---- Historical CMIP5 (chelsav1) ----
        if (years %in% c("2041-2060", "2061-2080")) {
          for (g in gcm) {
            for (r in rcp) {
              
              base_url <- "https://os.zhdk.cloud.switch.ch/chelsav1/cmip5"
              
              # Map var to folder name
              if (var == "pr") {
                var1 <- "prec"
              } else if (var == "tas") {
                var1 <- "temp"
              } else if (var == "tasmax") {
                var1 <- "tmax"
              } else if (var == "tasmin") {
                var1 <- "tmin"
              } else if (grepl("^bio\\d+$", var)) {
                var1 <- "bio"
              } else {
                var1 <- var
              }
              
              # Decide whether to add _V1.2 in the filename
              version_suffix <- if (var == "pr") "" else "_V1.2"
              
              filename <- sprintf("CHELSA_%s_mon_%s_rcp%d_r1i1p1_g025.nc_%s%s.tif",
                                  var, g, r, t, version_suffix)
              
              url <- sprintf("%s/%s/%s/%s", base_url, years, var1, filename)
              urls <- c(urls, url)
            }
          }
        }
        
        ## ---- CHELSAv2 Climatologies ----
        if (years %in% c("1981-2010", "2011-2040", "2041-2070", "2071-2100")) {
          
          if (years == "1981-2010") {
            
            base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies"
            
            # Map var to folder name
            var1 <- var
            
            if (grepl("^bio\\d+$", var) | 
                var %in% c("ai", "clt_max", "clt_min",
                           "clt_mean", "clt_range", "cmi_max", "cmi_min",
                           "cmi_mean", "cmi_range", "fcf", "fgd", "gdd0", "gdd5",
                           "gdd10", "gddlgd0", "gddlgd5", "gddlgd10",
                           "gddfgd0", "gddfgd5", "gddfgd10", "gls", "gsp", "gst",
                           "hurs_max", "hurs_min", "hurs_mean", "hurs_range",
                           "kg0", "kg1", "kg2", "kg3", "kg4", "kg5",
                           "lgd", "ngd0", "ngd5", "ngd10", "npp",
                           "pet_penman_max", "pet_penman_min", "pet_penman_mean",
                           "pet_penman_range", "rsds_max", "rsds_min", "rsds_mean", "rsds_range",
                           "scd", "sfcWind_max", "sfcWind_min", "sfcWind_mean", "sfcWind_range",
                           "swb", "swe", "vpd_max", "vpd_min", "vpd_mean", "vpd_range")) {
              var1 <- "bio"
            }
            
            if (var == "pet") var1 <- "pet_penman"
            
            if (var1 == "bio") {
              
              if (var %in% c("rsds_max", "rsds_min", "rsds_mean", "rsds_range")) {
                
                parts <- unlist(strsplit(var, "_"))
                part1 <- parts[1]
                part2 <-parts[2]
                filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif",
                                    part1, years, part2)
                url <- sprintf("%s/%s/%s/%s", base_url, years, var1, filename)
                urls <- c(urls, url)
              }
              else {
                
              filename <- sprintf("CHELSA_%s_%s_V.2.1.tif",
                                  var, years)
              url <- sprintf("%s/%s/%s/%s", base_url, years, var1, filename)
              urls <- c(urls, url)
              }
              
            } else {
            
            if (grepl("^[0-9]{2}_", t)) {
              # Month present
              month <- sub("^([0-9]{2})_.*", "\\1", t)
              filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif",
                                  var, month, years)
              url <- sprintf("%s/%s/%s/%s", base_url, years, var1, filename)
              urls <- c(urls, url)
              
            } else {
              # No month → loop over all months
              for (m in sprintf("%02d", 1:12)) {
                filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif",
                                    var, m, years)
                url <- sprintf("%s/%s/%s/%s", base_url, years, var1, filename)
                urls <- c(urls, url)
              }
            }
              
            }
            
          } else {
            ## ---- Future CHELSAv2 ----
            for (g in gcm) {
              
              for (s in ssp) {
                
                base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies"
                
                var1 <- var
                
                if (grepl("^bio\\d+$", var) | 
                    var %in% c("ai", "clt_max", "clt_min",
                               "clt_mean", "clt_range", "cmi_max", "cmi_min",
                               "cmi_mean", "cmi_range", "fcf", "fgd", "gdd0", "gdd5",
                               "gdd10", "gddlgd0", "gddlgd5", "gddlgd10",
                               "gddfgd0", "gddfgd5", "gddfgd10", "gls", "gsp", "gst",
                               "hurs_max", "hurs_min", "hurs_mean", "hurs_range",
                               "kg0", "kg1", "kg2", "kg3", "kg4", "kg5",
                               "lgd", "ngd0", "ngd5", "ngd10", "npp",
                               "pet_penman_max", "pet_penman_min", "pet_penman_mean",
                               "pet_penman_range", "rsds_max", "rsds_min", "rsds_mean", "rsds_range",
                               "scd", "sfcWind_max", "sfcWind_min", "sfcWind_mean", "sfcWind_range",
                               "swb", "swe", "vpd_max", "vpd_min", "vpd_mean", "vpd_range")) {
                  var1 <- "bio"
                }
                
                if (var == "pet") var1 <- "pet_penman"
                
                if (var1 == "bio") {
                  filename <- sprintf("CHELSA_%s_%s_%s_%s_V.2.1.tif", var, years, tolower(gcm), paste0("ssp", ssp))
                  url <- sprintf("%s/%s/%s/%s/%s/%s", base_url, years, toupper(gcm), paste0("ssp", ssp), var1, filename)
                  urls <- c(urls, url)
                  
                } else {
                  
                  if (grepl("^[0-9]{2}_", t)) {
                    # Month present
                    month <- sub("^([0-9]{2})_.*", "\\1", t)
                    t1 <- gsub("-", "_", years)
                    filename <- sprintf("CHELSA_%s_r1i1p1f1_w5e5_%s_%s_%s_norm.tif", tolower(gcm), paste0("ssp", ssp), var, t1)
                    url <- sprintf("%s/%s/%s/%s/%s/%s", base_url, years, toupper(gcm), paste0("ssp", ssp), var1, filename)
                    urls <- c(urls, url)
                    
                  } else {
                    # No month → loop over all months
                    for (m in sprintf("%02d", 1:12)) {
                      t1 <- gsub("-", "_", years)
                      filename <- sprintf("CHELSA_%s_r1i1p1f1_w5e5_%s_%s_%s_%s_norm.tif", tolower(gcm), paste0("ssp", ssp), var, m,t1)
                      url <- sprintf("%s/%s/%s/%s/%s/%s", base_url, years, toupper(gcm), paste0("ssp", ssp), var1, filename)
                      urls <- c(urls, url)
                    }
                  }
                  
                }
              
    
                  }
                }
              }
        }
        
        ## ---- Monthly ----
        if (years %in% c(1979:2019)) {
          
          base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly"
          
          # Map var to folder name
          var1 <- var
          if (var == "pet") var1 <- "pet_penman"
          
          if (grepl("^[0-9]{2}_", t)) {
            # Month present
            month <- sub("^([0-9]{2})_.*", "\\1", t)
            
            if (var == "rsds") {
              
            filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif",
                                  var,years, month)
            } else{
            filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif",
                                var, month, years)}
            
            url <- sprintf("%s/%s/%s", base_url,var1, filename)
            urls <- c(urls, url)
            
          } else {
            # No month → loop over all months
            for (m in sprintf("%02d", 1:12)) {
              
              if (var == "rsds") {
                
                filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif",
                                    var, years, m)
              } else{
                
              filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif",
                                  var, m, years)
              }
              url <- sprintf("%s/%s/%s", base_url, var1, filename)
              urls <- c(urls, url)
            }
          }
          
        }
        
        ## ---- Annual ----
        if (var == "swb") {
          
          base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/annual/swb"
          
          filename <- sprintf("CHELSA_%s_%s_V.2.1.tif",
                              var, years)
           url <- sprintf("%s/%s", base_url, filename)
           urls <- c(urls, url)
          
        }
        
          }
    }
    return(urls)
  }

