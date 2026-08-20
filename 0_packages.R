#### INSTALL AND LOAD PACKAGES ####

library_upload = function(packages) {
  
  for (package in packages) {
    
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    
    suppressPackageStartupMessages(
      library(package, character.only = TRUE)
    )
    
    message(package, " loaded.")
  }
}

packages = c(
  "foreign",
  "readr",
  "dplyr",
  "tidyverse",
  "raster",
  "sandwich",
  "stargazer",
  "marginaleffects",
  "MASS"
)

library_upload(packages)