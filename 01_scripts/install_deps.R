

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Focus:
#' Work on selected questions/figures only. 

#' Author: Steven Ponce
#' Date: 2023-12-21
#' 
#' Check for required packages, and install 
#' missing packages



# List of required packages
required_packages <- c("pacman", "camcorder", "tidyverse", "here", "janitor", "lubridate", 
                       "scales", "ggtext", "showtext", "data.table", "ggdist", "urbnthemes")


# Function to check and install missing packages
check_install_packages <- function(packages) {
  # Get the list of missing packages
  missing_packages <- setdiff(packages, installed.packages()[,"Package"])
  
  # Install missing packages
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, dependencies = TRUE)
    cat("Installed the following missing packages:", paste(missing_packages, collapse = ", "), "\n")
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Check and install missing packages
check_install_packages(packages = required_packages)

