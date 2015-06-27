#This script is used to load/ install the required packages and setup the theme for ggplot2 charts




#https://support.rstudio.com/hc/en-us/articles/201057987-Quick-list-of-useful-R-packages

#list of required packages
packages = list("scales", 
                "lubridate",
                "stringr",
                "gridExtra",
                "readr",
                "data.table",
                "tidyr",
                "RColorBrewer",
                "knitr",
                "rmarkdown",
                "htmlTable",
                "grid",
                "xtable",
                "energy",
                "GGally",
                "caret",
                "corrplot",
                "ggplot2",
                "ggthemes",
                "dplyr",
                "DT",
                "slackr",
                "showtext",
                "extrafont")

# check for installed packages, install missing ones and load the ones specified in "packages"
for (package in packages){
  if (package %in% installed.packages()[,"Package"] == FALSE){
    install.packages(package, dependencies = T); # install missing packages
  }
  #load the package in R
  library(package, character.only = TRUE)
}

#Update all installed packages
#update.packages(checkBuilt=TRUE, ask=FALSE, repos="http://cran.rstudio.com/")