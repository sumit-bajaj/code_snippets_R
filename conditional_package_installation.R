# This snipped can be used to conditionally install a list of required packages
# It will install the packages that are missing and load all the packages in the current R session


#list of required packages. Add all required packages to the below list
packages = list("ggplot2","scales", "dplyr");

# check for installed packages, install missing ones and load the ones specified in "packages"
for (package in packages){
  if (package %in% installed.packages()[,"Package"] == FALSE){
    install.packages(package); # install missing packages
  }
  #load the package in R
  library(package, character.only = TRUE)
}


#source: http://skygiant.com.au/software/conditional-package-installation-in-r
