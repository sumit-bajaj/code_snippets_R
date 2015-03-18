#install.packages(c("ggplot2","RColorBrewer","scales"))
library(ggplot2); library(scales); library(grid); library(RColorBrewer) ; library(gridExtra)

#http://skygiant.com.au/software/conditional-package-installation-in-r

#list of required packages
packages = list("ggplot2","scales", "dplyr");

# check for installed packages, install missing ones and load the ones specified in "packages"
for (package in packages){
  if (package %in% installed.packages()[,"Package"] == FALSE){
    install.packages(package); # install missing packages
  }
  #load the package in R
  library(package, character.only = TRUE)
}

#do.call(library, (packages))


#plot.ly
#library(plotly)
#set_credentials_file("user", "key")



# crude hack for geom_text text size scaling
theme.size = 14
text.size = (5/14)*0.8*theme.size

# View all available color palettes
#display.brewer.all()
#https://github.com/karthik/wesanderson
#http://colorbrewer2.org/
#https://learnr.wordpress.com/2009/04/15/ggplot2-qualitative-colour-palettes/
#http://novyden.blogspot.com/2013/09/how-to-expand-color-palette-with-ggplot.html

# View 3 colors for the Set2 palette
#display.brewer.pal(8,"Set2")

# Assign primary colors
col_pal = brewer.pal(3, "Set2")
color1 = col_pal[1]

# Theme created at start of file for orgazinational purposes
#v.grid
#h.grid
#major.grid
#minor.grid
#legend

chart_theme_min <- function(fsize = theme.size) {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_minimal(base_size=fsize) +
    
    # Set the entire chart region to a light gray color
    #theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    
    #theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    #theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    #theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    #theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=fsize*0.8,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=fsize*1.2, vjust=3.25, face = "bold")) +
    theme(axis.text.x=element_text(size=fsize*0.8,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=fsize*0.8,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=fsize,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=fsize,color=color.axis.title, vjust=1.25)) +
    
    # Set formatting for annotations
    
    
    # Plot margins
    theme(plot.margin = unit(c(1.35, 0.2, 0.5, 0.5), "cm"))
}


# reference
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
#http://blog.revolutionanalytics.com/2014/12/cindy-brewer-helping-you-choose-better-color-scales-for-maps.html
#http://www.noamross.net/blog/2013/11/20/formatting-plots-for-pubs.html
#http://fivethirtyeight.com/datalab/our-33-weirdest-charts-from-2014/
#http://stackoverflow.com/questions/19957536/add-dynamic-subtitle-using-ggplot
#http://stackoverflow.com/questions/3606697/how-to-set-limits-for-axes-in-ggplot2-r-plots

  

