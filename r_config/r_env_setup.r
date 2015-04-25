#This script is used to load/ install the required packages and setup the theme for ggplot2 charts

#list of required packages
packages = list("ggplot2",
                "scales", 
                "gridExtra",
                "dplyr", 
                "RColorBrewer",
                "knitr")

# check for installed packages, install missing ones and load the ones specified in "packages"
for (package in packages){
  if (package %in% installed.packages()[,"Package"] == FALSE){
    install.packages(package, dependencies = T); # install missing packages
  }
  #load the package in R
  library(package, character.only = TRUE)
}

# chart theme settings
chart_format_hist1 <- c(fsize=14, vgrid=1, hgrid=1, mvgrid=1, mhgrid=1)
chart_format_hist_no_minor <- c(fsize=14, vgrid=1, hgrid=1, mvgrid=0, mhgrid=0)
chart_format_hist_no_vgrid <- c(fsize=14, vgrid=0, hgrid=1, mvgrid=0, mhgrid=0)
chart_format_bar1 <- c(fsize=14, vgrid=0, hgrid=1, mvgrid=0, mhgrid=1)
chart_format_bar2 <- c(fsize=14, vgrid=0, hgrid=1, mvgrid=0, mhgrid=0)


#Display all color palettes
#display.brewer.all()
# View 3 colors for the Set2 palette
#display.brewer.pal(8,"Set2")
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

# Assign primary colors
col_pal = brewer.pal(3, "Set2")
#color1 = col_pal[1]
primary_color = c("#1A75CF", "#003896", "#6CABE7")
highlight_color = c("#F47B20", "#FDBB30", "#337321", "#61BF1A")

#v.grid
#h.grid
#major.grid
#minor.grid
#legend

# custom theme for ggplot2 charts
chart_theme_min <- function(fsize = 14, vgrid = 1, hgrid = 1, mvgrid=0, mhgrid=0) {
  
  # define global size variables
  theme.size <<- fsize  #default for base text size
  
  # crude hack for geom_text text size scaling
  text.size <<- (5/14)*0.8*theme.size 
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.grid.minor = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[7]
  
  # size attributes
  gridline.size = .1
  
  # Begin construction of chart
  theme_minimal(fsize) +
    
    # Set the entire chart region to a light gray color
    #theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    #theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    #theme(panel.border=element_rect(color=color.background)) +
    

    

    #Format the grid
    #make gridlines blank if vgrid / hgrid = 0
    theme(panel.grid.major.x = if(vgrid == 0){element_blank()}
          else{element_line(color=color.grid.major,size=gridline.size)}) +
    theme(panel.grid.major.y = if(hgrid == 0){element_blank()}
          else{element_line(color=color.grid.major,size=gridline.size)}) +
    
    #minor gridlines blank if mvgrid/ mhgrid = 0
    theme(panel.grid.minor.x = if(mvgrid == 0){element_blank()}
          else{element_line(color=color.grid.major,size=gridline.size)}) +
    theme(panel.grid.minor.y = if(mhgrid == 0){element_blank()}
          else{element_line(color=color.grid.major,size=gridline.size)}) +

    # Format the legend. Show it conditionally
    theme(legend.position="none") +
    #theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=fsize*0.8,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    #theme(axis.line.x = element_line(colour = color.grid.major)) +
    
    theme(axis.ticks = element_line(colour = color.grid.major)) +
    
    theme(plot.title=element_text(color=color.title, size=fsize*1.2, vjust=3.25, face = "bold")) +
    theme(axis.title.x=element_text(size=fsize,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=fsize,color=color.axis.title, vjust=1.25)) +
    
    theme(axis.text.x=element_text(size=fsize*0.8,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=fsize*0.8,color=color.axis.text)) +
    
        
    # Plot margins
    #around entire plot (unit with the sizes of the top, right, bottom, and left margins)
    #theme(plot.margin = unit(c(1.35, 0.2, 0.5, 0.5), "cm")) 
    theme(plot.margin = unit(c(1.35, 2.35, 0.5, 0.5), "cm")) 
  
}

# Inspired by : http://minimaxir.com/2015/02/ggplot-tutorial/

  

