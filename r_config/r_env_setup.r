#This script is used to load/ install the required packages and setup the theme for ggplot2 charts

#list of required packages
packages = list("ggplot2",
                "ggthemes",
                "scales", 
                "lubridate",
                "stringr",
                "gridExtra",
                "readr",
                "data.table",
                "tidyr",
                "RColorBrewer",
                "knitr",
                "htmlTable",
                "grid",
                "xtable",
                "energy",
                "GGally",
                "caret",
                "corrplot",
                "dplyr")

# check for installed packages, install missing ones and load the ones specified in "packages"

for (package in packages){
  if (package %in% installed.packages()[,"Package"] == FALSE){
    install.packages(package, dependencies = T); # install missing packages
  }
  #load the package in R
  library(package, character.only = TRUE)
}


#Specify custom colors
color_primary_pal = c("#1A75CF", "#003896", "#6CABE7")
color_highlight_pal = c("#F47B20", "#FDBB30", "#337321", "#61BF1A")



#Default font size for charts
chart.theme.size = 14
chart.line.size = 0.75

#chart theme presets for the custom theme function
chart_format_default <- c(fsize=chart.theme.size, vgrid=1, hgrid=1, mvgrid=0, mhgrid=0)
chart_format_hist1 <- c(fsize=chart.theme.size, vgrid=1, hgrid=1, mvgrid=1, mhgrid=1)
chart_format_hist_no_minor <- c(fsize=chart.theme.size, vgrid=1, hgrid=1, mvgrid=0, mhgrid=0)
chart_format_hist_no_vgrid <- c(fsize=chart.theme.size, vgrid=0, hgrid=1, mvgrid=0, mhgrid=0)
chart_format_bar1 <- c(fsize=chart.theme.size, vgrid=0, hgrid=1, mvgrid=0, mhgrid=1)
chart_format_bar2 <- c(fsize=chart.theme.size, vgrid=0, hgrid=1, mvgrid=0, mhgrid=0)


#set default theme as minimal
theme_set(theme_minimal(chart.theme.size)) #minimal theme as default

#custom theme function for ggplot2 charts
chart_theme_minimal <- function(fsize = chart.theme.size, vgrid = 0, hgrid = 1, mvgrid=0, mhgrid=0) {
  
  #size attributes - font, gridline etc.
  gridline.size = .15
  
  #crude hack for geom_text text size scaling
  text.size <<- (5/14)*0.8*fsize 
  
  #Generate the grey colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.grid.minor = palette[3]
  color.axis.text = palette[6]
  #color.axis.title = palette[7]
  color.axis.title = palette[5]
  color.title = palette[7]
  
  
  
  #Begin construction of chart
  theme_minimal(fsize) +  #start with the base minimal theme
    #basic shapes
    theme(
      line = element_line(colour = color_primary_pal[1], size = 0.5, linetype = 1, lineend = "butt"), 
      rect = element_rect(fill = color_primary_pal[1]), 
      text = element_text(colour = "black", size = fsize*0.8, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9)
      ) +
    #Set the entire chart region to a light gray color
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

    #Format the legend. Hide by default
    theme(legend.position="none") +  #hide the legend
    theme(legend.title = element_text(colour=color.axis.title, size=fsize*0.8, face="bold"))+
    #theme(legend.title=element_blank()) +
    #theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=fsize*0.8,color=color.axis.text)) +
    
    #Set title and axis labels, and format these and tick marks
    #theme(axis.line.x = element_line(colour = color.grid.major)) +
    #theme(axis.ticks = element_line(colour = color.grid.major)) +
    theme(axis.ticks = element_blank()) +
    
    theme(plot.title=element_text(color=color.title, size=fsize*1.2, vjust=1.25, face = "bold")) +
    theme(axis.title.x=element_text(size=fsize,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=fsize,color=color.axis.title, vjust=1.25)) +
    
    theme(axis.text.x=element_text(size=fsize*0.8,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=fsize*0.8,color=color.axis.text)) +
    
        
    #Plot margins
    #around entire plot (unit with the sizes of the top, right, bottom, and left margins)
    theme(plot.margin = unit(c(1.35, 2.35, 1.5, 0.5), "cm")) 
  
}


#A function to add footnotes to a ggplot chart
chart_footnote <- function(footnoteText =
                           format(Sys.time(), "%d %b %Y"),
                           size = .7, color = grey(.1))
{
  pushViewport(viewport())
  grid.text(label = footnoteText ,
            #x = unit(1,"npc") - unit(2, "mm"),
            x = 0.01,
            y = unit(2, "mm"),
            just = c("left", "bottom"),
            gp = gpar(cex = size, col = color, alpha = 0.5))
  popViewport()
}

#Alternate function to add footnote where you need to save it to a file using ggsave
chart_footnote_save <- function(chart1, footnote = "Source: <add text>\nNote* <add text>", font.size = chart.theme.size){
  chart1 <- chart1 + theme(plot.margin = unit(c(1.35, 2.35, .5, 0.5), "cm")) 
  arrangeGrob(chart1,
              sub = textGrob(footnote,
                             x = 0.01, hjust = 0, vjust=0.4,
                             gp = gpar( fontsize = font.size*.7, col = gray(0.1), alpha = 0.5)))
}

#Custom footer/ watermark
chart_footer <- annotate("text", x = Inf, y = -Inf, label = "sumitbajaj.me", hjust=1.1, vjust= -.5, col="gray", cex=3, alpha = 0.8)

#call the custom ggplot theme with a preset
chart_theme01 <- do.call(chart_theme_minimal, as.list(chart_format_default))
chart_theme_default <- do.call(chart_theme_minimal, as.list(chart_format_default))
chart_theme_histogram <- do.call(chart_theme_minimal, as.list(chart_format_hist_no_vgrid))
chart_theme_bar <- do.call(chart_theme_minimal, as.list(chart_format_hist_no_vgrid))
chart_theme_scatter <- do.call(chart_theme_minimal, as.list(chart_format_default))

chart_format_bar_x_axis_rotate <- theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))

geom_bar_custom <- geom_bar(fill = color_primary_pal[1], alpha = 0.9) 
legend_top <- theme(legend.position="top")
legend_title_hide <- theme(legend.title=element_blank())

#over-ride default size for legend symbol
legend_size_override <- function(size = chart.line.size*2){
  guides(colour = guide_legend(override.aes = list(size=size)))
}

  
#Display all color palettes
#display.brewer.all()
# View 5 colors for the Set2 palette
#display.brewer.pal(5,"Set2")
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#http://coolors.co/browser

#Extract colors from standard palette
#col_pal = brewer.pal(3, "Set2")
#color1 = col_pal[1]

#GGplot theme customization
#http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/#tip-on-creating-a-custom-theme
