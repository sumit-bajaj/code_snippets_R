#This script has custom theme function and several shortcuts for commonly used visualization tweaks in ggplot2


#Specify custom color palettes
color_primary_pal = c("#1A75CF", "#003896", "#6CABE7")
color_highlight_pal = c("#F47B20", "#FDBB30", "#337321", "#61BF1A")
color_fill_selective_pal = c("#D9D9D9", "#1A75CF")
color_qualitative_pal = c("#1A75CF",  "#ffa700", "#d62d20", "#008744")

#Default font size for charts
chart.theme.size = 14
chart.line.size = 0.75

#Default color variables 
#Generate the grey colors for the chart procedurally with RColorBrewer
palette <- brewer.pal("Greys", n=9)
color.background = palette[2]
color.grid.major = palette[3]
color.grid.minor = palette[3]
color.axis.text = palette[6]
color.axis.title = palette[5]
color.title = palette[7]


#set default theme as minimal(charts will adhere to this in case the custom theme function is not called)
theme_set(theme_minimal(chart.theme.size)) #minimal theme as default

#custom theme function for ggplot2 charts----------------------------------------------------------------------
chart_theme_custom_base <- function(fsize = chart.theme.size,
                                    background_grey = 0,
                                    chart.line.size = 0.5,
                                    vgrid = 1, 
                                    hgrid = 1, 
                                    mvgrid=0, 
                                    mhgrid=0) {
  #size attributes - font, gridline etc.
  gridline.size = .1
  
  #crude hack for geom_text text size scaling
  text.size <<- (5/14)*0.8*fsize 
  
  #Generate the grey colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.grid.minor = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[5]
  color.title = palette[7]
  if(background_grey == 0){color.background = "transparent"}else{color.background = palette[2]}
  
  #change default colors for chart elements
  update_geom_defaults(geom = "bar", list(fill = color_primary_pal[1]))
  update_geom_defaults(geom = "point", list(colour = color_primary_pal[1]))
  update_geom_defaults(geom = "jitter", list(colour = color_primary_pal[1]))
  update_geom_defaults(geom = "line", list(colour = color_primary_pal[1], size=chart.line.size))
  update_geom_defaults(geom = "histogram", list(fill = color_primary_pal[1], colour = "transparent", size = 0.25))
  
  #start construction of the custom theme
  theme_minimal(chart.theme.size) +
  theme(
    
    #format the background and panel
    #Set the entire chart region to a light gray color
    panel.background=element_rect(fill=color.background, color=color.background),
    plot.background=element_rect(fill=color.background, color=color.background),
    panel.border=element_rect(color=color.background, fill = "transparent"),
    
    #Format the grid
    #make gridlines blank if vgrid / hgrid = 0
    panel.grid.major.x = if(vgrid == 0){element_blank()}
                          else{element_line(color=color.grid.major,size=gridline.size)},
    panel.grid.major.y = if(hgrid == 0){element_blank()}
                          else{element_line(color=color.grid.major,size=gridline.size)},
      
    #minor gridlines blank if mvgrid/ mhgrid = 0
    panel.grid.minor.x = if(mvgrid == 0){element_blank()}
                          else{element_line(color=color.grid.minor,size=gridline.size)},
    panel.grid.minor.y = if(mhgrid == 0){element_blank()}
                          else{element_line(color=color.grid.minor,size=gridline.size)},
      
    #Format the legend. Hide by default
    legend.position="none",   #hide the legend
    legend.title = element_text(colour=color.axis.title, size=fsize*0.8, face="bold"),
      #theme(legend.title=element_blank()),
    legend.background = element_rect(fill=color.background, color = "transparent"),
    legend.text = element_text(size=fsize*0.8,color=color.axis.text),
      
    #Format chart title 
    plot.title=element_text(color=color.title, size=fsize*1.2, vjust=1.25, face = "plain"),
    
    #Format axis labels, tick marks, text etc.
    #theme(axis.line.x = element_line(colour = color.grid.major)) +
    #theme(axis.ticks = element_line(colour = color.grid.major)) +
    axis.ticks = element_blank(),
    
    axis.title.x=element_text(size=fsize,color=color.axis.title, vjust=0, face = "plain"),
    axis.title.y=element_text(size=fsize,color=color.axis.title, vjust=1.25, face = "plain"),
    
    axis.text.x=element_text(size=fsize*0.8,color=color.axis.text),
    axis.text.y=element_text(size=fsize*0.8,color=color.axis.text),
      
      
    #Plot margins
    #around entire plot (unit with the sizes of the top, right, bottom, and left margins)
    plot.margin = unit(c(1.35, 2.35, 1.5, 0.5), "cm"), complete = TRUE)

}
  
  
#theme shortcuts - legend_show(position, title etc), axis_labels(remove), data_labels(show + format), axis_label_format(comma, percent)
#axis_rotate - theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))
#teme facet formatter - no gridlines, size of axis lables smaller, only panel inside fill
#color scale shortcuts - selective_bar_color, qualitative, sequence, grey_scale etc.
  
  
 
#function to add footnotes to a ggplot chart
chart_footnote <- function(text = "Source:\nNote:",
                           size = .7, 
                           color = grey(.1)){
  pushViewport(viewport())
  grid.text(label = text ,
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

#Custom watermark
chart_watermark <- function(text = "sumitbajaj.me", size = 3, color = "gray", alpha = 0.8){
  annotate("text", x = Inf, y = -Inf, label = text, hjust=1.1, vjust= -.5, col=color, cex=size, alpha = alpha)
  }

#call the custom ggplot theme with a preset
#chart_theme01 <- do.call(chart_theme_minimal, as.list(chart_format_default))

#chart_format_bar_x_axis_rotate <- theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))



#show legend, select position, show title (1/0)
legend_show <- function(position = "top",
                        show_title = 1,
                        size=chart.theme.size*0.8
                       ){
  theme(legend.position= position, 
        legend.title = if(show_title == 0){element_blank()}
                                      else{element_text(colour=color.axis.title, size=chart.theme.size*0.8, face="bold")})

}

#Over-ride legend symbol size
legend_size <- function(symbol_size = chart.line.size*2){
  guides(colour = guide_legend(override.aes = list(size=symbol_size)))
}
  
#very minimal theme for charts without any gridlines and option to set transparent axis labels
chart_theme_custom_minimal <- function(fsize = chart.theme.size,
                                       background_grey = 0,
                                       chart.line.size = 0.5,
                                       text.x.color = color.axis.text,
                                       text.y.color = color.axis.text,
                                       color.axis.title = color.axis.text,
                                       vgrid = 0, 
                                       hgrid = 0, 
                                       mvgrid=0, 
                                       mhgrid=0){
  
  
  chart_theme_custom_base(fsize = chart.theme.size,
                          background_grey = background_grey,
                          chart.line.size = chart.line.size,
                          vgrid = vgrid, 
                          hgrid = hgrid, 
                          mvgrid=mvgrid, 
                          mhgrid=mhgrid) + 
  theme(axis.ticks = element_blank(),
        axis.title.x=element_text(size=fsize,color=color.axis.title, vjust=0),
        axis.title.y=element_text(size=fsize,color=color.axis.title, vjust=1.25),
        axis.text.x=element_text(size=fsize,color = text.x.color),
        axis.text.y=element_text(size=fsize,color = text.y.color))
}

#function to scale text size for geom_text
text_size_scale <- function(font_size = chart.theme.size){
  #crude hack for geom_text text size scaling
  return ((5.0/14.0)*font_size)
}


#Display all color palettes
#display.brewer.all()
# View 5 colors for the Set2 palette
#display.brewer.pal(3,"Paired")
#http://vis.stanford.edu/color-names/analyzer/
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#http://coolors.co/browser
#http://www.inside-r.org/packages/cran/ggthemes/docs/tableau_color_pal

#Extract colors from standard palette
#col_pal = brewer.pal(3, "Set2")
#color1 = col_pal[1]

#GGplot theme customization
#http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/#tip-on-creating-a-custom-theme

#useful links 
#http://docs.ggplot2.org/0.9.3.1/scale_date.html
#http://docs.ggplot2.org/0.9.3.1/scale_datetime.html

