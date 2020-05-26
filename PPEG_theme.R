PPEG_theme <- theme_bw()  +
  theme(axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 14),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid.minor.x=element_blank(),
        #panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        #panel.grid.major.y=element_blank(),
        #axis.ticks.length = unit(-0.25 , "cm"), # -ve length = inside ticks - although this fucks up the axis labels and cant figure out vjust to correct it
        axis.text.x = element_text(color="black", 
                                   size=12, vjust = 0.5), # margin between the ticks and the text
        legend.title = element_blank(),  # element_text(size = 12, face="plain"),
        legend.text = element_text(size = 12),
        legend.key = element_rect(colour="white"),
        legend.background = element_blank(),
        legend.justification=c(0,1), 
        legend.position=c(0, 1.02),
        #legend.position = "left",
        plot.margin = margin(t=.5, r=.75, b=.5, l=.5, unit = "cm")
  ) 
  


