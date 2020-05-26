#This script plots the main article figure - BAU, ambitious, and Target
source(here::here("r.scripts", "01-set-up.R"))
source(here::here("r.scripts", "03-emissions-model.R"))

### Take mean, 10th and 90th quantiles
results$av.val<- (rowSums(results[,5:1004])/1000) #mean of monte carlo simulation
results$percent10 <- q <- rowQuantiles(as.matrix(results[,5:1004]), probs = 0.1)
results$percent90 <- q <- rowQuantiles(as.matrix(results[,5:1004]), probs = 0.9)

unique(results$Scenario)

Global_emissions  <- results %>%
  dplyr::select(Year, Scenario, country, eco_status, av.val, percent10, percent90) %>%  #select the mean and 80% credible intervals 
  filter(Scenario %in% c("Business as usual", "Ambitious", "Target <8 Mt")) %>% 
  group_by(Year, Scenario) %>% 
  summarise(med=sum(av.val, na.rm=TRUE),  
            lwr = sum(percent10, na.rm = TRUE),
            upr = sum(percent90, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Scenario=fct_relevel(Scenario, "Business as usual", "Ambitious", "Target <8 Mt")) %>%   #reorder for plotting 
  mutate_if(is.numeric, round, 2)

#plot annual emissions global
Global <- Global_emissions %>% 
  ggplot(aes(Year, med, group=Scenario)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr,fill=Scenario), alpha=.3) + 
  scale_colour_manual(values=c( "#B8DE29FF", ambitious, target))+
  geom_line(aes( colour = Scenario)) +
  scale_fill_manual(values=c( bau, ambitious, target))+ 
  scale_x_continuous(limits = c(2016,2030), expand = c(0, 0)) +
  scale_y_continuous(breaks=c(0, 8, 10,  20, 30, 40, 50, 60, 70, 80, 90, 100, 110), 
                     labels = c("0", "8",  "","20", "30", "40", "50", "60", "70", "80","90", "100", "110"))+
  geom_hline(yintercept = 8,  lty=1, col=line8mt) +
  labs(x ="Year", y = "Plastic emissions (Mt)")+
  PPEG_theme +
  theme(plot.margin = unit(c(1,1,1,0.5), "lines"))


## Need to run 06-Income-summary.R 
source(here::here("r.scripts", "06-Income-summary.R"))
#plot Figure 1 in the manuscript
quartz()
ggarrange(Global, Income, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1,
          widths = c(1.8, 1))

ggsave(here::here("figs", "Fig1-Emissions.pdf"), 
       width = 8, height = 6)


#write emissions table to file
tablab <- "S4Table_Scenarios_2030_" # name of the output
tname <- paste("./tables/",tablab,st, ".csv", sep = "") #
write.table(Global_emissions, tname,  #change the file name for a new run
            sep=",", col.names= T, row.names=F)

