#this code makes the incme status figre and summary tbles for each country
#This script plots the main article figure - BAU, ambitious, and Target
source(here::here("r.scripts", "01-set-up.R"))
source(here::here("r.scripts", "03-emissions-model.R"))

### Take mean, 10th and 90th quantiles
results$av.val=(rowSums(results[,5:1004])/1000)
results$percent10 <- q <- rowQuantiles(as.matrix(results[,5:1004]), probs = 0.1)
results$percent90 <- q <- rowQuantiles(as.matrix(results[,5:1004]), probs = 0.9)

Income_status  <- results %>%
  dplyr::select(Year, Scenario, country, eco_status, av.val, percent10, percent90) %>% 
  filter(Scenario %in% c("Business as usual", "Ambitious", "Target <8 Mt")) %>% 
  group_by(Year, Scenario, eco_status) %>% 
  summarise(med=sum(av.val, na.rm=TRUE), 
            lwr = sum(percent10, na.rm = TRUE),
            upr = sum(percent90, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Scenario=fct_relevel(Scenario,"Business as usual", "Ambitious", "Target <8 Mt"), #reorder for plotting 
         eco_status= fct_relevel(eco_status, "HI", "UMI", "LMI", "LI")) #reorder for plotting 
#
Income <- Income_status %>% 
  ggplot(aes(Year, med, group=Scenario)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr,fill=Scenario), alpha=.3) + 
  scale_colour_manual(values=c("#B8DE29FF", ambitious, target))+
  geom_line(aes(colour = Scenario)) +
  scale_fill_manual(values=c(bau, ambitious, target))+ 
  #geom_point(color="black", size=0.1)+
  scale_y_continuous() +
  facet_grid("eco_status")+
  scale_x_continuous(limits = c(2016,2030), expand = c(0, 0)) +
  labs(x ="Year", y = "")+#Annual plastic emissions (Mt)
  #ggtitle("")+
  PPEG_theme +
  #geom_hline(yintercept = 8,  lty=1, col=line8mt) +
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(1,1,1,0.5), "lines"))

ggsave(here::here("figs", "FigS4-Income-Emissions.pdf"), 
       width = 5, height =11)


#tables of global annual emissions and country level emissions 
Managed2016 <-country.data %>% group_by(eco_status) %>% summarise(managed = 1-mean(total_missman))
head(Managed2016)
#summry table of country level emissions
names(results)
summary  <- results %>%
  dplyr::select(country, Year, Scenario,  av.val ,percent10, percent90) %>%
  mutate_if(is.numeric, round, 5)
head(summary)
tablab <- "Country-Summary" # table label
tname <- paste("./tables/", tablab, st, ".csv", sep = "") #
write.table(summary, tname,  #change the file name for a new run
            sep=",", col.names= T, row.names=F)
