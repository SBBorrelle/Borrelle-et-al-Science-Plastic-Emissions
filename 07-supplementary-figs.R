#This script makes supplementary figures and tables
source(here::here("r.scripts", "01-set-up.R"))
source(here::here("r.scripts", "02-read-clean-data.R"))
source(here::here("r.scripts", "04-emissions-model.R"))
### Take mean, 10th and 90th quantiles
results$av.val=(rowSums(results[,5:1004])/1000)
results$percent10 <- q <- rowQuantiles(as.matrix(results[,5:1004]), probs = 0.1)
results$percent90 <- q <- rowQuantiles(as.matrix(results[,5:1004]), probs = 0.9)

#NOTE : USE THE EMISSIONS SCENARIOS '06-scenari0-runs.R' for this figure
#change the names of the scenarios in the 'scenarios_runs.csv'
scenario_names <- c(
  `4-reduction-1` = "25% Reduction",
  `4-reduction-2` = "50% Reduction",
  `4-reduction-3` = "75% Reduction",
  `4-reduction-4` = "85% Reduction",
  `5-wm-1` = "Waste management-1",
  `5-wm-2` = "Waste management-2",
  `5-wm-3` = "Waste management-3",
  `5-wm-4` = "Waste management-4",
  `6-recovery-1` = "25% Recovery",
  `6-recovery-2` = "50% Recovery",
  `6-recovery-3` = "75% Recovery",
  `6-recovery-4` = "85% Recovery",
  `trial-1` = "Target trial #1",
  `trial-2` = "Target trial #2",
  `trial-3` = "Target trial #3",
  `trial-4` = "Target trial #4",
  `trial-5` = "Target trial #5",
  `trial-6` = "Target trial #6",
  `trial-7` = "Target trial #7",
  `trial-8` = "Target trial #8",
  `trial-9` = "Target trial #9"
)

unique(results$country) ## Choose a country to plot
# Figure S2 - scenarios of individual strategies
filter(results, country=='IDN') %>%
  gather(replicate, plastic, -Year, -country, -eco_status, -Scenario) %>%
  mutate(replicate=as.numeric(gsub('rep.', '', replicate))) %>%
  filter(replicate<100) %>% ## makes it plot faster
  filter(Scenario %in% c("4-reduction-1", "4-reduction-2", "4-reduction-3","4-reduction-4",
                          "5-wm-1","5-wm-2", "5-wm-3", "5-wm-4",
                         "6-recovery-1", "6-recovery-2", "6-recovery-3","6-recovery-4")) %>%
  ggplot(aes(Year, plastic, group=replicate)) + geom_line(alpha=0.1) +
  facet_wrap('Scenario', labeller = as_labeller(scenario_names), ncol=4)+ #+ scale_y_log10()+
  labs(x ="Year", y = "Plastic emissions Mt") +
  theme(panel.spacing = unit(1.5, "lines"))+
  ggtitle("Emissions reduction scenarios: Indonesia")
ggsave(here::here("figs", "FigS2-Indonesia-strategies.pdf"), 
       width = 10, height = 10)

#figure S3  the trials for Indonesia
filter(results, country=='IDN') %>%
  gather(replicate, plastic, -Year, -country, -eco_status, -Scenario) %>%
  mutate(replicate=as.numeric(gsub('rep.', '', replicate))) %>%
  filter(replicate<100) %>% ## makes it plot faster
  filter(Scenario %in% c("trial-1", "trial-2", "trial-3", 
                       "trial-4", "trial-5", "trial-6",
                       "trial-7", "trial-8", "trial-9")) %>%
  ggplot(aes(Year, plastic, group=replicate)) + geom_line(alpha=0.1) +
  facet_wrap('Scenario', labeller = as_labeller(scenario_names), ncol=3)+ #+ scale_y_log10()+
  labs(x ="Year", y = "Plastic emissions Mt") +
  theme(panel.spacing = unit(1.2, "lines"))+
  ggtitle("Trials for target < 2 Mt: Indonesia")
ggsave(here::here("figs", "FigS3-Indonesia-trials.pdf"), 
       width = 8, height = 8)

#Plot of the target trials to get below 2 Mt
results %>%
  dplyr::select(Year, Scenario, country, eco_status, av.val, percent10, percent90) %>%  #select the mean and 80% credible intervals 
  filter(Scenario %in% c("trial-1", "trial-2", "trial-3", 
                         "trial-4", "trial-5", "trial-6",
                         "trial-7", "trial-8", "trial-9")) %>% 
  group_by(Year, Scenario) %>% 
  summarise(med=sum(av.val, na.rm=TRUE), 
            lwr = sum(percent10, na.rm = TRUE),
            upr = sum(percent90, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  ggplot(aes(Year, med, group=Scenario)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=Scenario), alpha=.3) +
  scale_colour_manual(values=c(target, target, target,target, target, target,target, target, target))+
  geom_line(aes(colour = Scenario)) +
  scale_fill_manual(values=c(target, target, target,target, target, target,target, target, target))+ 
  #geom_point(color="darkgrey", size=1)+
  scale_x_continuous(limits = c(2016,2030), expand = c(0, 0)) +
  scale_y_continuous(breaks=c(0, 8, 10,  20, 30, 40, 50), 
                     labels = c("0",  "8",  "","20", "30", "40", "50"))+
  #geom_hline(yintercept = 2,  lty=1, col=line2mt) + 
  geom_hline(yintercept = 8,  lty=1, col=line8mt) +
  facet_wrap('Scenario', labeller = as_labeller(scenario_names), ncol=3)+ #+ scale_y_log10()+
  labs(x ="Year", y = "Plastic emissions (Mt)")+
  PPEG_theme + 
  theme(legend.position = "none")
ggsave(here::here("figs", "FigS4-Target-trials.pdf"), 
       width = 8, height = 8)


#scale_y_continuous(labels = scales::comma)+ #code for removing scientific notation
