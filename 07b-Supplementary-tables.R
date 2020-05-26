##### Make output tables of summary results for country, income status and global emissions for all of the trials
### Take mean, 10th and 90th quantiles
# results$av.val=(rowSums(results[,5:1004])/1000)
# results$percent10 <- q <- rowQuantiles(as.matrix(results[,5:1004]), probs = 0.1)
# results$percent90 <- q <- rowQuantiles(as.matrix(results[,5:1004]), probs = 0.9)

#total global emissions - TABLE S5
global_emissions_trials  <- results %>%
  dplyr::select(Year, Scenario, country, eco_status, av.val, percent10, percent90) %>%  #select the mean and 80% credible intervals 
  group_by(Year, Scenario) %>% 
  summarise(med=sum(av.val, na.rm=TRUE), 
            lwr = sum(percent10, na.rm = TRUE),
            upr = sum(percent90, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, round, 2)
#write emissions table to file
tablab <- "TableS3_Global_trials_2030_" # name of the output
tablab <- "TableS4_Global_2030_" # name of the output
tname <- paste("./tables/",tablab,st, ".csv", sep = "") #
write.table(global_emissions_trials, tname,  #change the file name for a new run
            sep=",", col.names= T, row.names=F)

##Emissions for economic status - APPENDIX 3
ecostatus_emissions_trials  <- results %>%
  dplyr::select(Year, Scenario, country, eco_status, av.val, percent10, percent90) %>%  #select the mean and 80% credible intervals 
  group_by(Year, Scenario, eco_status) %>% 
  summarise(med=sum(av.val, na.rm=TRUE), 
            lwr = sum(percent10, na.rm = TRUE),
            upr = sum(percent90, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, round, 3)
#write emissions table to file
tablab <- "Appendix_3b_Ecostatus_trials_2030_" # name of the output
tablab <- "Appendix_3_Ecostatus_2030_" # name of the output
tname <- paste("./tables/",tablab,st, ".csv", sep = "") #
write.table(ecostatus_emissions_trials, tname,  #change the file name for a new run
            sep=",", col.names= T, row.names=F)

#country level emissions - APPENDIX 4
country_emissions_trials  <- results %>%
  dplyr::select(Year, Scenario, country, eco_status, av.val, percent10, percent90) %>%  #select the mean and 80% credible intervals 
  group_by(Year, Scenario, country) %>% 
  summarise(med=sum(av.val, na.rm=TRUE), 
            lwr = sum(percent10, na.rm = TRUE),
            upr = sum(percent90, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, round, 6)
#write emissions table to file
tablab <- "Appendx_4b_Country_trials_2030_" # name of the output
tablab <- "Appendx_4_Country_2030_" # name of the output
tname <- paste("./tables/",tablab,st, ".csv", sep = "") #
write.table(country_emissions_trials, tname,  #change the file name for a new run
            sep=",", col.names= T, row.names=F)

