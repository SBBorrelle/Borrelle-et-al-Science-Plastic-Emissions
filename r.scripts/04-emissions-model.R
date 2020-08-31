## This code calculates the business as usual emsisions scenario and
## intervention scenarios to create country level plots of
## intermediary steps if desired

rm(list=ls())
source(here::here("r.scripts", "01-set-up.R"))
source(here::here("r.scripts", "02-read-clean-data.R"))

names(scen.data)[1] <- "Scenario"
str(scen.data) #check the input values

## make intermediate steps, useful for exploring/checking code
set.seed(1421)                          # make reproducible
runs <- beta.list <- Missman.list <- Waste.list <- list(); zz <- 1                 # intermediate containers
## years 2015-2030 for vectorized calcs;  Year for naming
yri <- 0:14; Year <- 2016:2030; nyr <- sum(yri>0)
#if you want to only run one country change line 20: 
## unique(country.list$iso3c) #list of uniques countries
##for(i in unique(country.list$iso3c[13])){ select country "13" from the list - here its "BEN"
for(i in unique(country.list$iso3c)){
  ## Pull out the country-specific data
  country <- country.data[which(country.data$iso3c == i),]
  ## Emission ratios (% plastic going to ocean)
  eratios <- as.numeric(emission.ratios[which(emission.ratios$iso3c == i),-(1:2)])
  ## population from 2016 to 2030
  pop <- as.numeric(subset(pop.data2, iso3c==i, select=-c(1,2)))
  ## kg waste per capita per year
  Waste.year <- country$Kg_capita_day_waste_gen*365
  eco.status <- country$eco_status      # list of economic status
  pplastic <- country$percent_plastic   # percent plastic
  missman <- country$total_missman # percent plastic mismanaged
  man <- 1-missman # % properly managed
  ## change in proportion of plastic in the waste stream
  ppmax <- waste.prop.data[which(waste.prop.data$eco_status==country$eco_status),]$prop_2030
  scenmax <- 1-(scen.data[which(scen.data$eco_status==country$eco_status),]$Value) #maximum target value of each scenario for each income status
  scenarios <- scen.data[which(scen.data$eco_status == eco.status),]
  ## Loop over each scenario for this country
  for(k in unique(scen.data$Scenario)){
    scenario <- scenarios[which(scenarios$Scenario== k),]
    ## scenario specific values for reduction, waste management and recovery
    rd <- scenario[which(scenario$action == "rd"),'Value'] #reduction value for scenario
    wm <- scenario[which(scenario$action == "wm"),'Value'] #waste mangement value for scenario
    wm_min <- scenario[which(scenario$action == "wm"),'min_rate'] #minimum WM value
    wm_max <- scenario[which(scenario$action == "wm"),'max_rate'] #max WM value
    cu <- scenario[which(scenario$action == "cu"),'Value'] #recovery 
    ## These do not have MC so just do them once here
    Reduction <- pmax(0, pmin(1, 1+yri*((rd-1)/nyr))) #implementation rate
    Capture <- pmax(0, pmin(1, 1-yri*((1-cu)/nyr)))#implementation rate
    ## Preallocate annual uncertainty for better efficiency
    waste.slope <- runif(1000, 1.0054, 1.0154)# growth in waste between 0.54 and 1.54% per annum based on World bank predictions
    beta.slope <- runif(1000, 1, 1.10) # growth in plastic proportion in waste between 0 and 10% per annum
    Missman.slope <- runif(1000, wm_min, wm_max) #value that is set for scenario
    final <- Waste <-  beta <- Missman <-
      matrix(NA, nrow=1000, ncol=length(yri))
    for(j in c(1:1000)){ # loops (k,i,j) every scenario, every country, 1000 x
      ##Monte carlo the waste generation
      Waste[j,] <-
        pmax(Waste.year, (Waste.year*country$annual_change_waste_gen^yri)*waste.slope[j]^yri)
      ## Monte Carlo simulation plastic proportion. If
      ## pplastic>ppmax it will be that (won't decrease)
      ## otherwise it will grow linearly but not be larger than ppmax.
      beta[j,] <-
        pmax(pplastic, pmin(ppmax, pplastic*beta.slope[j]^yri))
      ## Misman decreases exponentially from initial missman value, cannot
      ## increase, and will not go below (1-wm) which depends on the scenario
      if(missman<1-wm){
        Missman[j,] <- missman
      } else {
        Missman[j,] <-
          pmax(1-wm, pmin(missman, 1-(man*Missman.slope[j]^yri)))
      }
      ## metric `tons of plastic in aquatic ecosystems; equation SI.1
      final[j,] <-
        pop*Waste[j,]*beta[j,]*eratios[j]*(Missman[j,]*Reduction*Capture)/1e9 #kgs to million metric tonnes 
    }
    ## Store results for this scenario in temporary container
    ## that is efficient in R
    Missman.list[[zz]] <-
      data.frame(country=i, scenario=k, rep=1:1000, Missman)
    Waste.list[[zz]] <-
      data.frame(country=i, scenario=k, rep=1:1000, Waste)
    beta.list[[zz]] <-
      data.frame(country=i, scenario=k, rep=1:1000, beta)
    runs[[zz]] <- data.frame(Year, Scenario=k, country=i,
                             eco_status=eco.status, t(final),
                             stringsAsFactors=FALSE)
    zz <- zz+1
  }
}
## Combine everything together with fast binding and then and
## clean up names
results <- as.data.frame(data.table::rbindlist(runs))
names(results)[-(1:4)] <- paste0("rep.", 1:1000)
## Check reproducibility
results[1,5] #[1]  million metric tonnes

## Try plotting a few countries to see if it makes sense
filter(results, country=='KHM') %>% #change the ISO code to see a different country
  gather(replicate, plastic, -Year, -country, -eco_status, -Scenario) %>%
  mutate(replicate=as.numeric(gsub('rep.', '', replicate))) %>%
  filter(replicate<100) %>% ## makes it plot faster
  ggplot(aes(Year, plastic, group=replicate)) + geom_line(alpha=0.1) +
  facet_wrap('Scenario', ncol=3) + scale_y_log10()

## Checks on the model
## Look at Monte Carlo output by factor
betas <- as.data.frame(data.table::rbindlist(beta.list))
missmans <- as.data.frame(data.table::rbindlist(Missman.list))
Wastes <- as.data.frame(data.table::rbindlist(Waste.list))
names(missmans)[-(1:3)] <- names(betas)[-(1:3)] <- names(Wastes)[-(1:3)] <- Year
mc <- rbind(cbind(factor='beta', betas),
            cbind(factor='missman', missmans),
            cbind(factor='Waste', Wastes)) %>%
  gather(year, value, -factor, -country, -scenario, -rep) %>%
  mutate(year=as.numeric(year))
## Merge the plastic onto this to make a single plot by country
results.long <- dplyr::select(results, -eco_status) %>%
  gather(rep, value, -Year, -Scenario, -country) %>%
  mutate(factor='emissions', rep=as.numeric(gsub('rep.', '', x=rep))) %>%
  rename(year=Year, scenario=Scenario)
mc <- rbind(mc, results.long)
head(mc)
unique(mc$factor)
## For each country plot the different factors and output emission
pdf(file='figs/Appendix_2_country_projections.pdf', onefile=TRUE, width=7, height=5)
for( cc in unique(mc$country)){
  g <- filter(mc,  country==cc & rep <100) %>%
    ggplot(aes(year, value, group=rep)) +
    facet_grid(factor~country+scenario, scales='free_y') +
    geom_line(alpha=.2) + theme_bw()
  print(g)
}
dev.off()
#Check a single country
filter(mc,  country=="IDN" & rep <100) %>%
  ggplot(aes(year, value, group=rep)) +
  facet_grid(factor~country+scenario, scales='free_y') +
  geom_line(alpha=.2) + theme_bw()


#save the results including MC simulations to file ## its large (>50mb) so need to save to symlink folder or not push to github
st=format(Sys.time(), "%Y-%m-%d") #this gets the timestamp
tablab <- "Emissions_2030_" # name of the output
tname <- paste("./r.outputs/tables/",tablab,st, ".csv", sep = "") #
tname

write.table(results, tname,  #change the file name for a new run
            sep=",", col.names= T, row.names=F)

#rm(list= ls()[!(ls() %in% c('results'))])