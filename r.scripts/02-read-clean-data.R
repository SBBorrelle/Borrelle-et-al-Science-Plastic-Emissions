#this script reads in data and matches the datasets together
source(here::here("r.scripts", "01-set-up.R")) #load packages

# waste generation per capita, proportion plastic in waste, and expected change in waste management, mismanaged waste levels for 2016
country.data <- read_csv(here::here("inputs", "01-waste-inputs.csv"))  %>% # waste generation per capita and expected change in waste, inadequately managed waste
  dplyr::select(country, eco_status, iso3c, region_id,
         Kg_capita_day_waste_gen,
         annual_change_waste_gen,
         percent_plastic,
         total_missman)
#make all of the inputs numeric if not characters
country.data[] <- lapply(country.data, function(x) {
  if(is.double(x)) as.numeric(as.character(x)) else x
})
sapply(country.data, class)

## emission.ratios (product of  03-spatial-emissions-ratio.R)
emission.ratios <- read.csv(here::here('inputs', '02-spatial_emission_ratios.csv'))

#population growth data from World Bank 
pop.data <-  read.csv(here::here("inputs","03-population-growth_PF.csv"))  # World Bank population data from 1960-2050
pop.data2 <- cbind(pop.data[1:2], pop.data[59:73])  # gets years 2016-2030
names(pop.data2[1:5])

## max proportion of plastic in the waste stream by 2030 - changable
waste.prop.data <- read_csv(here::here("inputs", "04-waste-proportions_PF.csv")) 
names(waste.prop.data)
# [1] "eco_status" "prop_2030"

#country codes
countries <- read.csv(here::here("inputs", "05-country-codes_PF.csv")) 
all.data <- data.frame(all=unique(countries$iso3c))
names(all.data)

#the scenarios for the manuscript
scen.data <- read.csv(here::here("inputs", "07-scenarios_Manuscript.csv")) %>%
   arrange(match(Scenario, c("Business as usual", "Ambitious", "Target <8 Mt")),
           desc(action), desc(eco_status),
           desc(Value), desc(min_rate), desc(max_rate))
#[1] "Scenario"   "action"     "eco_status" "Value"
unique(scen.data$Scenario)
names(scen.data)

# match data together based on country
match.iso=data.frame()
for(x in unique(all.data$all)){ #loop thru all data inputs and 'any' returns a list of iso codes that are mentioned anywhere in the input files
  c.dat=any(x==country.data$iso3c)
  p.dat=any(x==pop.data2$iso3c)
  s.dat=any(x==emission.ratios$iso3c)
  temp.dat=data.frame(iso3c=x, c.dat=c.dat, p.dat=p.dat, s.dat=s.dat)
  match.iso=rbind(match.iso, temp.dat) #join together into a dataframe called 'temp.data'
}

match.iso$match <- paste0(match.iso$c.dat, match.iso$p.dat, match.iso$s.dat)  #test - joins all of the trues and falses together - so we can select only the countries where all are true
country.list <- match.iso[which(match.iso$match == "TRUETRUETRUE"),] #vector that we trun thru - all values are TRUE - data needs to exist in all data frames or crashes

## #export the data used in the model as appendix
tablab <- "Appendix_1_InputVariables_" # name of the output
tname <- paste("./tables/",tablab,st, ".csv", sep = "") #
tname
x <- read.csv(here::here("inputs", "01-waste-inputs.csv"))
x1 <- full_join(x, pop.data2, by="iso3c")
x2 <- read.csv(here::here("tables", "Appendx_4_Country_2030_2019-12-19.csv"))
x2016 <- x2 %>% 
  dplyr::filter(Year ==2016) %>% 
  dplyr::filter(Scenario == "Ambitious") %>% 
  dplyr::select(-Scenario, -Year) %>% 
  rename(., iso3c = country,
         med_2016 = med,
         lwr_2016 = lwr,
         upr_2016= upr)

x3 <- full_join(x1, x2016, by="iso3c")
xscen2030 <-x2 %>% 
  dplyr::filter(Year ==2030) %>% 
  dplyr::filter(Scenario == "Ambitious") %>% 
  dplyr::select(-Scenario, -Year) %>% 
  rename(., iso3c = country,
         med_ambitious_2030 = med,
         lwr_ambitious_2030 = lwr,
         upr_ambitious_2030= upr)
x4 <- full_join(x3, xscen2030, by="iso3c")

xscen2030_bau <-x2 %>% 
  dplyr::filter(Year ==2030) %>% 
  dplyr::filter(Scenario == "Business as usual") %>% 
  dplyr::select(-Scenario, -Year) %>% 
  rename(., iso3c = country,
         med_bau_2030 = med,
         lwr_bau_2030 = lwr,
         upr_bau_2030= upr)
x5 <- full_join(x4, xscen2030_bau, by="iso3c")

xscen2030_tar <-x2 %>% 
  dplyr::filter(Year ==2030) %>% 
  dplyr::filter(Scenario == "Target <8 Mt") %>% 
  dplyr::select(-Scenario, -Year) %>% 
  rename(., iso3c = country,
         med_target_2030 = med,
         lwr_target_2030 = lwr,
         upr_target_2030= upr)
x6 <- full_join(x5, xscen2030_tar, by="iso3c")


#averaage of emissions ratio
emission.ratios$av.val<- (rowSums(emission.ratios[,3:1002])/1000) #mean of monte carlo simulation
emission.ratios$percent10 <- q <- rowQuantiles(as.matrix(emission.ratios[,3:1002]), probs = 0.1)
emission.ratios$percent90 <- q <- rowQuantiles(as.matrix(emission.ratios[,3:1002]), probs = 0.9)
ratio  <- emission.ratios %>%
  dplyr::select(iso3c, Country, av.val, percent10, percent90) %>%  #select the mean and 80% credible intervals
  group_by(iso3c, Country) %>%
  summarise(Eratio_med=sum(av.val, na.rm=TRUE),
            Eratio_lwr = sum(percent10, na.rm = TRUE),
            Eratio_upr = sum(percent90, na.rm = TRUE)) %>%
  ungroup()

inputdata <- x6 %>%
  dplyr::select(-country.x, -country.y) %>%
  full_join(.,ratio, by="iso3c") %>%
  drop_na(c(Eratio_med, X2016, eco_status)) %>%
  dplyr::select(Country, everything()) %>% 
  mutate(per_cap_emissions_2016 = med_2016/X2016,
         per_cap_emissions_amb_2030 = med_ambitious_2030/X2030,
         per_cap_emissions_bau_2030 = med_bau_2030/X2030,
         per_cap_emissions_tar_2030 = med_target_2030/X2030)


write.table(inputdata, tname,  #change the file name for a new run
             sep=",", col.names= T, row.names=F)
