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

