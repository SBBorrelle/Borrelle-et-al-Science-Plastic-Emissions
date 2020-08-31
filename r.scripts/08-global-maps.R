## modified from
## https://stackoverflow.com/questions/30706124/plotting-the-world-map-in-r
library(ggplot2)
library(dplyr)
library(maps)
library(viridis)
library(mapproj)

## plastic outputs
rm(list=ls())
dat <- read.csv('./tables/Appendix_1_InputVariables_2020-01-01.csv', stringsAsFactors=FALSE)
## convert to ISO code for both
dat$region <- dat$iso3c

## World Data for maps
wd <- map_data('world')
wd$country <- wd$region
wd$region <- iso.alpha(wd$region,3)


## Annoyingling WD uses the same code for multiple countries
codes <- wd %>% group_by(region) %>%
  summarize(n.countries=length(unique(country))) %>%
  filter(n.countries>1) %>% pull(region)
filter(wd, region %in% codes) %>% dplyr::select(region, country) %>%
  distinct()

## Some country regions don't match?
miss.region <- dat$region[which(!dat$region %in% wd$region)]
miss.country <- dat$Country[which(!dat$region %in% wd$region)]
cbind(miss.region, miss.country)

## The code (region) doesn't match so fix it: convert DMA to DOM
## for Dominican Republic
unique(wd$country[grep('Dominic', wd$country)])
unique(wd$region[grep('Dominic', wd$country)])
wd$region[grep('Dominican Republic', wd$country)]  <- 'DOM'

## Gibraltar?? No match in maps
## unique(wd$country[grep('Gibr', wd$country)])
## Convert GNB to GIN for Guinea-Bissau
unique(wd$country[grep('Bissau', wd$country)])
unique(wd$region[grep('Bissau', wd$country)])
wd$region[grep('Guinea-Bissau', wd$country)]  <- 'GNB'

## Hong Kong doesn't exist in map data base???
## unique(wd$country[grep('Hong', wd$country)])
## Macau also missing
## unique(wd$country[grep('Mac', wd$country)])

## Annoyingly WD uses the same code for Nigeria and Niger. So
## convert Nigeria NER to NGA
unique(wd$country[grep('Niger', wd$country)])
unique(wd$region[grep('Niger', wd$country)])
wd$region[grep('Nigeria', wd$country)]  <- 'NGA'

## Tuvalu also missing
## unique(wd$country[grep('Tuv', wd$country)])
## Virgin islands exists but has NA for country code?? I guess
## replace the map code
unique(wd$country[grep('Virgin', wd$country)])
unique(wd$region[grep('Virgin', wd$country)])
wd$region[grep('Virgin', wd$country)] <- 'VIR'
## Check after fixing as many as possible
miss.region <- dat$region[which(!dat$region %in% wd$region)]
miss.country <- dat$Country[which(!dat$region %in% wd$region)]
cbind(miss.region, miss.country)


dat <- filter(dat, !Country %in% miss.country)
#dat <- filter(dat, X2029 > 100000)

## Convert outputs to more useful units 
dat$per_cap_emissions_amb_2030 <-
  dat$per_cap_emissions_amb_2030*1000000 # metric tons
dat$X2030 <- log10(dat$X2030) # millions of people
dat$per_cap_emissions_2016 <-
  dat$per_cap_emissions_2016*1000000 # metric tons
dat$X2016 <- dat$X2016/1000000 # millions of people

## Note the geom_map apparently only works if column is called
## 'region'
scens <- c('Eratio_med', 'per_cap_emissions_bau_2030', 'X2030',
           'percent_plastic', 'Kg_capita_day_waste_gen',
           'total_missman')

scens <- c('Eratio_med', 'per_cap_emissions_2016', 'X2016',
           'percent_plastic', 'Kg_capita_day_waste_gen',
           'total_missman')

p <- list()
for(i in 1:length(scens)){
  ss <- scens[i]
  zlims <- list(NULL,#c(0,1),
                c(0, max(dat$per_cap_emissions_2016)),
                NULL, NULL, NULL, NULL)
  titles <- c('Emissions Ratio', 'Per Capita Emissions in 2016',
              'Population 2016', '% Plastic in Waste',
              'KG waste/capita/day', 'Proportion Inadequately Managed Waste')
  p[[i]] <- ggplot() +
    geom_map(data = wd, map = wd,
             aes(group = group, map_id=region),
             fill = gray(.8), colour = gray(.6), size=0.15) +
    geom_map(data = dat, map=wd,
             aes_string(fill=ss, map_id='region'),
             colour=gray(.6), size=0.15) +
    coord_map("mollweide", xlim=c(-180,180), ylim=c(-60, 90)) +
    ## scale_fill_continuous(low="thistle2", high="darkred",
    ##                       guide="colorbar", limits=zlims[[i]]) +
    scale_fill_viridis(option='viridis', limits=zlims[[i]])+
    ##scale_fill_distiller(palette = "Spectral", limits=zlims[[i]]) +
    labs(fill=' ', title=titles[i], x="", y="") + theme_bw() +
    theme(legend.position = c(0.1, 0.4), legend.key.size=unit(.3, units='in'))
  ggsave(paste0('map_', ss,'.png'), p[[i]], width=7, height=3.5,
         dpi=500, units='in')
}

#dat[which.max(dat$per_cap_emissions_bau_2030) ,]

library(cowplot)
g <- plot_grid(p[[5]], p[[4]], p[[6]], p[[1]],ncol=1, labels=c('A','B','C', 'D'))
ggsave('maps_all.png', g, width=7, height=3.5*4, dpi=500, units='in')

pop <-  plot(p[[3]])
ggsave('population_2016.png', pop, width = 7, height=3.5, dpi=500, units = 'in')
