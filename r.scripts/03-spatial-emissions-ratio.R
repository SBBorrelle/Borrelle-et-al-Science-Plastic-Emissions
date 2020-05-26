##### What does this code do?
# Translates spatial waste generation data from maps to probabilities,
# using a falloff model (exponential decay) and MC simulations to generate uncertainty
## Note these are *not* probabilities. They can be interpreted as
## proportions but not probabilities.

##### Dependencies:
# To run this requires the following spatial data:
# distance to ocean map: distance map global-a_nodata.tif 
# spatial layer of waste generation: LebretonAndrady2019_TotalPlasticWaste.tif https://figshare.com/s/843a17a4995b4c9e8af6
# world boundaries: TM_WORLD_BORDERS-0.3

library(here)
##Note - this code takes a couple of hours to run.
source(here::here("r.scripts", "01-set-up.R")) #call packages / set up

## Save maps of steps so can check them individually.
plot.leak.map <- function(){
  png(paste0('leak/leak_maps_', i,".png"), width=10, height=3.75,
      units='in', res=400)
  total.temp <- mismap_mask
  #drain.temp <- drainmap_mask
  land.temp <- total.temp*log(drainmap_mask+1,101)
  ocean.temp <- total.temp-land.temp
  ratio <- ocean.temp/total.temp
  ## Ditch these for better plotting and consistent zlim in log
  ## space
   land.temp[land.temp==0] <- NA
   land.temp[land.temp<=1] <- NA
   ocean.temp[ocean.temp==0] <- NA
  z1 <- cellStats(total.temp, 'max')
  z0 <- min(cellStats(land.temp, 'min'), cellStats(ocean.temp, 'min'))
  zlim <- c(log(z0), log(z1))
  par(mfrow=c(1,3), oma=c(0,0,3,1), mar=c(2.5,2.5,2.5,3),
      mgp=c(3,.5,0), cex.axis=.9)
  plot(log(total.temp), main='Plastic waste generated (log kg)', zlim=zlim); lines(border)
  #plot(log(drain.temp),  main='Drainage',  zlim=c(0,1)); lines(border)
  plot(1-log(drainmap_mask+1.1,101), main='% plastic to aquatic ecosystem', zlim=c(0,1)); lines(border)
  #plot(log(ocean.temp), main='Potential plastic emissions (log kg)', zlim=zlim); lines(border)
  plot(ratio,  main='Emissions ratio',  zlim=c(0,1)); lines(border)
  mtext(i, outer=TRUE)
  dev.off()
}

##### Read in data from your local data folder ########
#world borders
world <- readOGR("./TM_WORLD_BORDERS-0.3", "TM_WORLD_BORDERS-0.3")

## needed to correct metadata for nodatavalue in the distance map global.tif
## in the terminal, ran the code in the next line to set nodatavalue to -99 in metadata and write out new edited .tif
## $ gdal_translate -of GTIFF -a_nodata -99 "distance map global.tif" "distance map global-a_nodata.tif"
drainmap <- raster("./distance map global-a_nodata.tif")  # read in the new .tif
# spatial plastic waste generation from Lebreton and Andrady 2018 Datafile: data file https://figshare.com/s/843a17a4995b4c9e8af6
mismap <- raster("./LebretonAndrady2019_TotalPlasticWaste.tif")
          
# compare projections
proj4string(world) ; proj4string(mismap) ; proj4string(drainmap)
# rasters and shapefile are in the same coordinate system
#setwd("~/Dropbox/R/")
#dir.create('leak', showWarnings=FALSE) #create folder to store the check figures

## which countries
message("Starting Monte Carlo emissions ratio calculations by country..")
countries <- as.character(unique(world@data$NAME))
make_plots <- TRUE # whether to make plots
boot.data <- list()
set.seed(1)
t0 <- Sys.time()
n.boot <- 1000
## Start of loop through countries
i="Finland"
for(i in countries){## For each country, crop, then resample, then mask to extent.
  ind <- which(i == world@data$NAME)
  border <- world[which(world@data$NAME == i),]
  mismap_crop <- crop(mismap, extent(border))
  # if(mismap_crop@extent@ymax>62){
  #   message(paste("Skipping country because extent exceeds drainmap:",i))
  #   next
  # }
  drainmap_crop <- tryCatch(crop(drainmap, extent(border)),
                            error= function(e) 'error')
  drainmap_crop[drainmap_crop<0.001] <- 25
  if(is.character(drainmap_crop)){
    ## this fails when the country is not in drainmap
    message(paste("Skipping country because failed to crop drainmap:",i))
    next
  }
  mismap_rs <- raster::resample(mismap_crop, drainmap_crop, "bilinear")  # create resampled version with same extent
  mismap_mask <- mask(mismap_rs, border)  # masks the mismaptemp with the country border
  drainmap_mask <- mask(drainmap_crop, border)  # masks the drainmpatemp with the country border
  ## These are critical adjustments. The second b/c for some
  ## drainages at 100 (i.e., really far upstream, instead of
  ## being marked with a 100% chance of staying on land, it is
  ## marked as NA. This then messes up the subsequent
  ## calculations and plastic that should be on land is not, and
  ## thus by calculation must be in the ocean. So the emission
  ## ratio is biased high.
  drainmap_mask[drainmap_mask>100] <- 100
  drainmap_mask[is.na(drainmap_mask)] <- 100
  ## Start of calculations for emissions ratio
  total <- cellStats(mismap_mask, 'sum')
  land <- cellStats(mismap_mask*log(drainmap_mask+1,101), 'sum')
  if(total==0){
    message(paste('Skipping country because 0 plastic produced:' ,i))
    next
  }
  ## Monte Carlo simulation
#equation written as \sum(W_i) - U*\sum(W_i)log101(%drain+1). Note
  ## that for each value of U the other parts of the summation do not
  ## change. This means we only need to calculate the total waste
  ## (sum(W_i)) and the amount not going to oceans
  ## (\sum(W_i)log101(%drain+1) separately, and then apply the vector
  ## U to it.  Note: This assumes that the range of  the drainage is (0,100) ie nothing futher than 100km makes enters a waterbody
  ocean <- total-runif(n.boot,.9,1)*land
  emission.ratio <- ocean/total
  if(any(emission.ratio>1)){
    message(paste('Some emission ratios were >1 for country=',i))
  }
  boot.data[[i]] <- data.frame(iso3c= as.character(world@data$ISO3[ind]), Country=i, t(emission.ratio))
  if(make_plots) plot.leak.map()
}
results <- do.call(rbind, boot.data)
names(results)[-(1:2)] <- paste0('rep', 1:1000)
results[1,2:3]
## Antigua and Barbuda 0.7742788 0.7716815
Sys.time()-t0
message(paste(nrow(results), "countries processed out of", length(countries)))


## Quick plot of distribution of emission ratios by country
## across the Monte Carlo draws.
g <- gather(results, replicate, emission.ratio, -Country) %>%
  group_by(Country) %>% summarize(med=median(emission.ratio),
                                  lwr=min(emission.ratio),
                                  upr=max(emission.ratio)) %>%
   mutate(group=rep(1:3, len=n())) %>%
ggplot(aes(x=Country, y=med, ymin=lwr, ymax=upr)) + geom_point() +
  geom_linerange() +  ylim(0,1) + coord_flip() +
  facet_wrap('group', scales='free') + ylab("Emission ratio")
ggplot2::ggsave(here::here('r.outputs', 'leak', 'emissions_results.png'), g,
       width=15, height=7)

### Save results to file for use in other scripts
# create unique filename for each run
syst <- format(Sys.time(), "%Y-%m-%d") #this gets the timestamp
fname <- paste0('spatial_emission_ratios25_', syst, ".csv")
message("Saving emissions ratio results to data folder")
readr::write_csv(results, paste0("./inputs/", fname))

