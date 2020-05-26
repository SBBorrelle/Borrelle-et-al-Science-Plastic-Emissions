# This code sets up the dependencies and source functions 
# call dependencies
#specify the packages needed for all analysis and outputs - 
suppressPackageStartupMessages({
    require(here)
    require(tidyverse)
    require(scales)
    require(data.table)
    require(rgeos)
    require(sp)
    require(parallel)
    require(matrixStats)
    require(tibble)
    require(SOAR)
    require(janitor)
    require(raster)
    require(rgdal)
    require(reshape2)
    require(viridis)
    require(gridExtra)
    require(cowplot)
    require(kableExtra)
  require(egg)
  require(ggpubr)
  require(magrittr)
  require(gtrendsR)
  require(gifski)
  require(gganimate)
  require(ggimage)
  require(lubridate)
  require(usethis)
})

#source functions
source(here::here("r.Scripts", "PPEG_theme.R"))

#Outputs 
st=format(Sys.time(), "%Y-%m-%d") #this gets the timestamp and adds to labels on figures / csvs
PF_pal = viridis::viridis_pal(alpha=1)(12)
#Plotting palettes 
bau <- "#FDE725FF"
ambitious <- "#25858EFF"
target <- "#482173FF"
line8mt <- "#E89242FF"

##-------------------------------------------
#set symlink to data folder -with data files > 50mb

# ONLY DO ONCE
# file.symlink(
#   from = "~/filepath/to/your/data/folder", #actual path to dropbox or server
#   to = 'data_sym'
#   )
