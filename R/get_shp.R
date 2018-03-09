##==================================================================================##
# MPP MAPS FUNCTIONS
#  Streamline map-making
#
# Cecile Murray
# January 2018
##==================================================================================##

library(here)
source(here("R", "setup.R"))

#============================================================#
# SHAPEFILES
#============================================================#

states.shp <- states()
states.df <- tidy(states.shp, region = "GEOID") 
lower48.df <- filter(states.df, as.numeric(id) < 57, !id %in% c("02", "15"))

counties.shp <- counties()
counties.df <- tidy(counties.shp, region = "GEOID")

cbsa.shp <- core_based_statistical_areas()
cbsa.pts <- cbsa.shp@data %>% dplyr::select(GEOID, NAMELSAD, INTPTLAT, INTPTLON) %>%
  separate(NAMELSAD, c("NAME", "name2"), sep = ", ") %>%
  filter(!name2 %in% c("PR Metro Area", "AK Metro Area", "AK Micro Area",
                       "HI Metro Area", "HI Micro Area")) %>%
  unite("NAME", NAME, name2, sep = ", ") %>%
  dplyr::rename(cbsa = GEOID) %>%
  mutate(lat = as.numeric(INTPTLAT), long = as.numeric(INTPTLON)) %>%
  dplyr::select(-INTPTLAT, -INTPTLON)


