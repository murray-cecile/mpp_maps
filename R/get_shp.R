##==================================================================================##
# BROOKINGS METRO MAPS FUNCTIONS
#  Streamline map-making
#
# Cecile Murray
# January 2018
##==================================================================================##

homedir <- "C:/Documents/mpp_maps"
setwd(homedir)

libs <- c("tidyverse", "magrittr", "stringr", "readr", "openxlsx", "janitor", "sp",
          "maptools", "tigris", "censusapi", "broom", "ggmap", "foreign", "readstata13",
          "plotly")
lapply(libs, library, character.only=TRUE)

setwd("V:/Metro Poverty/CMM/scripts")
source("utils.R")
source("api_keychain.R")
setwd(homedir)

census_key <- Sys.getenv("CENSUSAPI_KEY")

#============================================================#
# SHAPEFILES
#============================================================#

states.shp <- states()
states.df <- tidy(states.shp, region = "GEOID") 
lower48.df <- filter(states.df, as.numeric(id) < 57, !id %in% c("02", "15"))

counties.shp <- counties()
counties.df <- tidy(counties.shp, region = "GEOID")

cbsa.shp <- core_based_statistical_areas()
cbsa.pts <- cbsa.shp@data %>% select(GEOID, NAMELSAD, INTPTLAT, INTPTLON) %>%
  separate(NAMELSAD, c("NAME", "name2"), sep = ", ") %>%
  filter(!name2 %in% c("PR Metro Area", "AK Metro Area", "AK Micro Area",
                       "HI Metro Area", "HI Micro Area")) %>%
  unite("NAME", NAME, name2, sep = ", ") %>%
  dplyr::rename(cbsa = GEOID) %>%
  mutate(lat = as.numeric(INTPTLAT), long = as.numeric(INTPTLON)) %>%
  select(-INTPTLAT, -INTPTLON)

#============================================================#
# DATA TO MAP
#============================================================#

data <- getCensus(name = "acs/acs1", vintage = "2016", key = census_key,
                  vars = c("B01001_001E", "B17001_001E", "B17001_002E"), 
                  region = "metropolitan statistical area/micropolitan statistical area:*") %>%
  dplyr::rename(cbsa = metropolitan.statistical.area.micropolitan.statistical.area) %>%
  replace_api_nulls() %>%
  mutate(povrate = B17001_002E / B17001_001E)

names(data)[2:5] <- c("pop", "povuniv", "poor", "povrate")

mapdata <- merge(data, cbsa.pts, by = "cbsa") %>%
  mutate(povcat = ntile(povrate, 4))

#============================================================#
# THE MAP
#============================================================#

lower48 <- ggplot() +
  geom_polygon(data = lower48.df, aes(x=long, y=lat, group=group),
               fill = "gray60", color = "white") +
  geom_point(data = mapdata, aes(x=long, y=lat, size = pop, color = povcat)) +
  coord_map("albers", parameters = c(20, 40)) +
  theme_minimal()

ggplotly(lower48)

ggplot() +
  geom_map(aes(map_id = id), map = fifty_states, color = "gray60") +
  geom_point(data = mapdata, aes(x=long, y=lat, size = pop, color = povcat)) +
  coord_map("albers", parameters = c(20, 60))

st_dummy <- data.frame(state = unique(fifty_states$id),
                       dummy = rep(1, length(unique(fifty_states$id))))

ggplot() +
  geom_map(data = st_dummy, aes(map_id = state), map = fifty_states, color = "gray30") +
  geom_point(data = mapdata, aes(x=long, y=lat, size = pop, color = povcat)) +
  coord_map("albers", parameters = c(20, 60)) +
  theme_void()
