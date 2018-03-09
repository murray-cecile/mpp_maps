##==================================================================================##
# MPP MAPS: TEST STATIC MAP FUNCTONS
#  Testing static map functions
#
# Cecile Murray
# March 2018
##==================================================================================##

setwd(scripts_dir)
source("api_keychain.R")
setwd(here())

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
