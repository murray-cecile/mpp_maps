##==================================================================================##
# METRO MAPS: MAKE BASE 50-STATE MAPFILE
#  Based on code from fiftystateR package by Will Murphy;
#   modified to replace with a different shapefile
#
# Cecile Murray
# December 2017
##==================================================================================##

homedir <- "V:/Metro Poverty/CMM/sandbox/mpp_maps"
setwd(homedir)

libs <- c("tidyverse", "magrittr", "sp", "maptools", "tigris", "censusapi",
          "broom", "ggmap", "foreign", "readstata13", "rgdal", "fiftystater")
lapply(libs, library, character.only=TRUE)


#============================================================#
# CODE
#============================================================#

transform_state <- function(object, rot, scale, shift){
  object %>% elide(rotate = rot) %>%
    elide(scale = max(apply(bbox(object), 1, diff)) / scale) %>%
    elide(shift = shift)
}

#state shape file from
# http://www.arcgis.com/home/item.html?id=f7f805eb65eb4ab787a0a3e1116ca7e5
# loc <- file.path(tempdir(), "stats_dat")
# unzip("inst/extdata/states_21basic.zip", exdir = loc)
# fifty_states_sp <- readOGR(dsn = loc, layer = "states", verbose = FALSE) %>%
#   spTransform(CRS("+init=epsg:2163"))

# replacing state shape file with Census TIGER file

states.shp <- states()

alaska <- fifty_states_sp[fifty_states_sp$STATE_NAME == "Alaska", ] %>%
  transform_state(-35, 2.5, c(-2400000, -2100000))
proj4string(alaska) <- proj4string(fifty_states_sp)

hawaii <- fifty_states_sp[fifty_states_sp$STATE_NAME == "Hawaii", ] %>%
  transform_state(-35, .75, c(-1170000,-2363000))
proj4string(hawaii) <- proj4string(fifty_states_sp)

fifty_states <-
  fifty_states_sp[!fifty_states_sp$STATE_NAME %in% c("Alaska","Hawaii"), ] %>%
  rbind(alaska) %>%
  rbind(hawaii) %>%
  spTransform(CRS("+init=epsg:4326")) %>%
  fortify(region = "STATE_NAME") %>%
  mutate(id = tolower(id))

devtools::use_data(fifty_states, overwrite = TRUE)

get_box <- function(id) {
  fifty_states[fifty_states$id == id, c("long", "lat")] %>%
    as.matrix %>%
    bbox %>%
    t %>%
    # expand slightly to leave inner margin
    apply(2, `+`, c(-.5, .5)) %>%
    # rearrange corner coordinates into path coordinates
    `[`(c(1, 2, 2, 1, 1, 3, 3, 4, 4, 3)) %>%
    matrix(ncol = 2) %>%
    as.data.frame %>%
    setNames(c("x", "y")) %>%
    cbind(data.frame(id = id, stringsAsFactors = FALSE))
}

fifty_states_inset_boxes_data <- c("alaska", "hawaii") %>%
  lapply(get_box) %>%
  bind_rows

devtools::use_data(fifty_states_inset_boxes_data, overwrite = TRUE)