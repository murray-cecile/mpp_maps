# Author: Sifan Liu
# Date: Mon Dec 10 13:26:02 2018
# --------------

# packages dependency
pkgs <- c("maps","ggthemes",'rgdal','tigris','mapproj')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
}

devtools::install_github("wmurphyrd/fiftystater")

# get cbsa locations from tigris
cbsa = core_based_statistical_areas()

# Read metro data ==================================================================================
# CHANGE THE FILES AND SPECIFIES VARIABLES HERE
data_metro = datafiles$PeerMetro_ai # whatever metro level data
data_metro$CBSAFP = as.character(data_metro$cbsa_code)
data_metro = left_join(data_metro, cbsa@data, by = c('CBSAFP'))

# select volume and intensity variables to map
data_metro$VOLUME = data_metro$emp16
data_metro$INTENSITY = data_metro$EmpAIShare06


# Without HI or AK ==========================================================================

usa <- map_data("state")

ggplot()+
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "#969696", color = "white") +
  coord_map("albers", lat0=39, lat1=45) +
  geom_point(data = data_metro, aes(x = long, y = lat, fill = INTENSITY, color = VOLUME), size = 6, shape = 21)+
  theme_map()

# Show HI and AK next to the main map ===========================================================

HI_AK <- c("46520", "27980","11260", "21820")

# correct for lat and lon coordinates
data_metro$long = -as.numeric(substring(data_metro$INTPTLON,2))
data_metro$lat = as.numeric(data_metro$INTPTLAT)

# transform  CBSAs locationsin HI and AK  
data_metro <- data_metro %>%
  mutate(long = case_when(
    CBSAFP == "46520" ~ -108.5,
    CBSAFP == "27980" ~ -104.5,
    CBSAFP == "11260" ~ -117.5,
    CBSAFP == "21820" ~ -117,
    TRUE ~ .$long),
    lat = case_when(
      CBSAFP == "46520" ~ 27,
      CBSAFP == "27980" ~ 24.5,
      CBSAFP == "11260" ~ 27,
      CBSAFP == "21820" ~ 27.5,
      TRUE ~ .$lat
    )
  )

# MAPPING ==================================================================================

# map
ggplot() + 
  geom_map(data = fifty_states, map = fifty_states, aes(map_id = id),
           fill = "light grey", color = "white") + 
  geom_point(data = data_metro, aes(x = long, y = lat,  color = INTENSITY, size = VOLUME)) +
  coord_map("albers", lat0=39, lat1=45) +
  theme_map()

