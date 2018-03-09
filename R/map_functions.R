##==================================================================================##
# MPP MAPS: FUNCTIONS FOR STATIC MAPPING
#  Functions to undergird metro maps  
#
# Cecile Murray
# March 2018
##==================================================================================##

# Map wrapper function:
#   takes a dataframe, 
#   merges with metro point shapefile
#   maps the indicated variables for bubble size and color
make_metro_map <- function(df, b_size, b_color, save = FALSE,
                           cbsas = cbsa.pts, states = states.df){
  
  # merge with cbsa point file
  mapdf <- merge(df, cbsas, by = "cbsa")
  
  # map the variables
  ggplot(mapdf) +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill =  "grey60", color = white) +
    geom_polygon(aes(x = long, y=lat, group = group, 
                     size = !!b_size, color = !!b_color))
  
  
}