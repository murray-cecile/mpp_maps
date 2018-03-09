##==================================================================================##
# MPP MAPS: SETUP
#  Set working directories, globals, etc.
#
# Cecile Murray
# March 2018
##==================================================================================##

# packages
libs <- c("magrittr", "stringr", "readr", "openxlsx", "janitor", "sp",
          "maptools", "tigris", "censusapi", "broom", "ggmap", "foreign", "readstata13",
          "plotly", "tidyverse")
lapply(libs, library, character.only=TRUE)

#============================================================#
# GLOBAL FILEPATHS
#============================================================#

scripts_dir <- "V:/Metro Poverty/CMM/scripts"

xwalk_dir <- "V:/Metro Poverty/CMM/xwalks"

tigris_path <- "V:/Metro Poverty/CMM/data/shp"

#============================================================#
# SETUP
#============================================================#

options(tigris_use_cache=TRUE)
tigris_cache_dir(tigris_path)
readRenviron('~/.Renviron')
