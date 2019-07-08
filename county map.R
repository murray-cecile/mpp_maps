# Author: Sifan Liu
# Date: Mon Jul 16 16:28:48 2018
# --------------
pkgs <- c('tidyverse','urbnmapr')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

library('tidyverse')
library('urbnmapr')
# urban ggplot2 theme
# require insall Ghostscript
# source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_theme_windows.R')

padz <- function(x, n=max(nchar(x))) gsub(" ", "0", formatC(x, width=n)) 

counties %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

tariff <- read.csv("V:/Sifan/mpp_maps/county_by_partner_map.csv") %>% 
  select(c_fips_lgc_, share_ret_xn) %>%
  filter(!is.na(c_fips_lgc_))
BEAFIPS <- read.csv('V:/Sifan/R/xwalk/FIPSModifications.csv', colClasses = 'character')

# Fix : 
# Shannon County, SD (FIPS code=46113) was renamed Oglala Lakota County and assigned a new FIPS code (46102).
BEAFIPS[nrow(BEAFIPS)+1,] <- c("46113","Shannon County", "46102", "Oglala Lakota County")

tariff <- tariff %>%
  mutate(county_fips = padz(c_fips_lgc_,5)) %>%
  left_join(BEAFIPS, by = c("county_fips"="BEA.FIPS")) %>%
  mutate(county_fips = ifelse(is.na(FIPS),county_fips,FIPS)) %>%
  mutate(share = as.numeric(gsub("%","",share_ret_xn))) 

tariff$share_cut = cut(tariff$share,
                         breaks = c(-Inf, 3, 6, 12, 20, Inf),
                         labels = c("0 - 3 %", "3 - 6 %", "6 - 12 %", "12 - 20 %", "20 % and above"))

tariff_map <- left_join(tariff, counties, by = "county_fips")

# continuous scale

tariff_map %>%
  ggplot(aes(long, y = lat, group = group, fill = share)) +
  geom_polygon(color = "grey", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  scale_fill_distiller(palette = 'Oranges', direction = 1 ) +
  labs(x=NULL, y=NULL) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())

# categorical scale

tariff_map %>%
  ggplot(aes(long, y = lat, group = group, fill = share_cut)) +
  geom_polygon(color = "grey", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  scale_fill_brewer(name = "Share of Exports in Tariff Affected \nIndustries, by County", 
                    palette = 'Oranges', direction = 1 ) +
  labs(x=NULL, y=NULL,face = "italic",
       caption = "Source: Brookings analysis of data from Census, BEA, BLS, IRS, EIA, Eurostat, Moody's Analytics, NAFSA, PIIE, Sabre, and Trade Map, International Trade Centre, www.intracen.org/marketanalysis") + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())

ggsave('V:/Sifan/Share of Targeted Exports.pdf', width = 12, height = 10)

