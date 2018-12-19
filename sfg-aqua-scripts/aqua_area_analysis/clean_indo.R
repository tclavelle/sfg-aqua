##################################################
## Project: Aquaculture area analysis
## Script purpose: Process and visualize Indonesia data
## Date: 11/08/2018
## Author: Tyler Clavelle
##################################################


# Packages and Data -------------------------------------------------------

# Packages
library(tidyverse)
library(scales)

# Production and area data
aqua_area <- read.csv(file = "sfg-aqua-data/indo-data/indo_aqua_area_FIXED.csv", na = c("-"), colClasses = 'character')
aqua_prod <- read.csv(file = "sfg-aqua-data/indo-data/indo_aqua_production_FIXED.csv", na = c("-"), colClasses = 'character')

# Process data into long format
aqua <- aqua_prod %>%
  gather(key = 'year', value = 'value', 4:ncol(.)) %>%
  bind_rows(aqua_area %>%
              gather(key = 'year', value = 'value', 4:ncol(.))) %>% 
  spread(units, value) %>% 
  mutate(hectares = as.numeric(gsub('\\.', '', hectares)),
         ton          = as.numeric(gsub('\\.', '', ton)),
         tons_per_ha = ton / hectares,
         year        = gsub('X', '', year),
         year          = as.numeric(year))

# Indonesia totals
indo_total <- aqua %>% 
  filter(province == 'Indonesia')

# Plots -------------------------------------------------------------------

indo_total %>% 
  group_by(year) %>% 
  summarize(area = sum(hectares, na.rm = T)) %>% 
  filter(area > 0) %>% 
  ggplot(aes(x = year, y = area)) +
  geom_line()


# Save Data ---------------------------------------------------------------

write_csv(aqua, path = 'sfg-aqua-data/indo-data/indo_aqua_processed.csv')
