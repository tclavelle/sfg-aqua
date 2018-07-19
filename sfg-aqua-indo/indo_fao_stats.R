##################################################
## Project: CI Indonesia Aquaculture Guidelines
## Script purpose: Summary of Indo aquaculture and fishery stats
## Date: 06/22/2018
## Author: Tyler Clavelle
##################################################

library(tidyverse)

# Read latest FAO data
fao_aqua <- read_csv(file = '../../Google Drive/Project Data/fao-data/20-02-2017_fao_aquaculture_production.csv')
fao_fish <- read_csv(file = '../../Google Drive/Project Data/fao-data/20-02-2017_fao_capture.csv')

# Filter to Indonesia
indo_fish <- fao_fish %>% 
  filter(`Country (Country)` == 'Indonesia') %>% 
  filter(`Fishing area (Inland/Marine areas)` == 'Marine areas') %>% 
  select(`Species (ASFIS species)`, `Species (ISSCAAP group)`, `2014`) %>% 
  mutate(`2014` = as.numeric(`2014`)) %>% 
  arrange(desc(`2014`))

# Calculate totals by species group
indo_fish %>% 
  group_by(`Species (ISSCAAP group)`) %>% 
  summarize(catch = sum(`2014`, na.rm = T)) %>% 
  arrange(desc(catch)) %>% 
  ungroup() %>% 
  mutate(perc = 100 * catch / sum(catch, na.rm = T))
