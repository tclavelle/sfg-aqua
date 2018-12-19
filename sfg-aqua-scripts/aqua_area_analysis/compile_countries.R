##################################################
## Project: Global Aquaculture Area
## Script purpose: Prelim concept analysis
## Date: 11/14/2018
## Author: Tyler Clavelle
##################################################


# Packages and Data -------------------------------------------------------

library(tidyverse)
library(here)

### FAO Data
fao <- read_csv(here('sfg-aqua-data/fao', '20-02-2017_fao_aquaculture_production.csv'), na = c('...', '-', '0 0'))

### National Datasets
# China 
china <- read_csv(here('sfg-aqua-data/china-data', 'china_area_cleaned.csv'))
# Indonesia 
indo <- read_csv(here('sfg-aqua-data/indo-data', 'indo_aqua_processed.csv'))
# Vietnam
nam <- read_delim(here('sfg-aqua-data/vietnam-data', 'vietnam_aquatype_area_.csv'), delim = ';', skip = 2, na = '..')
# Bangladesh
bangla <- read_csv(here('sfg-aqua-data/bangladesh-data', 'bangladesh_aqua_area.csv'))
# Korea
korea <- read_csv(here('sfg-aqua-data/korea-data', 'korea_aqua_area.csv'))

# FAO aquatic plant groups
seaweeds <- c('Red seaweeds', 'Brown seaweeds', 'Green seaweeds', 'Miscellaneous aquatic plants')

# Process FAO -------------------------------------------------------------

# Convert to long format
fao <- fao %>% 
  gather(year, production, `1950`:ncol(.)) %>% 
  select(`Country (Country)`, `Species (ASFIS species)`, `Species (ISSCAAP group)`, 
         `Aquaculture area (Inland/Marine areas)`, `Environment (Environment)`, year, production) %>%
  rename(country     = `Country (Country)`,
         comm_name   = `Species (ASFIS species)`,
         isscaap     = `Species (ISSCAAP group)`,
         area        = `Aquaculture area (Inland/Marine areas)`,
         environment = `Environment (Environment)`) %>% 
  mutate(production = as.numeric(gsub(production, pattern = ' F', replacement = '')),
         year = as.numeric(year),
         food_cat = ifelse(isscaap %in% seaweeds, 'Aquatic plants', 'Food fish'))

# FAO data summaries
# All aquaculture by country in 2014
cntry_total <- fao %>% 
  filter(year == max(year)) %>% 
  group_by(country) %>% 
  summarize(total = sum(production, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  ungroup() %>% 
  mutate(perc      = round(total / sum(total) * 100, digits = 1),
         cum_total = round(cumsum(total), digits = 1),
         cum_perc  = round(cum_total / sum(total) * 100, digits = 1))

# Marine and brackish water aquaculture by country in 2014
marine_cntry_total <- fao %>% 
  filter(year == max(year),
         area == 'Marine areas') %>% 
  group_by(country) %>% 
  summarize(total = sum(production, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  ungroup() %>% 
  mutate(perc      = round(total / sum(total) * 100, digits = 1), 
         cum_total = round(cumsum(total), digits = 1),
         cum_perc  = round(cum_total / sum(total) * 100, digits = 1))


# Compile National Datasets --------------------------------------------------------

# Isolate just total mariculture area at the national level for each country
global_total <- china %>% # china
  group_by(year) %>% 
  summarize(total_area = sum(total_area, na.rm = T)) %>%
  mutate(country = 'China') %>% 
  bind_rows(indo %>% # indonesia
              filter(province == 'Indonesia',
                     !aquaculture_type %in% c('total', 'freshwater pond', 'paddy field')) %>% 
              group_by(year) %>% 
              summarize(total_area = sum(hectares, na.rm = T)) %>% 
              mutate(country = 'Indonesia')) %>% 
  bind_rows(nam %>% # vietnam (brackish area + freshwater area + area for breeding)
              gather(key = 'year', value = 'total_area', 2:ncol(nam)) %>% 
              filter(`Area of water surface` %in% c('Area of marine aquaculture', 'Brackish water:')) %>% 
              filter(year != 'Prel. 2017') %>% 
              select(year, total_area) %>% 
              mutate(year = as.numeric(year),
                     total_area = total_area * 1000) %>% 
              mutate(country = 'Vietnam')) %>% 
  bind_rows(bangla %>% # bangladesh
              filter(aqua_type %in% c('shrimp farm', 'pen', 'cage')) %>%
              rename(total_area = hectares) %>%
              mutate(country = 'Bangladesh') %>%
              select(country, year, total_area)) %>%
  bind_rows(korea %>% 
              filter(aqua_type == 'cage') %>% 
              mutate(total_area = `1000_m2`*10) %>% 
              mutate(country = 'Korea') %>% 
              select(country, year, total_area))
  