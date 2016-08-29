########################################################################
## Philippines fisheries and aquaculture data analysis 
##
## Data obtained from the CountrySTAT Philippines website available 
## at http://countrystat.psa.gov.ph/?cont=10&pageid=1&ma=D10PNVMP
##
## By Tyler Clavelle
########################################################################

########################################################################
### Load packages and data ---------------------------------------------

## Packages
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(readxl)
library(broom)
library(readr)

## Data files
# Fishery and aquaculture data
phils <- read.csv(file = '../../Google Drive/Project Data/phils-data/phils_complete_processed.csv', stringsAsFactors = F)
phils_fish <- read.csv(file = '../../Google Drive/Project Data/phils-data/phils_fisheries_processed.csv', stringsAsFactors = F)
phils_aqua <- read.csv(file = '../../Google Drive/Project Data/phils-data/phils_aqua_processed.csv', stringsAsFactors = F)
phils_80to12 <- read.csv(file = '../../Google Drive/Project Data/phils-data/phils-prod-80-12/phils_prov_prod_80to12_1.csv', header = F, na.strings = c('.','..'), stringsAsFactors = F) %>%
  bind_rows(read.csv(file = '../../Google Drive/Project Data/phils-data/phils-prod-80-12/phils_prov_prod_80to12_2.csv', header = F, na.strings = c('.','..'), stringsAsFactors = F))

colnames(phils_80to12) <- c('province_name','sector', c(1980:2012))

phils_80to12 <- phils_80to12 %>%
  gather(year, harvest, 3:35) %>%
  mutate(year = as.numeric(year))

phils_80to12$province_name <- toupper(gsub(phils_80to12$province_name, pattern = '....', replacement = '', fixed = T))
phils_80to12$sector <- gsub(phils_80to12$sector, pattern = '..', replacement = '', fixed = T)

phils_80to12 <- phils_80to12 %>%
  spread(sector, harvest) %>%
  group_by(year) %>%
  mutate(all_fisheries = sum(`Commercial Fisheries`, `Municipal Fisheries`, na.rm = T)) %>%
  ungroup()

# Region socioeconomic data
region_data <- tbl_df(read.csv(file = '../../Google Drive/Project Data/phils-data/phils_fisheries_regions.csv', stringsAsFactors = F))

colnames(region_data) <- tolower(colnames(region_data))

########################################################################
### Prepare fishery and aquaculture data for regression ----------------

# function to prepare regression data at desired aggregation level
prep_regression <- function(phils_aqua_df = phils_aqua, phils_fish_df = phils_fish, region_data, grp_vars = 'region_name') {
  
  # phils_aqua_df = phils_aqua
  # phils_fish_df = phils_fish
  # grp_vars = 'province_name'
  
# aggregate all fishery and aquaculture data by region
phils_aqua_nat <- phils_aqua_df %>%
  rename_(grp_var = grp_vars) %>%
  rename(aqua_harvest  = harvest,
         aqua_price    = price,
         aqua_value_us = value_us,
         farm_type     = archetype) %>%
  group_by(grp_var, farm_type, year) %>%
  summarize(aqua_harvest  = sum(aqua_harvest, na.rm = T),
            aqua_value_us = sum(aqua_value_us, na.rm = T),
            aqua_price    = sum(aqua_value_us, na.rm = T) / sum(aqua_harvest, na.rm = T)) %>%
  ungroup()

## Assign variables by culture type 
farm_culture <- data_frame(farm_type    = unique(phils_aqua_nat$farm_type),
                           culture_type = c('Marine', 'Marine', 'Seaweed', 'Freshwater', 'Marine', 'Freshwater', 'Freshwater',
                                            'Mussel', 'Oyster', 'Freshwater', 'Freshwater', 'Marine', 'Marine'))
# Spread culture types out 
aqua_harvest <- phils_aqua_nat %>%
  left_join(farm_culture) %>%
  select(grp_var, culture_type, year, aqua_harvest) %>%
  group_by(grp_var, culture_type, year) %>%
  summarize(aqua_harvest = sum(aqua_harvest, na.rm = T)) %>%
  spread(culture_type, aqua_harvest) 

colnames(aqua_harvest) <- c('grp_var','year','freshwater_harvest','marine_harvest','mussel_harvest','oyster_harvest','seaweed_harvest')

aqua_price <- phils_aqua_nat %>%
  left_join(farm_culture) %>%
  select(grp_var, culture_type, year, aqua_value_us, aqua_harvest) %>%
  group_by(grp_var, culture_type, year) %>%
  summarize(aqua_price = sum(aqua_value_us, na.rm = T) / sum(aqua_harvest, na.rm = T)) %>%
  spread(culture_type, aqua_price) 

colnames(aqua_price) <- c('grp_var','year','freshwater_price','marine_price','mussel_price','oyster_price','seaweed_price')

phils_aqua_nat <- aqua_harvest %>%
  left_join(aqua_price)

phils_fish_nat <- phils_fish_df %>%
  rename_(grp_var = grp_vars) %>%
  rename(fish_harvest  = harvest,
         fish_price    = price,
         fish_value_us = value_us) %>%
  group_by(grp_var, species, isscaap_name, year) %>%
  summarize(fish_harvest  = sum(fish_harvest, na.rm = T),
            fish_value_us = sum(fish_value_us, na.rm = T),
            fish_price    = sum(fish_price, na.rm = T) / sum(fish_harvest, na.rm = T)) %>%
  ungroup()

# join national aggregates
phils_all_nat <- phils_fish_nat %>%
  left_join(phils_aqua_nat) %>%
  filter(is.na(fish_harvest)==F)

########################################################################
### Prepare socioeconomic data for inclusion in regression 

# Aggregate by region 
reg_1 <- region_data %>%
  tbl_df() %>%
  select(-x, -region_psgc, -province_psgc, -municipal_psgc, -psgc, -component.lgu, -income_class) %>%
  separate(income.class, into = c('income_class','drop'), sep = '[[:lower:]]') %>%
  rename_(grp_var = grp_vars) %>%
  group_by(grp_var) %>%
  summarize(length_coast     = sum(length_of_coastline_km, na.rm = T),
            no_barangays     = sum(no_of_barangays, na.rm = T),
            area_muni_waters = sum(area_of_municipal_waters_ha, na.rm = T),
            coastal_mgmt     = mean(coastal.marine.ecosystems.management, na.rm = T),
            enviro_avg       = mean(environ.average, na.rm = T),
            poverty          = mean(poverty_incidence, na.rm = T),
            gov_avg          = mean(governance.average, na.rm = T),
            income_level     = mean(as.numeric(income_class), na.rm = T)) %>%

  ungroup()

# Extract and spread population data into single variable
reg_pop <- region_data %>%
  rename_(grp_var = grp_vars) %>%
  select(grp_var, population_1980, population_1990, population_2000, population_2010) %>%
  gather(pop_var, population, 2:5) %>%
  group_by(grp_var, pop_var) %>%
  summarize(area_population = sum(population, na.rm = T)) %>%
  separate(pop_var, into = c('variable', 'years'), sep = '_') %>%
  mutate(years = as.numeric(years)) %>%
  complete(years = 1980:2013) %>%
  rename(year = years) %>%
  ungroup()

regs <- unique(reg_pop$grp_var) # unique regions

# Fill in missing years for population
for(a in 1:length(regs)) {
  reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year < 1990] <- reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year == 1980]
  reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year > 1990 & reg_pop$year < 2000 ] <- reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year == 1990]
  reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year > 2000 & reg_pop$year < 2010 ] <- reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year == 2000]
  reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year > 2010 ] <- reg_pop$area_population[reg_pop$grp_var == regs[a] & reg_pop$year == 2010]
}

# join datasets
phils_all_nat <- phils_all_nat %>%
  left_join(reg_pop) %>% 
  left_join(reg_1) %>%
  arrange(grp_var,species, isscaap_name, year )

return(phils_all_nat)
}

# create regression data
reg_level<-prep_regression(phils_aqua_df = phils_aqua, phils_fish_df = phils_fish, region_data, grp_var = 'region_name')
prov_level<-prep_regression(phils_aqua_df = phils_aqua, phils_fish_df = phils_fish, region_data, grp_var = 'province_name')

# save regression input data
# write.csv(prov_level, file = '../../../Box Sync/aqua-phils-data/philippines_regression_input.csv')

########################################################################
### Compile provincial level aggregated dataset  -----------------------

# Aggregate provincial level across species
agg_prov <- prov_level %>%
  rename(province_name = grp_var) %>%
  group_by(province_name, species, year) %>%
  summarize(fish_catch         = sum(fish_harvest, na.rm = T),
            fish_price_avg     = mean(fish_price, na.rm = T),
            marine_harvest     = sum(marine_harvest, na.rm = T),
            marine_price       = mean(marine_price, na.rm = T),
            oyster_harvest     = sum(oyster_harvest, na.rm = T),
            oyster_price       = mean(oyster_price, na.rm = T),
            mussel_harvest     = sum(mussel_harvest, na.rm = T),
            mussel_price       = mean(mussel_price, na.rm = T),
            freshwater_harvest = sum(freshwater_harvest, na.rm = T),
            freshwater_price   = mean(freshwater_price, na.rm = T),
            seaweed_harvest    = sum(seaweed_harvest, na.rm = T),
            seaweed_price      = mean(seaweed_price, na.rm = T)) %>%
  mutate(all_marine_harvest    = sum(marine_harvest + oyster_harvest + mussel_harvest, na.rm = T)) %>%
  ungroup()

agg_price <- agg_prov %>%
  select(province_name, species, year, fish_price_avg, marine_price, oyster_price, mussel_price, freshwater_price, seaweed_price) %>%
  gather(price_type, price, 4:9)

agg_prov <- agg_prov %>%
  select(province_name, species, year, fish_catch, marine_harvest, oyster_harvest, mussel_harvest, freshwater_harvest, seaweed_harvest) %>%
  gather(harvest_type, harvest, 5:9) %>% 
  left_join(agg_price)


# Assign aquaculture types farm styles
# farm_culture <- data_frame(archetype    = unique(phils_aqua$archetype),
#                            culture_type = c('Marine', 'Marine', 'Seaweed', 'Freshwater', 'Marine', 'Freshwater', 'Freshwater',
#                                             'Mussel', 'Oyster', 'Freshwater', 'Freshwater', 'Marine', 'Marine'))
# agg_prov2 <- phils_aqua %>%
#   left_join(farm_culture) %>%
#   group_by(province_name, year, culture_type) %>%
#   summarize(harvest = sum(harvest, na.rm = T)) %>%
#   spread(culture_type, harvest) %>%
#   right_join(phils_80to12)
# 
# # Join with 1980's data. The fish_catch variable contains the aggregated marine and commercial harvests
# agg_prov_all <- agg_prov2 %>%
#   left_join(agg_prov)



########################################################################
### Run regressions  ---------------------------------------------------

## Regression variables
# Model 1
m1_vars <- c('province_name','harvest_type', 'harvest', 'price_type', 'price')  
fmla_1 <- as.formula(paste("fish_catch ~ ", paste( m1_vars, collapse= "+"))) 
m1 <- glm(fmla_1, family = gaussian(link = 'log'), data = agg_prov)

summary(m1)






