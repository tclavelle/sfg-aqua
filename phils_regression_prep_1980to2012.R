########################################################################
### Load packages and data ---------------------------------------------

## Packages
library(tidyverse)
library(readxl)
library(broom)
library(corrplot)
library(knitr)
library(stargazer)

## Data files
# Fishery and aquaculture production data
phils_prod <- read.csv(file = '../../Google Drive/Project Data/phils-data/phils-prod-80-12/phils_prov_prod_80to12_1.csv', header = F, na.strings = c('.','..'), stringsAsFactors = F) %>%
  bind_rows(read.csv(file = '../../Google Drive/Project Data/phils-data/phils-prod-80-12/phils_prov_prod_80to12_2.csv', header = F, na.strings = c('.','..'), stringsAsFactors = F))

colnames(phils_prod) <- c('province_name','sector', c(1980:2012))

# Fishery and aquaculture value data
phils_val <- read.csv(file = '../../Google Drive/Project Data/phils-data/phils-prod-80-12/phils_prov_10to14_value_1.csv', header = F, na.strings = c('.','..'), stringsAsFactors = F) %>%
  bind_rows(read.csv(file = '../../Google Drive/Project Data/phils-data/phils-prod-80-12/phils_prov_10to14_value_2.csv', header = F, na.strings = c('.','..'), stringsAsFactors = F))

colnames(phils_val) <- c('province_name','sector', c(1980:2014))

# phils aquaculture data 1996 to 2014
phils_aqua <- read.csv(file = '../../Google Drive/Project Data/phils-data/phils_aqua_processed.csv', stringsAsFactors = F)

# Region socioeconomic data
region_data <- tbl_df(read.csv(file = '../../Google Drive/Project Data/phils-data/phils_fisheries_regions.csv', stringsAsFactors = F))

colnames(region_data) <- tolower(colnames(region_data))

## Process municipal fishery and aquaculture data into single regression data file. Not broken out by species
phils_prod <- phils_prod %>%
  gather(year, harvest, 3:35) %>%
  mutate(year = as.numeric(year)) %>%
  spread(sector, harvest)

phils_val <- phils_val %>%
  select(-`2013`, -`2014`) %>%
  gather(year, value, 3:35) %>%
  mutate(year = as.numeric(year)) %>%
  spread(sector, value)

phils <- left_join(phils_prod, phils_val, by = c("province_name", 'year'))

phils$province_name <- toupper(gsub(phils$province_name, pattern = '....', replacement = '', fixed = T))

phils <- phils %>%
  rename(muni_marine_catch = `....Marine Municipal Fisheries.x`,
         aqua_harvest_all  = `..Aquaculture.x`, # indicate this is the volume of all aquaculture aggregated
         comm_catch        = `..Commercial Fisheries.x`,
         muni_catch        = `..Municipal Fisheries.x`,
         muni_marine_value = `....Marine Municipal Fisheries.y`,
         aqua_value        = `..Aquaculture.y`,
         comm_value        = `..Commercial Fisheries.y`,
         muni_value        = `..Municipal Fisheries.y`) %>%
  group_by(province_name, year) %>%
  mutate(all_catch      = sum(muni_marine_catch + comm_catch, na.rm = T),
         all_value      = sum(muni_marine_value + comm_value, na.rm = T),
         muni_price     = muni_marine_value / muni_marine_catch,
         aqua_price_all = aqua_value / aqua_harvest_all, # indicate that this is the aggregate aquaculture price 
         comm_price     = comm_value / comm_catch,
         all_price      = all_value / all_catch)


# aggregate all fishery and aquaculture data (1996-2015) by region
phils_aqua_df <- phils_aqua %>%
  rename(aqua_harvest  = harvest,
         aqua_price    = price,
         aqua_value_us = value_us,
         farm_type     = archetype) %>%
  group_by(province_name, farm_type, year) %>%
  summarize(aqua_harvest  = sum(aqua_harvest, na.rm = T),
            aqua_value_us = sum(aqua_value_us, na.rm = T),
            aqua_price    = sum(aqua_value_us, na.rm = T) / sum(aqua_harvest, na.rm = T)) %>%
  ungroup()

## Assign variables by culture type 
farm_culture <- data_frame(farm_type    = unique(phils_aqua_df$farm_type),
                           culture_type = c('Marine', 'Marine', 'Seaweed', 'Freshwater', 'Marine', 'Freshwater', 'Freshwater',
                                            'Mussel', 'Oyster', 'Freshwater', 'Freshwater', 'Marine', 'Marine'))
# Spread culture types out 
aqua_harvest <- phils_aqua_df %>%
  left_join(farm_culture) %>%
  select(province_name, culture_type, year, aqua_harvest) %>%
  group_by(province_name, culture_type, year) %>%
  summarize(aqua_harvest = sum(aqua_harvest, na.rm = T)) %>%
  ungroup()

#colnames(aqua_harvest) <- c('province_name','year','freshwater_harvest','marine_harvest','mussel_harvest','oyster_harvest','seaweed_harvest')

aqua_price <- phils_aqua_df %>%
  left_join(farm_culture) %>%
  select(province_name, culture_type, year, aqua_value_us, aqua_harvest) %>%
  group_by(province_name, culture_type, year) %>%
  summarize(aqua_value_us = mean(aqua_value_us, na.rm = T),
    aqua_price = sum(aqua_value_us, na.rm = T) / sum(aqua_harvest, na.rm = T)) %>%
  ungroup()

#colnames(aqua_price) <- c('province_name','year','freshwater_price','marine_price','mussel_price','oyster_price','seaweed_price')

phils_aqua_df <- aqua_harvest %>%
  left_join(aqua_price) #%>%
  # filter(culture_type != 'Freshwater')


########################################################################
### Prepare socioeconomic data for inclusion in regression 

# Aggregate by region
reg_1 <- region_data %>%
  tbl_df() %>%
  select(-x, -region_psgc, -province_psgc, -municipal_psgc, -psgc, -component.lgu, -income_class) %>%
  separate(income.class, into = c('income_class','drop'), sep = '[[:lower:]]') %>%
  mutate(income_class = as.numeric(income_class)) %>%
  group_by(province_name) %>%
  summarize(length_coast     = sum(length_of_coastline_km, na.rm = T),
            no_barangays     = sum(no_of_barangays, na.rm = T),
            area_muni_waters = sum(area_of_municipal_waters_ha, na.rm = T),
            income_class     = mean(income_class, na.rm = T),
            mangroves        = sum(mangroves, na.rm = T),
            coral            = sum(coralreefs, na.rm = T)) %>%

  ungroup()

# Extract and spread population data into single variable
reg_pop <- region_data %>%
  select(province_name, population_1980, population_1990, population_2000, population_2010) %>%
  gather(pop_var, population, 2:5) %>%
  group_by(province_name, pop_var) %>%
  summarize(area_population = sum(population, na.rm = T)) %>%
  separate(pop_var, into = c('variable', 'years'), sep = '_') %>%
  mutate(years = as.numeric(years)) %>%
  complete(years = 1980:2013) %>%
  rename(year = years) %>%
  ungroup()

regs <- unique(reg_pop$province_name) # unique regions

# Fill in missing years for population
for(a in 1:length(regs)) {
  reg_pop$area_population[reg_pop$province_name == regs[a] & reg_pop$year < 1990] <- reg_pop$area_population[reg_pop$province_name == regs[a] & reg_pop$year == 1980]
  reg_pop$area_population[reg_pop$province_name == regs[a] & reg_pop$year > 1990 & reg_pop$year < 2000 ] <- reg_pop$area_population[reg_pop$province_name == regs[a] & reg_pop$year == 1990]
  reg_pop$area_population[reg_pop$province_name == regs[a] & reg_pop$year > 2000 & reg_pop$year < 2010 ] <- reg_pop$area_population[reg_pop$province_name == regs[a] & reg_pop$year == 2000]
  reg_pop$area_population[reg_pop$province_name == regs[a] & reg_pop$year > 2010 ] <- reg_pop$area_population[reg_pop$province_name == regs[a] & reg_pop$year == 2010]
}

phils <- phils %>%
  left_join(reg_1) %>%
  left_join(reg_pop) %>%
  select(-variable)

# Subset to only municipalities with non-zero area of waters
phils_reg <- phils %>%
  ungroup() %>%
  filter(area_muni_waters > 0) %>%
  filter(!(province_name == 'EASTERN SAMAR' & year == 1995))

phils_reg_2 <- phils_reg %>%
  left_join(phils_aqua_df, by = c("province_name", "year"))

########################################################################
### Create difference in differences dummy variables

prov_aqua_summary <- phils_reg_2 %>%
  filter(is.na(culture_type) ==F) %>%
  group_by(province_name, year, culture_type) %>%
  summarize(aqua_harvest = sum(aqua_harvest, na.rm = T)) %>%
  group_by(province_name, culture_type) %>%
  mutate(treated = aqua_harvest > 0,
         time    = min(year[treated == TRUE])) %>%
  summarize(treated = sum(treated, na.rm = T),
            treat_year    = min(time, na.rm = T),
            harvest_2012 = mean(aqua_harvest, na.rm = T))

prov_aqua_summary <- prov_aqua_summary %>%
  ungroup() %>%
  complete(province_name, culture_type, fill = list(treated = 0, treat_year = 0))

treatments <- subset(prov_aqua_summary, culture_type == 'Marine') %>%
  filter(is.na(harvest_2012) == F) %>%
  filter(treated == 0 | treat_year > 1996) %>%
  select(-harvest_2012)

treatments$treated[treatments$treated > 0] <- 1


diff_test <- phils_reg_2 %>%
  filter(province_name %in% c(treatments$province_name) & culture_type == 'Marine') %>%
  filter(is.na(muni_marine_catch) ==F) %>%
  left_join(treatments)  %>%
  mutate(time = ifelse(year >= treat_year, 1, 0),
         did = treated * time)

test <- lm(log(muni_marine_catch) ~ did, data = diff_test)

########################################################################
### Plot timeseries by province

# Plot timeseries of aquaculture and fishery production for each province
ps <- unique(phils_reg_2$province_name)
pdf(file = 'phils_province_plots.pdf')
for(b in 1:length(ps)) {
  
tmp1 <- phils_reg_2 %>%
  filter(province_name == ps[b]) %>%
  group_by(province_name, year) %>%
  summarize(muni_catch  = sum(muni_marine_catch, na.rm = T),
            comm_catch  = sum(comm_catch, na.rm = T),
            # all_catch   = sum(all_catch, na.rm = T),
            aqua        = sum(aqua_harvest_all, na.rm = T))

tmp2 <- phils_reg_2 %>%
  filter(province_name == ps[b] & is.na(culture_type)==F & culture_type != 'Freshwater') %>%
  group_by(province_name, year, culture_type) %>%
  summarize(aqua_harvest = sum(aqua_harvest, na.rm = T))
  
# aqua_start2 <- min(tmp2$year[tmp2$aqua_harvest > 0 & is.na(tmp2$year) == F])
starts <- sapply(X = unique(tmp2$culture_type), FUN = function(x) {
  min(tmp2$year[tmp2$aqua_harvest > 0 & tmp2$culture_type == x])
})

starts <- data_frame(culture_type = names(starts),
                     startyr      = starts)

print(  
  tmp1 %>%
    gather(key = 'Sector', value = 'Production', 3:5) %>%
    ggplot(aes(x = year, y = Production)) +
    geom_line(aes(linetype = Sector)) +
    scale_y_log10() +
    geom_vline(xintercept = 1996, color = 'black', linetype = 2) +
    geom_line(data = tmp2, aes(x = year, y = aqua_harvest, color = culture_type)) +
    geom_vline(data = starts, aes(xintercept = startyr, color = culture_type), linetype = 2) +
    labs(title = unique(tmp1$province_name))
  )
}
dev.off()
########################################################################
### Save prepped regression data file

write_csv(phils_reg_2, path = 'sfg-aqua-data/phils_regression_df_1980to2012.csv')
