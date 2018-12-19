##################################################
## Project: Aquaculture area
## Script purpose: Wrangle national aquaculture area datasets
## Date: 11/08/2018
## Author: Tyler Clavelle
##################################################

# Libraries
library(tidyverse)

# Data --------------------------------------------------------------------

### China

# empty list to store dataframe of each worksheet
china1 <- list() 
china2 <- list()

# Get sheet names
china_sheets1 <- excel_sheets('sfg-aqua-data/china-data/mari_area_fish_province.xlsx')
china_sheets2 <- excel_sheets('sfg-aqua-data/china-data/mariculture_fish_province.xlsx')

# Loop over sheets and read in data
for(s in seq_along(china_sheets1)) {
  china1[[s]] <- read_excel(path = 'sfg-aqua-data/china-data/mari_area_fish_province.xlsx', sheet = china_sheets1[s]) %>% 
    mutate(year = china_sheets1[s]) %>% 
    mutate_all(gsub, pattern = ',', replacement = '')
} 

# Flatten list
china1 <- bind_rows(china1)

# Loop over sheets and read in data
for(s in seq_along(china_sheets2)) {
  china2[[s]] <- read_excel(path = 'sfg-aqua-data/china-data/mariculture_fish_province.xlsx', sheet = china_sheets2[s]) %>% 
    mutate(year = china_sheets2[s]) %>% 
    mutate_all(gsub, pattern = ',', replacement = '')
} 

# Flatten lists
china1 <- bind_rows(china1)
china2 <- bind_rows(china2)

# Convert variables to numeric
china_area <- china1 %>% 
  select(-X__1, -X__2) %>%
  rename(all_fish_area = `all fish`) %>% 
  left_join(china2) %>% 
  mutate_at(.vars = c(2:ncol(.)), as.numeric) %>% 
  arrange(Province, year) %>% 
  select(Province, year, everything()) %>% 
  rename_all(tolower)

# remove spaces in variable names
colnames(china_area) <- sapply(colnames(china_area), gsub, pattern = ' ', replacement = '_')

# Save data file
write_csv(china_area, path = 'sfg-aqua-data/china-data/china_area_cleaned.csv')
