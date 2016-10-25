###############################################################################
##
## Compile New Zealand Fisheries and Aquaculture Data
##
###############################################################################

# Load packages
library(tidyverse)
library(stringr)
library(fuzzyjoin)

###############################################################################
### Load Data

# Get file names for scallop and snapper csv files
sca_files <- list.files(path = '../../Google Drive/Project Data/nz-data/scallop_fishery/', full.names = T)
snap_files <- list.files(path = '../../Google Drive/Project Data/nz-data/snapper_fishery/', full.names = T)

# write function to read in all data file and add year
read_nz <- function(x) {
  temp <- read_csv(x)
  yr <- str_split(x, pattern = '_')[[1]][3]
  yr <- as.numeric(gsub(pattern = '.csv', replacement = '', yr))
  temp <- temp %>%
    mutate(Year = yr,
           Statistical_Area_Code = gsub(pattern = '[[:punct:]]', replacement = '', Statistical_Area_Code),
           est_catch_weight      = gsub(pattern = ' kgs', replacement = '', est_catch_weight),
           est_catch_weight      = as.numeric(gsub(pattern = ',', replacement = '', est_catch_weight, fixed = T)) / 1000) # convert from kg to tons
}

# apply function to scallop and snapper files and flatten into single dataframes for each species
sca <- lapply(sca_files, FUN = read_nz) %>% # Scallops
  bind_rows() %>%
  select(Species_Common_Name, Species_Scientific_Name, Species_Code, Statistical_Area_Code,
         Year, est_catch_weight, Date_Certified, Coastline_km, Area_ha)

snap <- lapply(snap_files, FUN = read_nz) %>% # Snappers
  bind_rows() %>%
  mutate(Species_Common_Name     = 'Snapper',
         Species_Scientific_Name = 'Pagrus auratus',
         Species_Code            = 'SNA') %>%
  select(Species_Common_Name, Species_Scientific_Name, Species_Code, Statistical_Area_Code,
         Year, est_catch_weight, Date_Certified, Coastline_km, Area_ha)

# Join scallop and snapper datasets
nz_catch <- bind_rows(sca, snap) 

## New Zealand port prices
nz_price <- read_csv(file = '../../Google Drive/Project Data/nz-data/nz_national_prices.csv')

# rename column names
colnames(nz_price)<-c(nz_price[1,])

nz_price<-nz_price[2:nrow(nz_price),]

# remove NA column
nz_price<-nz_price[,c(1:18,20:23)]

# rename columns with single years (2008 data is missing)
colnames(nz_price)<-c('Fishstock','FishCode','FishName','Latin name','1994','1995','1996','1997','1998','1999','2000','2001',
                '2002','2003','2004','2005','2006','2007','2009','2010','2011','2012')

# Convert to long format and rename code variable
nz_price <- nz_price %>% 
  gather('Year','Price',5:22, convert=T) %>%
  mutate(Price = as.numeric(gsub(Price, pattern = '\\$', replacement = ''))) %>%
  rename(Species_Code = FishCode)

# Assign catch to associated fish stock
  SCA1 = c('9D','9C','9B','9A','1A','1B','1C','1D','1E','1F','1G','1H','1I','1J','1L','1M','1N',
           '1O','1P','1Q','1R','1S')
  SCACS = c("2A","2B","2C","2D","2E","2F","2G","2H","2I","2J","2K","2L",
            "2M","2N","2O","2P","2Q","2R","2S","2T","2U","2W","2X","2Y")
  SCA4  = c('4A','4B','4C','4D','4E','4F','4G','4H')
  SCA7  = c('7AA','7BB','7CC','7DD','7EE','7FF','7GG','7HH','7II','7JJ','7KK','7LL')

nz_catch$Fishstock <- NA
nz_catch$Fishstock[nz_catch$Statistical_Area_Code %in% SCA1] <- 'SCA1'
nz_catch$Fishstock[nz_catch$Statistical_Area_Code %in% SCACS] <- 'SCACS'
nz_catch$Fishstock[nz_catch$Statistical_Area_Code %in% SCA4] <- 'SCA4'
nz_catch$Fishstock[nz_catch$Statistical_Area_Code %in% SCA7] <- 'SCA7'
nz_catch$Fishstock[is.na(nz_catch$Fishstock)] <- paste(nz_catch$Species_Code[is.na(nz_catch$Fishstock)], 
                                                       nz_catch$Statistical_Area_Code[is.na(nz_catch$Fishstock)], sep = '')
####
# NEED TO FINISH MATCHING PRICES
####
nz_catch <- nz_catch %>%
  left_join(nz_price)
  # stringdist_join(subset(nz_price,FishCode %in% c('SCA', 'SNA')), max_dist = 1)

# FAO aquaculture data
fao <- tbl_df(read.csv(file = '../../Google Drive/Project Data/fao-data/FAO Aquaculture Data Final.csv', stringsAsFactors = F)) %>%
  filter(Country == 'New Zealand')


###############################################################################
### Exploratory Plots

sca %>%
  filter(grepl('7', Statistical_Area_Code)) %>%
  group_by(Statistical_Area_Code, Year) %>%
  summarize(catch = sum(est_catch_weight, na.rm = T)) %>%
  ggplot(aes(x = Year, y = catch, color = Statistical_Area_Code)) +
  geom_line() +
  guides(color = F) +
  facet_wrap(~Statistical_Area_Code)

snap %>%
  filter(est_catch_weight > 0) %>%
  group_by(Statistical_Area_Code, Year) %>%
  summarize(catch = sum(est_catch_weight, na.rm = T)) %>%
  ggplot(aes(x = Year, y = catch, color = Statistical_Area_Code)) +
  geom_line() +
  guides(color = F) +
  facet_wrap(~Statistical_Area_Code)

###############################################################################
### Join FAO data and run preliminary regressions

# format fao data to have individual variables for each aquaculture type
fao_aqua <- fao %>%
  select(Year, CommName, AquaProduction, AquaValue) %>%
  rename(aqua_CommName = CommName)

fao_aqua$AquaProduction[is.na(fao_aqua$AquaProduction)] <- 0
fao_aqua$AquaValue[is.na(fao_aqua$AquaValue)] <- 0
  

# join with catch data and test
nz_catch <- nz_catch %>%
  left_join(fao_aqua) %>%
  arrange(Statistical_Area_Code, Year)

# Define model formula
m1_fmla <- 'log(est_catch_weight) ~ Statistical_Area_Code + aqua_CommName + AquaProduction + AquaValue + 
aqua_CommName * AquaProduction + AquaValue * aqua_CommName'
  
# Apply model to scallop and snapper catch series
nz_catch %>%
  split(.$Species_Common_Name) %>%
  map(~ lm(m1_fmla, data = .)) %>%
  map(summary)

# Run model on fish price
m2_fmla <- 'Price ~  est_catch_weight + AquaProduction + aqua_CommName'

# Apply model to scallop and snapper catch series
nz_catch %>%
  filter(Species_Code == 'SCA') %>%
  split(.$Species_Common_Name) %>%
  map(~ lm(m2_fmla, data = .)) %>%
  map(summary)
