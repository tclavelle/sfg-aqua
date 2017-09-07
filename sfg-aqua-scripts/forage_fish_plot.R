
library(tidyverse)
library(scales)

# load RAM data
load(file = '../upside-v2/upside-v2-data/DBdata_v415_model_fits.RData')

# RAM datasets to use
ram_datasets <- c('timeseries_values_views', 'stock', 'area', 'bioparams', 'timeseries_assessments_views',
                  'bioparams_values_views')

# Remove all unneccessary RAM data
rm(list = c(ls()[!(ls() %in% ram_datasets)]))

# Extract main RAM timeseries
ram <- timeseries_values_views %>%
  tbl_df() %>%
  select(stockid, stocklong, year, TBbest, TCbest, BdivBmsypref, UdivUmsypref, BdivBmgtpref, UdivUmgtpref) # TCbest and TBbest in MT

# Join with stock data and area data
ram <- ram %>%
  left_join(tbl_df(stock), by = c('stockid', 'stocklong')) %>%
  select(-inmyersdb, -myersstockid) %>%
  left_join(bioparams_values_views %>%
              select(stockid, MSY))

# Key fishmeal stocks include the following (from Naylor 2000):
# anchoveta, Chilean jack mackerel, atlantic herring, chub mackerel, japanese anchovy, round sardinella, atlantic
# mackerel, and european anchovy

ram_species <- unique(ram[,c('stockid','stocklong','scientificname')])

fm_stocks <- c('Engraulis ringens', 'Sardinops sagax', 'Trachurus murphyi','Mallotus villosus','Clupea harengus',
               'Brevoortia patronus', 'Ammodytes hexapterus', 'Ammodytes marinus', 'Ammodytes personatus',
               'Micromesistius poutassou', 'Engraulis japonicus', 'Brevoortia tyrannus')

forage <- ram %>%
  filter(scientificname %in% fm_stocks) %>%
  group_by(year) %>%
  summarize(total_catch = sum(TCbest, na.rm = T),
            stocks           = length(unique(stockid)),
            med_b_bmsy       = median(BdivBmsypref, na.rm = T),
            med_f_fmsy       = median(UdivUmsypref, na.rm = T),
            med_b_mgmt       = median(BdivBmgtpref, na.rm = T),
            med_f_mgmt       = median(UdivUmgtpref, na.rm = T)) %>%
  gather(metric, value, 4:ncol(.)) %>%
  mutate(ref_value = ifelse(grepl('_b_', metric), 'Biomass', 'Fishing pressure'),
         `Reference target` = ifelse(grepl('msy$', metric), 'MSY', 'Management'))


forage %>%
  filter(year > 1959) %>%
  ggplot(aes(x = year, y = value, color = `Reference target`)) +
  geom_line() +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 1, linetype = 2, color = 'black') +
  labs(x = 'Year',
       y = 'Reference Value') +
  facet_wrap(~ref_value, ncol = 1, scales = 'free_y') +
  theme_bw() +
  theme(legend.position = 'bottom')
  
ggsave(filename = 'sfg-aqua-figures/forage_status.png', height = 4, width = 5)
