########################################################################
##
## Seaweed Aquaculture Data Analysis
## 08/31/2016
##
########################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Read data
aqua <- tbl_df(read.csv(file = '../../Google Drive/Project Data/fao-data/FAO Aquaculture Data Final.csv', stringsAsFactors = F)) %>%
  filter(FoodCat != 'Other')

fish <- tbl_df(read.csv(file = '../../Google Drive/Project Data/fao-data/fao_capture_50to14.csv', stringsAsFactors = F))

# Scatter plot theme
scatterTheme <- theme_bw() + theme(axis.text=element_text(size=12),
                                   axis.title=element_text(size=12, vjust=.15),
                                   plot.margin=unit(c(.25,0.25,0.25,0.25), "lines"),
                                   legend.title = element_text(size=12),
                                   legend.text= element_text(size=12),
                                   plot.title = element_text(lineheight=.8, size=12),
                                   strip.text.x = element_text(size = 12))
  
# Calculate production of different Food types
aqua_totals <- aqua %>%
  group_by(FoodCat, Year) %>%
  summarize(total_prod  = sum(AquaProduction, na.rm = T),
            total_value = sum(AquaValue, na.rm = T)) %>%
  ungroup()

# Calculate production by Country
aqua_totals_cntry <- aqua %>%
  filter(Year == max(Year, na.rm = T)) %>%
  group_by(Country, Year) %>%
  summarize(total_prod  = sum(AquaProduction, na.rm = T),
            total_value = sum(AquaValue, na.rm = T)) %>%
  ungroup()

aqua_totals %>%
  mutate(FoodCat = factor(FoodCat, levels = c('Food Fish', 'Aquatic Plants'))) %>%
  ggplot(aes(x = Year, ymin = 0, ymax = total_prod, fill = FoodCat)) +
  geom_ribbon() +
  scale_fill_manual(values = c('lightblue','lightgreen')) +
  scale_y_continuous(labels = comma) +
  labs(fill = 'Food Category',
       y    = 'Production (MT)') +
  scatterTheme

# ggsave(filename = 'Food fish and seaweed production.pdf', width = 7.5, height = 5)


# Subset out aquatic plants
seaweed <- aqua %>%
  filter(FoodCat == 'Aquatic Plants')

# Calculate national totals annually
nat_totals <- seaweed %>%
  filter(FoodCat == 'Aquatic Plants') %>%
  group_by(Country, Year) %>%
  summarize(total_production = sum(AquaProduction, na.rm = T),
            total_value      = sum(AquaValue, na.rm = T))

# Calculate national totals annually by species
nat_sp_totals <- seaweed %>%
  filter(FoodCat == 'Aquatic Plants') %>%
  group_by(Country, CommName, Year) %>%
  summarize(total_production = sum(AquaProduction, na.rm = T),
            total_value      = sum(AquaValue, na.rm = T))

nat_sp_totals %>%
  filter(Country %in% c('China', 'Indonesia') & Year == 2013) %>%
  ggplot(aes(x = Country, y = total_production, fill = CommName)) +
  geom_bar(stat = 'identity') + 
  scale_y_continuous(labels = comma) +
  labs(y    = 'Total Production in 2013, (MT)',
       fill = 'Species') +
  scatterTheme
  
ggsave(filename = 'Seaweed species China Indo.pdf', width = 7, height = 5)

# Seaweed production by top 10 countries bar plot
seaweed %>%
  filter(FoodCat == 'Aquatic Plants' & Year == max(Year, na.rm = T)) %>%
  group_by(Country) %>%
  summarize(total = sum(AquaProduction, na.rm = T)) %>%
  ungroup() %>%
  arrange(total) %>%
  mutate(rank = dense_rank(-total)) %>%
  filter(rank < 11) %>%
  ggplot(aes(x = reorder(Country, -rank), y = total)) +
  geom_bar(stat = 'identity', fill = 'lightgreen') +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  labs(x = NULL,
       y = 'Production of aquatic plants in 2013, (MT)') +
  scatterTheme

# ggsave(filename = 'Top ten seaweed producers.pdf', width = 7, height = 5)

# Seaweed production in China and Indonesia
nat_totals %>%
  filter(Country %in% c('China', 'Indonesia', 'Philippines') & Year > 1969) %>%
  ggplot(aes(x = Year, y = total_production, color = Country)) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = comma) +
  labs(y = 'Production of aquatic plants, (MT)') +
  scatterTheme +
  theme(legend.key = element_blank())

# ggsave(filename = 'Top three seaweed producers lineplot.jpeg', width = 7, height = 5)

# Calculate the percent increase in seaweed production by Indonesia since 2002
nat_totals %>%
  filter(Country == 'Indonesia' & Year %in% c(2002, 2013)) %>%
  summarize(perc_change = 100 * (total_production[Year==2013] - total_production[Year==2002]) / total_production[Year == 2002])

# Seaweed production by species in China and Indonesia and Philippines
seaweed %>%
  filter(Country %in% c('China', 'Indonesia', 'Philippines') & Year > 1969) %>%
  group_by(Country,SpeciesCatName, Year) %>%
  summarize(total_prod  = sum(AquaProduction, na.rm = T),
            total_value = sum(AquaValue, na.rm = T)) %>%
  ggplot(aes(x = Year, y = total_prod, color = SpeciesCatName)) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c('#996600', '#339933', 'lightblue', '#ff6666')) +
  labs(y = 'Production of aquatic plants, (MT)') +
  facet_wrap(~Country, nrow = 1) +
  labs(color = 'Species Category') +
  scatterTheme +
  theme(legend.key = element_blank(),
        legend.position = 'bottom')

# ggsave(filename = 'country_species_seaweed.pdf', width = 10, height = 5)

data_frame(years         = c(2000, 2005, 2010, 2012, 2013, 2014),
                      China_fishers = c(9213, 8389, 9013, 9226, 9090, 9036),
                      China_farmers = c(3722, 4514, 4979, 5214, 5192, 5124),
                      Indonesia_fishers  = c(3105, 2590, 2620, 2749, 2640, 2667),
                      Indonesia_farmers  = c(2143, 2507, 3351, 3344, 3344, 3344)) %>%
  gather(type, number, 2:5) %>%
  separate(type, into = c('country','Occupation', sep = '_')) %>%
  ggplot(aes(x = years, y = number, linetype = Occupation)) +
  geom_line(size = 1.5) +
  facet_wrap(~country, nrow = 2, scales = 'free_y') +
  labs(y = 'Number Employed (1000s)',
       x = 'Year') +
  scatterTheme +
  theme(legend.key = element_blank())

# ggsave(filename = 'country_employment.pdf', width = 6, height = 7)
