##################################################
## Project: Mariculture vs Freshwater EFCR plot
## Script purpose:
## Date: May 22, 2017
## Author: Tyler Clavelle
##################################################

library(tidyverse)
library(scales)

# Read in FCR data
fcr <- read.delim(file = 'sfg-aqua-data/tacon_metian_2015.csv', stringsAsFactors = F)

p1 <- fcr %>%
  filter(year < 2011) %>%
  mutate(production = as.numeric(gsub(',',replacement = '', production)),
         feed_used  = as.numeric(gsub(',',replacement = '',feed_used)),
         prod_on_feed = production * (perc_on_feeds / 100)) %>%
  group_by(type, year) %>%
  summarize(production = sum(production, na.rm = T),
            prod_on_feed = sum(prod_on_feed, na.rm = T),
            feed_used    = sum(feed_used, na.rm = T),
            perc_of_prod_on_feed = prod_on_feed / production) %>%
  mutate(efcr = feed_used / prod_on_feed) %>%
  group_by(year) %>%
  mutate(all_feed = sum(feed_used, na.rm = T),
         perc_of_feed_used = feed_used / all_feed,
         perc_of_all_on_feed = prod_on_feed / sum(prod_on_feed, na.rm = T))


ggplot(p1,aes(x = year, y = efcr, color = type)) +
  geom_line() +
  scale_x_continuous(breaks = c(2000:2010)) +
  scale_color_brewer(palette = 'Set1') +
  labs(x = 'Year',
       y = 'Effective Feed Conversion\nRatio (EFCR)',
       color = 'Sector') +
  theme_bw()

p1 %>%
  filter(type == 'marine') %>%
  select(year, type, perc_of_all_on_feed, perc_of_feed_used) %>%
  gather(key = 'metric', value = 'value', 3:4) %>%
  ggplot(aes(x = year, y = value, linetype = metric)) +
  # ggplot(aes(x = year, y = value, color = type, linetype = metric)) +
  geom_line() +
  scale_x_continuous(breaks = c(2000:2010)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(labels = percent) +
  labs(x = 'Year',
       y = 'Percent of Total Aquaculture Sector',
       linetype = 'Metric')
