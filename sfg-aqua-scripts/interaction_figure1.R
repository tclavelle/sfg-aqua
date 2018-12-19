##################################################
## Project: Aquaculture interactions
## Script purpose: Production figure
## Date: 11/30/2018
## Author: Tyler Clavelle
##################################################


# Packages & Data ---------------------------------------------------------

library(tidyverse)
library(patchwork)
library(wesanderson)

# Source processing scripts
load('../fishstatJ/data/capture.rda')
load('../fishstatJ/data/aqua.rda')

# Unfed categories
seaweeds <- c("Brown seaweeds","Green seaweeds",
           "Miscellaneous aquatic plants","Red seaweeds")

# Major forage fish species
forage_fish <-  c('Engraulis ringens', 'Engraulis japonicus', 'Clupea harengus', 
                  'Sardina pilchardus','Sardinella spp')#,'Brevoortia patronus', 'Mallotus villosus')

non_food_fish <- c('Corals', 'Krill, planktonic crustaceans', 'Blue-whales, fin-whales',
                   'Sperm-whales, pilot-whales', 'Crocodiles and alligators', 'Pearls, mother-of-pearl, shells',
                   'Sea-squirts and other tunicates', 'Frogs and other amphibians', 'Sponges', 'Turtles',
                   'Miscellaneous aquatic mammals', 'Eared seals, hair seals, walruses')

# Long format function
fao_to_long <- function(df, value = 'value', dataset = 'capture'){
  
  # Convert to long format
  df <- df %>%
    ungroup() %>%
    gather(year, value, `1950`:ncol(.)) %>%
    mutate(dataset = dataset,
           year = as.integer(year))
  
  return(df)
}

# process capture
capture <- fao_to_long(capture, value = 'production')
aqua <- fao_to_long(aqua, value = 'production', dataset = 'aquaculture')

# Add categories for aquaculture
fao <- aqua %>% 
  filter(!isscaap %in% non_food_fish) %>% 
  mutate(category = ifelse(isscaap %in% c("Abalones, winkles, conchs",
                                        "Clams, cockles, arkshells",
                                        "Oysters",
                                        "Mussels",
                                        "Scallops, pectens",
                                        "Miscellaneous marine molluscs",
                                        "Squids, cuttlefishes, octopuses"), 'Bivalves,\nmolluscs', 'Marine\nfinfish'),
       category = ifelse(isscaap %in% c("Shrimps, prawns"), 'Shrimps,\nprawns', category),
       category = ifelse(isscaap %in% c("Salmons, trouts, smelts", "Miscellaneous diadromous fishes"),
                         'Salmon and misc.\ndiadromous fishes', category),
       category = ifelse(isscaap %in% seaweeds, 'Aquatic\nplants', category),
       food_cat = ifelse(isscaap %in% c(seaweeds, non_food_fish), 'Non food fish', 'Food fish'))

# Summarize global capture production and subtract quantity of non food use
global_capture <- capture %>%
  filter(!isscaap %in% c(seaweeds, non_food_fish),
         !scientific_name %in% forage_fish,
         year >= 1990) %>% 
  group_by(year) %>% 
  summarize(production = sum(as.numeric(value), na.rm = T))

forage <- capture %>% 
  filter(year == 2014, isscaap == 'Herrings, sardines, anchovies') %>% 
  group_by(common_name, scientific_name) %>% 
  summarize(catch = sum(as.numeric(value), na.rm = T)) %>% 
  arrange(desc(catch))
# Figure ------------------------------------------------------------------

# Figure theme
fig_theme <- theme_bw() +
  theme(#legend.position = 'bottom',
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.margin = margin(0.2))

# Total global production by environment
p1 <- fao %>% 
  filter(year >= 1990,
         food_cat == 'Food fish') %>%
  group_by(year, environment) %>% 
  summarize(production = sum(value, na.rm = T) / 1e6) %>% 
  ggplot() +
  geom_area(aes(x = year, y = production, fill = fct_relevel(environment, c('Brackishwater', 'Marine', 'Freshwater')))) +
  scale_fill_brewer(palette = 'Paired', direction = 1) +
  geom_line(data = global_capture, aes(x = year, y = production / 1e6, color = 'Capture\nfisheries'), linetype = 2) +
  scale_color_manual(values = 'black') +
  scale_x_continuous(breaks = seq(1990, 2015, by = 5)) +
  coord_cartesian(ylim = c(0,80)) +
  labs(y = 'Production (food fish, MMT)',
       x = 'Year',
       fill = 'Environment',
       color = NULL,
       title = 'A') +
  fig_theme

# Total mariculture production by species category
p2 <- fao %>% 
  filter(year >= 1990,
         environment %in% c('Marine', 'Brackishwater')) %>% 
  group_by(year, category) %>% 
  summarize(production = sum(value, na.rm = T) / 1e6) %>% 
  ggplot(aes(x = year, y = production, fill = fct_reorder(category, production, .fun = last))) +
  geom_area() +
  scale_fill_brewer(palette = 'Blues', direction = 1) +
  scale_x_continuous(breaks = seq(1990, 2015, by = 5)) +
  coord_cartesian(ylim = c(0,80)) +
  labs(y = 'Mariculture Production (MMT)',
       x = 'Year',
       fill = 'Species Group',
       title = 'B') +
  fig_theme

# Combine plots
fig_out <- p1 + p2 + plot_layout(nrow = 2)

ggsave(filename = 'sfg-aqua-figures/interact_fig_1_with_capture.pdf', dpi = 800, width = 180, height = 130, units = 'mm')


# Calculations for caption ------------------------------------------------

# Percent of marine and brackish farming compared to total food fish production
fao %>% 
  filter(year == 2016,
         food_cat == 'Food fish') %>%
  group_by(year, environment) %>% 
  summarize(production = sum(value, na.rm = T) / 1e6) %>% 
  group_by(year) %>% 
  mutate(percent = production / sum(production, na.rm = T) * 100)

fao %>% 
  filter(year == 2016,
         environment %in% c('Marine', 'Brackishwater')) %>% 
  group_by(year, category) %>% 
  summarize(production = sum(value, na.rm = T) / 1e6) %>%
  group_by(year) %>% 
  mutate(percent = production / sum(production, na.rm = T) * 100)
