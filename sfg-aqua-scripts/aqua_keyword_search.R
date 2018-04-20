
# Script Metadata ----------------------------------------------------------
# Purpose: Systematic lit search of keywords pertaining to mariculture and fisheries
# interactions.
# By: Tyler Clavelle
# Date: September 9th, 2017

### Packages
library(tidyverse)
library(stringr)
library(forcats)
# devtools::install_github("juba/rwos") # run if rwos not already installed
library(rwos)
library(RColorBrewer)

# Keywords ----------------------------------------------------------------

# Primary Keywords
primary <- c('aquaculture','mariculture','fish farm','fishery','fisheries','wild fish')

# Interaction Keywords
exclusion <- c('spatial exclusion','exclusion','use conflict','ocean grabbing','space use','ocean use')
mpa <- c('de facto MPA','fish productivity','MPA effect','increase abundance','conservation benefit')
# mpa <- c('de facto MPA','fish productivity','increase abundance','conservation benefit')
fad <- c('FAD effect','aggregation','fish attraction', 'fish aggregating device')
habitat <- c('habitat modification','habitat conversion','habitat degradation',
             'habitat impact','habitat restoration','mangrove loss')
water <- c('water quality','pollution','nutrification','effluent','phosphorous','nitrogen',
           'hypoxia','eutrophication')
disease <- c('disease','disease transmission','pathogen','parasite','sea lice')
feed <- c('feed','forage fish','fish-in fish-out','feed conversion','fishmeal',
          'fish meal','fish oil', 'reduction fisheries')
genetic <- c('genetic pollution','introgression','GMO','wild fitness','escape')
invasive <- c('invasive species','exotic species')
seed <- c('wild fingerlings', 'wild seedlings', 'seedlings', 'seedstock')
price <- c('price competition', 'market competition','price effect')
market <- c('increased market for fishery products','increased overall demand for fish',
            'seafood market','seafood demand','seafood consumption','market effect')

market_2 <- c('price competition', 'market competition','price effect', 'market integration',
                'seafood market','seafood demand','market effect')

spatial <- c('spatial exclusion', 'use conflict', 'ocean use', 'marine space', 'ocean space',
             'fish aggregation', 'fish aggregating device', 'fish attraction', 'FAD effect',
             'MPA effect', 'de facto MPA')

# List of all interaction types and keywords
int_list <- list('exclusion'= exclusion, 'mpa' = mpa, 'fad' = fad,
                 'habitat' = habitat, 'water' = water, 'disease' = disease, 
                 'feed' = feed, 'genetic' = genetic, 'invasive' = invasive,
                 'seed' = seed, 'price' = price, 'market' = market)

# Simplified list of interactions
int_list <- list('water' = water, 'disease' = disease,
                   'feed' = feed, 'genetic' = genetic,
                   'invasive' = invasive, 'market' = market_2,
                 'spatial' = spatial)

# Construct Queries -------------------------------------------------------

# Use keywords to build structured queries for the WOS search
# Aquaculture vs Mariculture query
aqua_mari_q <- paste0("(TI=(",primary[1], " OR ", primary[2], ")"," AND (DT=(Article OR Book)))")

# Primary query
prime_q <- paste0("(TS=(",primary[1], " OR ", primary[2], " OR ", primary[3],")",
                  " AND (TS=(", primary[4], ' OR ', primary[5], ' OR ', primary[6], '))',
                  " AND (DT=(Article OR Book))")

# Function for constructing queries for each interaction
build_query <- function(int) {
 paste0(prime_q, ' AND (TS=(', paste(int, collapse = " OR "),')))')
}

# Build all queries
queries <- lapply(int_list,
                  FUN = function(x) build_query(int = x))

# Web of Science Search ---------------------------------------------------

# Authenticate
sid <- wos_authenticate()

# Search for results for aquaculture vs mariculture 
aqua_mari_results <- wos_search(sid, query = aqua_mari_q)

# Search results for primary keywords
prime_res <- wos_search(sid, query = paste0(prime_q,')')) # 5,600 results

# Search results for all interactions
all_results <- lapply(queries, FUN = function(x) wos_search(sid, query = x[1]))

# Examine Results ---------------------------------------------------------

# Retrieve results for aqua vs mari search
aqua_mari_pubs <- wos_retrieve_all(aqua_mari_results)

# Retrieve results for primary keywords results
prime_pub_results <- wos_retrieve(prime_res)

# Retrieve results for all interaction queries
all_pub_results <- lapply(seq_along(all_results), function(r){
  results <- all_results[[r]]
  all_results <- wos_retrieve_all(results) %>%
    mutate(interaction = names(all_results[r]))
  return(all_results)
}) %>%
  bind_rows()

# Save CSV for filtering out unrelated studies
to_validate <- select(all_pub_results, uid, interaction, title, journal)
write_csv(to_validate, path = 'sfg-aqua-data/lit_search_results.csv')

# Summarize Results -------------------------------------------------------

# Publications by use of aquaculture, mariculture, or both in title
aqua_mari_pubs <- aqua_mari_pubs %>%
  filter(grepl('aquaculture', title, ignore.case = T) & grepl('mariculture', title, ignore.case = T) ==F) %>%
  mutate(title_word = 'aquaculture') %>%
  bind_rows(aqua_mari_pubs %>%
              filter(grepl('mariculture', title, ignore.case = T)) %>%
              mutate(title_word = 'mariculture')) 

# Plot of number of publications with aquaculture or mariculture in the title
aqua_mari_pubs %>%
  filter(year < 2017) %>%
  group_by(year, title_word) %>%
  summarize(pubs = length(unique(uid))) %>%
  ggplot(aes(x = as.numeric(year), y = pubs, color = title_word)) +
  geom_line() +
  scale_color_brewer(palette = 'Paired') +
  labs(y = 'Publications',
       x = 'Year',
       color = 'Title word') +
  theme_bw()

# Publications by interaction type over time
all_pub_results %>%
  filter(year < 2017) %>%
  group_by(interaction, year) %>%
  summarize(pubs = length(unique(uid))) %>%
  ggplot(aes(x = as.numeric(year), y = pubs, color = fct_reorder(interaction, pubs, sum, .desc = T))) +
  geom_line() +
  scale_color_brewer(palette = 'Paired') +
  labs(x = 'Year',
       y = 'Publications',
       color = 'Interaction') +
  theme_bw()

ggsave(filename = 'sfg-aqua-figures/publications_by_year.pdf', width = 5.5, height = 4, dpi = 600)
