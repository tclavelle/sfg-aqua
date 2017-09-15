
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
fad <- c('FAD effect','aggregation','fish attraction')
habitat <- c('habitat modification','habitat conversion','habitat degradation','habitat impact','habitat restoration',
             'mangrove loss')
water <- c('water quality','pollution','nutrification','effluents','phosphorous','nitrogen',
           'hypoxia','eutrophication')
disease <- c('disease','disease transmission','pathogen','parasite','sea lice')
feed <- c('feed','forage fish','fish-in fish-out','feed conversion','fishmeal','fish meal','fish oil')
genetic <- c('genetic pollution','introgression','GMO','wild fitness','escape')
invasive <- c('invasive species','resource competition','exotic species')
enhancement <- c('stock enhancement','hatchery','hatcheries','conservation aquaculture')
price <- c('price competition', 'market competition','price effect')
market <- c('increased market for fishery products','increased overall demand for fish',
            'seafood market','seafood demand','seafood consumption','market effect')

# List of all interaction types and keywords
int_list <- list('exclusion'= exclusion, 'mpa' = mpa, 'fad' = fad,
                 'habitat' = habitat, 'water' = water, 'disease' = disease, 
                 'feed' = feed, 'genetic' = genetic, 'invasive' = invasive,
                 'enhancement' = enhancement, 'price' = price, 'market' = market)

# Construct Queries -------------------------------------------------------
# Use keywords to build structured queries for the WOS search

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

# Search results for primary keywords
prime_res <- wos_search(sid, query = paste0(prime_q,')')) # 5,600 results

# Search results for all interactions
all_results <- lapply(queries, FUN = function(x) wos_search(sid, query = x[1]))

# Examine Results ---------------------------------------------------------

# Retrieve results for primary keywords results
prime_pub_results <- wos_retrieve(prime_res)

# Retrieve results for all interaction queries
all_pub_results <- lapply(seq_along(all_results), function(r){
  results <- all_results[[r]]
  firsts <- seq(from = 1, to = results[2]$results, by = 100)
  all_pubs <- lapply(firsts, function(x) {
    temp <- wos_retrieve(results, first = x)
  }) %>%
    bind_rows() %>%
    mutate(interaction = names(all_results[r]))
  return(all_pubs)
}) %>%
  bind_rows()

# Save CSV for filtering out unrelated studies
to_validate <- select(all_pub_results, uid, interaction, title, journal)
write_csv(to_validate, path = 'sfg-aqua-data/lit_search_results.csv')

# Summarize Results -------------------------------------------------------

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
