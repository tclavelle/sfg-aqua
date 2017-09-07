
# Script Metadata ----------------------------------------------------------
# Purpose: Systematic lit search of keywords pertaining to mariculture and fisheries
# interactions.
# By: Tyler Clavelle
# Date: September 9th, 2017

### Packages
library(tidyverse)
library(stringr)
# devtools::install_github("juba/rwos") # run if rwos not already installed
library(rwos)

# Keywords ----------------------------------------------------------------

# Primary Keywords
primary <- c('aquaculture','mariculture','fishery','fisheries')

# Interaction Keywords
spatial <- c('spatial exclusion','use conflict','ocean grabbing','space use','ocean use')
mpa <- c('de facto MPA','fish productivity','MPA effect','increase abundance','conservation benefit')
fad <- c('FAD effect','aggregation','fish attraction')
habitat <- c('habitat modification','habitat degradation','habitat impact','habitat restoration')
water <- c('water quality','pollution','nutrification','effluents','phosphorous','nitrogen',
           'hypoxia','eutrophication')
disease <- c('disease','disease transmission','pathogen','parasite','sea lice')
feed <- c('feed','forage fish','fish-in fish-out','feed conversion','fishmeal','fish oil')
genetic <- c('genetic pollution','introgression','GMO','wild fitness','escape')
invasive <- c('invasive species','resource competition','escape')
enhancement <- c('stock enhancement','hatchery','hatcheries','conservation aquaculture')
price <- c('price competition', 'market competition')
market <- c('increased market for fishery products','increased overall demand for fish',
            'seafood market','seafood demand','seafood consumption')

# List of all interaction types and keywords
int_list <- list('spatial'= spatial, 'mpa' = mpa, 'fad' = fad,
                 'habitat' = habitat, 'water' = water, 'disease' = disease, 
                 'feed' = feed, 'genetic' = genetic, 'invasive' = invasive,
                 'enhancement' = enhancement, 'price' = price, 'market' = market)

# Construct Queries -------------------------------------------------------
# Use keywords to build structured queries for the WOS search

# Primary query
prime_q <- paste0("(TS=(",primary[1], " OR ", primary[2],")",
                  " AND (TS=(", primary[3], ' OR ', primary[4], '))',
                  " AND (DT=(", 'Article))')

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

# Retrieve results for de facto MPAs
