library(tidyverse)
library(maps)

load(file = 'sfg-aqua-data/china-data/2008_china_translated.Rdata')

# search for tables with desired word
lapply(ch1_trans, function(x) {
  unique(grepl('price', x, ignore.case = T))
})


boats <- ch1_trans[[75]]
summary <- ch1_trans[[116]]
aqua <- ch1_trans[[66]]
fisher <- ch1_trans[[92]]
species <- ch1_trans[[33]]


write_csv(data.frame(boats), path = 'sfg-aqua-data/china-data/china_example_boats.csv')
write_csv(data.frame(summary), path = 'sfg-aqua-data/china-data/china_example_production.csv')
write_csv(data.frame(aqua), path = 'sfg-aqua-data/china-data/china_example_aquaculture.csv')
write_csv(data.frame(fisher), path = 'sfg-aqua-data/china-data/china_example_fishermen.csv')
write_csv(data.frame(species), path = 'sfg-aqua-data/china-data/china_example_species.csv')

places <- tbl1[4:33,1]

coord_df <- data_frame(location = places,
                       lat      = NA,
                       long     = NA)

write_csv(coord_df, path = 'sfg-aqua-data/china-data/china_coordinates.csv')

world.cities[grepl(places, world.cities$name, ignore.case = T),]

