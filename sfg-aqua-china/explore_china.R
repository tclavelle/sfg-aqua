library(tidyverse)
library(maps)
library(xlsx)

load(file = 'sfg-aqua-data/china-data/2008_china_translated.Rdata')

# search for tables with desired word
lapply(ch1_trans, function(x) {
  unique(grepl('mariculture production', x, ignore.case = T))
})
# 21, 22, 58, 75, 78, 79, 80, 86, 88
gfw <- ch1_trans[c(21, 22, 58, 75, 78, 79, 80, 86, 88)]

# Save boat data to excel workbook
wb <- createWorkbook()  
for(i in seq_along(gfw))
{
  message("Creating sheet", i)
  sheet <- createSheet(wb, sheetName = paste('sheet', i, sep = ' '))
  message("Adding data frame", i)
  addDataFrame(gfw[[i]], sheet)
}
saveWorkbook(wb, "sfg-aqua-data/china-data/China_vessel_data.xlsx") 


boats <- ch1_trans[[22]]
summary <- ch1_trans[[116]]
aqua <- ch1_trans[[38]]
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

