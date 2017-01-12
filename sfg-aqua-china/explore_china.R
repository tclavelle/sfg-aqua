##################################################
## Project: China 
## Script purpose: Explore China data to find key datasets
## Date:
## Author: Tyler Clavelle
##################################################

# Load packages
library(tidyverse)
library(xlsx)

# What year do you want to explore?
yr <- 2008 # change this variable as desired

# Load Rdata object to explore. The file path is relative to the location of this Rscript - if the data object is in the same
# folder as this script you can simply put the name of the file. 
load(file = paste(yr,'_china_translated.Rdata', sep = ''))


# search for tables with desired word by using lapply to search for the word in each list element
lapply(ch1_trans, function(x) {
  unique(grepl('mariculture', x, ignore.case = T))
})

# after finding tables of interest create a vector of the list element numbers of the datasets you would like to extract
dfs_needed <- c(1,2,3,4,5) # just using 1-5 as an example

# create vector of all list elements for writing the entire R object to an Excel workbook
all_dfs <- c(1:length(ch1_trans))

## Section: Save data to excel workbook
##################################################

# Create empty workbook
wb <- createWorkbook() 

# create a worksheet in the workbook for each data table of interest
for(i in seq_along(all_dfs)) # currently set to write workbook of all tables. Change to dfs_needed if desired
{
  message("Creating sheet", i)
  sheet <- createSheet(wb, sheetName = paste('sheet', i, sep = ' '))
  message("Adding data frame", i)
  addDataFrame(ch1_trans[[i]], sheet) 
}

saveWorkbook(wb, paste('China_data_',yr, '.xlsx', sep = '')) # save workbook for year 


