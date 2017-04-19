##################################################
## Project: China 
## Script purpose: Explore China data to find key datasets
## Date:
## Author: Tyler Clavelle
##################################################


# Load packages
library(tidyverse)
library(stringr)
library(xlsx)


# What year do you want to explore?
yr <- 2008 # change this variable as desired

# Load Rdata object to explore. The file path is relative to the location of this Rscript - if the data object is in the same
# folder as this script you can simply put the name of the file. 
load(file = paste('Translated R Obj/',yr,'_china_translated.Rdata', sep = ''))


# search for tables with desired word by using lapply to search for the word in each list element
lapply(ch1_trans, function(x) {
  unique(grepl('3,257.038', x, ignore.case = T))
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

saveWorkbook(wb, paste('China Data Spreadsheets/','China_data_',yr, '.xlsx', sep = '')) # save workbook for year 









## Section: Save each File Type as an excel workbook (Iwen)
##########################################################

#Use index-filename datatable column names (year) as function arguments
#Create list with years as list name and index as value
#read in index-filename data table
Index_Filename <- read.csv("Index_Filename.csv", stringsAsFactors = FALSE)
View(Index_Filename)

#change column names
years <- seq(2008, 1979)
colnames(Index_Filename) <- c("Filename", years)
View(Index_Filename)
names2 <- as.vector(Index_Filename[,1])
rownames(Index_Filename) <- names2
IndexFilename <- Index_Filename[,2:31]
View(IndexFilename)

# loop over data sets and produce workbooks for each dataset
for(a in 1:length(names2)) {
  
temp_df <- IndexFilename[names2[a],] # pull out dataset 

#Create list of indexes for each year for mariculture_fish_province
#mariculture_fish_province <- as.numeric(as.vector(IndexFilename[1,]))
#mariculture_fish_province

# Create empty workbook
wb <- createWorkbook() 

# create a worksheet in the workbook for each data table of interest
for(i in 1:ncol(temp_df)){
  
  load(file = paste('Translated R Obj/',years[i],'_china_translated.Rdata', sep = ''))
  
  # setwd("C:/Iwen/SFG/Translated R Obj")
  # years <- seq(2008, 1979)
  # Check to see if any years have multiple tables (indicated by a dash) and create separate sheets for that year
  if(any(grepl('-', temp_df[i])==TRUE)){
    
    print('multiple tables')
    index <- as.numeric(str_split(temp_df[1,i], pattern = '-', simplify = T))
    
  } else index <- temp_df[[i]]
  
# now create worksheet by looping over index vector (will usually be 1 value)
  
  message("Creating sheet", years[i])
  sheet <- createSheet(wb, sheetName = paste(years[i], sep = ' '))
  message("Adding data frame", years[i])
  addDataFrame(ch1_trans[[index]], sheet) 
}

setwd("C:/Iwen/SFG/China Data Spreadsheets (by data)")

saveWorkbook(wb, paste('mariculture_fish_province', '.xlsx', sep = '')) # save workbook for mariculture_fish_province

