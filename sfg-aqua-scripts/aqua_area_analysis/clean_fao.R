##################################################
## Function purpose: Process raw wide format FAO data
## Date: 14-11-2018
## Author: Tyler Clavelle
##################################################

clean_fao <- function(file){
  
  # Read in file
  df <- read_csv(file, na = c('...', '-', '0 0'))
  
  df <- df %>% 
    select(ncol(.), 1, everything()) %>%
    gather(year, production, `1950`:ncol(.))
  
  return(df)
}