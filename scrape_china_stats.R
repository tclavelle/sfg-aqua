###############################################################################
##
## Extracting data from China's fishery yearbooks
##
###############################################################################

# Load packages
library(tidyverse)
library(rJava)
library(tabulizer)
library(tabulizerjars)
library(translateR)
library(stringr)
# library(ghit)
# ghit::install_github(c("ropenscilabs/tabulizer"))

# Select the file to scrape tabular data
china_pdf <- '2011 年全国渔业统计情况综述.pdf'

# Extract tables 
ch1 <- extract_tables(file = china_pdf)

# Create lookup table by unlisting all values and translating chinese characters
trans_vec <- unlist(ch1)

trans_vec2 <- unique(trans_vec[trans_vec != ''])

trans_vec2 <- trans_vec2[grepl(pattern = '.', trans_vec2, fixed =  T )==F & grepl(pattern = '[[:digit:]]', trans_vec2) == F]

trans_vec_out <- NA

for(a in 1:length(trans_vec2)) {
  
  trans_vec_out[a] <- translate(content.vec = c(trans_vec2[a]),
                                google.api.key = 'AIzaSyBoopi6QQIXe-dcD9RaU9hGdV6FPtSDseA',
                                source.lang = 'zh-CN',
                                target.lang = 'en')
}

# Create lookup table by combining vectors
lkup <- data_frame(trans_vec2, trans_vec_out)

# replace values in unlisted vector with values in lkup table
for(b in 1:length(trans_vec2)) {
  trans_vec[grepl(trans_vec2[b], trans_vec)] <- trans_vec_out[b]
}

# relist vector according to original structure
ch1_trans<-relist(trans_vec, ch1)

test <- ch1_trans[[26]] # aquacaulture ~25-37


# save data object
write_csv(ch1_1, path = 'sfg-aqua-data/china_yrbk_2011.csv')

