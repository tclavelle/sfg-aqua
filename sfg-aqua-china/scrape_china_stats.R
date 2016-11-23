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
library(parallel)
# library(ghit)
# ghit::install_github(c("ropenscilabs/tabulizer"))

# Select the file to scrape tabular data
# china_pdf <- '2011 年全国渔业统计情况综述.pdf'

# Get filenames of all china pdfs
china_pdfs <- list.files(path = '~/Desktop/Box Sync/China Data/', full.names = T)

# Create results directory to save translated files
dir.create(path = 'sfg-aqua-data/china-data', recursive = T)

translate_china <- function(y) { 
  print(y)
# Extract tables
ch1 <- extract_tables(file = y)

# Create lookup table by unlisting all values and translating chinese characters
trans_vec <- unlist(ch1)

trans_vec2 <- unique(trans_vec[trans_vec != ''])

trans_vec2 <- trans_vec2[grepl(pattern = '.', x = trans_vec2, fixed =  T )==F & grepl(pattern = '[[:digit:]]', x = trans_vec2) == F]

# translate character strings from Chinese to English
trans_vec_out <- sapply(trans_vec2, USE.NAMES = F, FUN = function(x) {
  tmp <- translate(content.vec = x,
               google.api.key = 'AIzaSyBoopi6QQIXe-dcD9RaU9hGdV6FPtSDseA',
               source.lang = 'zh-CN',
               target.lang = 'en')
  return(tmp)
})

# Create lookup table by combining vectors
lkup <- data_frame(trans_vec2, trans_vec_out)

# replace values in unlisted vector with values in lkup table
for(b in 1:length(trans_vec2)) {
  trans_vec[grepl(pattern = trans_vec2[b], x = trans_vec, fixed = T)] <- trans_vec_out[b]
}

# relist vector according to original structure
ch1_trans<-relist(trans_vec, ch1)

# get year name of file
yr <- str_extract(y, pattern = '[[:digit:]]{4}')

# save data object
save(ch1_trans, file = paste('sfg-aqua-data/china-data/',yr,'_china_translated.Rdata', sep = ''))

# write.table(paste(round(100*(y/length(pdfs)),2),'% Done with Translations',sep=''),
#             file = 'sfg-aqua-data/china-data/Translation Progress.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
ch1_trans <- 'fuck'
return(ch1_trans)
}
 
# china_results <- mclapply(china_pdfs,translate_china, mc.cores = 4)

china_results <- lapply(china_pdfs, FUN = translate_china)
# aqua <- tbl_df(ch1_trans[[26]]) # aquacaulture ~25-37
# aqua_compare <- ch1[[35]]

# save data object
# write_csv(aqua, path = 'sfg-aqua-data/china_yrbk_2011.csv')

###############################################################################
## Clean up workbook

# load(file = 'sfg-aqua-data/china-data/1979_china_translated.Rdata')

