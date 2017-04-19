library(scales)

# Key fishmeal stocks include the following (from Naylor 2000):
# anchoveta, Chilean jack mackerel, atlantic herring, chub mackerel, japanese anchovy, round sardinella, atlantic
# mackerel, and european anchovy

fm_stocks <- c('Engraulis ringens', 'Sardinops sagax', 'Trachurus murphyi','Mallotus villosus','Clupea harengus',
               'Brevoortia patronus', 'Ammodytes hexapterus', 'Ammodytes marinus', 'Ammodytes personatus',
               'Micromesistius poutassou', 'Engraulis japonicus', 'Brevoortia tyrannus')

test <- unique(ram2[,c("stocklong",'scientificname')])

forage <- ram2 %>%
  filter(scientificname %in% fm_stocks | isscaap == 'Herrings, sardines, anchovies') %>%
  group_by(year) %>%
  summarize(total_catch = sum(TCbest, na.rm = T),
            med_b       = median(BdivBmsytouse, na.rm = T),
            med_f       = median(UdivUmsytouse, na.rm = T),
            stocks      = length(unique(assessid)))

ram2 %>%
  group_by(assessid) %>%
  # filter(year == max(year, na.rm = T)) %>%
  filter(scientificname %in% fm_stocks) %>%
  group_by(year) %>%
  summarize(med_f = median(UdivUmsytouse, na.rm = T))

forage <- ram2 %>%
  filter(isscaap == 'Herrings, sardines, anchovies') %>%
  group_by(year) %>%
  summarize(total_catch = sum(catch, na.rm = T),
            med_b       = median(BdivBmsytouse, na.rm = T),
            med_f       = median(UdivUmsytouse, na.rm = T),
            stocks      = length(unique(assessid)))

forage %>%
  filter(year > 1992 & year < 2012) %>%
  rename(`B/Bmsy` = med_b,
         `F/Fmsy` = med_f) %>%
  gather(key = 'status', value = 'value', `B/Bmsy`:`F/Fmsy`) %>%
  filter(status == 'F/Fmsy') %>%
  ggplot(aes(x = year, y = value)) +
  geom_rect(aes(ymin = 0.8, ymax = 1.2, xmin = 1992, xmax = 2011), fill = '#bcf8ad', alpha = 0.4) +
  geom_point(aes(size = total_catch), alpha = 0.75) +
  geom_smooth(show.legend = F, color = 'black') +
  scale_size_continuous(breaks = c(seq(5000000, 25000000, by = 5000000)), range = c(1,8), labels = comma) +
  geom_hline(yintercept = 1, color = 'red', linetype = 2) +
  labs(y = 'Median F/Fmsy',
       x = 'Year',
       size  = 'Total\ncatch (MT)') +
  theme_bw()

ggsave(filename = '../sfg-aqua/sfg-aqua-figures/forage_status.png', height = 4, width = 5)
