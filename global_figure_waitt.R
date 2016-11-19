###################################################
##
## Figure of global protein demand and aquaculture
##
###################################################

library(tidyverse)
library(scales)
library(RColorBrewer)
library(gridExtra)

# Data
year      <- c(1980,2015)
beef_veal <- c(50000000,67451000)
pork      <- c(51000000,117567900)
poultry   <- c(25000000,112538900)
sheep     <- c(8000000,14256900)
aqua      <- c(5400000*.75,73783700*.75)
wild_fish   <- c(45000000*.75,67000000*.75)

tot_2015 <- beef_veal[2] + pork[2] +poultry[2] + sheep[2] + aqua[2] + wild_fish[2]
tot_1980 <- beef_veal[1] + pork[1] +poultry[1] + sheep[1] + aqua[1] + wild_fish[1]
fish_recovered <- 8000000
protein_gap <- 435504000 - (beef_veal[2] + pork[2] +poultry[2] + sheep[2])  
  
# 30563326 protein demand from Tilman/Steve but doesn't match graph, which is already over 400

# Build dataframe for future protein requirements
df <- data.frame(category=c('c1980','c2015',"Wild fish", "?", "Demand in 2050"),
                    value=c(tot_1980,
                            tot_2015-tot_1980,
                            fish_recovered,
                            protein_gap-fish_recovered, 
                            -(tot_2015 + fish_recovered + protein_gap)))

df$order = seq(1, nrow(df))

last.id <- nrow(df)
df$value[last.id] <- -df$value[last.id]

## Calculate the cumulative sums
df <- df %>% mutate(cs1 = cumsum(value))

## Throw a warning if the values don't match zero as expected
final_value <- round(df$cs1[nrow(df)], 1)

if (final_value != 0) {
  #  warning(sprintf("Final value doesn't return to 0.", final_value))
}

## Calculate the max and mins for each category and sector
df <- transform(df,
                min.val = c(0, head(cs1,-1)),
                max.val = c(head(cs1,-1), 0))
df <- df %>% group_by(order, category, value, cs1) %>%
  summarize(y_min = min(min.val, max.val),
            y_max = max(min.val, max.val))

## Create the lines data frame to link the bars
lines <- df %>% group_by(order) %>% summarize(cs = max(cs1))
lines <- with(lines, data.frame(
  x = head(order,-1),
  xend = tail(order,-1),
  y = head(cs,-1),
  yend = head(cs,-1)
))

lines <- lines[3:nrow(lines),]

## Add the offset parameter
df <- transform(df, offset = 0.4) %>%
  filter(category %in% c('Wild fish', '?', 'Demand in 2050')) %>%
  mutate(label = c(prettyNum(paste('Recovering\nfisheries\n',df$value[3], ' MT'), big.mark = ',', digits = 3),
                   prettyNum(paste('Animal\nprotein gap\n', df$value[4], ' MT'), digits = 3, big.mark = ','),
                   prettyNum(paste('Approx. demand\n in 2050\n', df$value[5], ' MT'), digits = 3, big.mark = ',')))

# Build dataframe
df2 <- data_frame(year = year, 
                  Beef = beef_veal,
                  Pork = pork,
                  Poultry = poultry,
                  Mutton = sheep,
                  `Wild fish` = wild_fish,
                  Aquaculture = aqua) %>%
  gather(key = 'Protein\nsource', value = 'Consumption', 2:7)


# Build plot
ggplot(df2) +
  geom_bar(aes(x = factor(year), y = Consumption, fill = `Protein\nsource`), stat = 'identity') +
  scale_fill_brewer(type = 'qual', palette = 'Set2') +
  geom_segment(data = lines,
               aes(
                 x = x,
                 y = y,
                 xend = xend,
                 yend = yend
               ),
               linetype = "dashed")  +
  geom_rect(
    data = df,
    aes(
      xmin = order - offset,
      xmax = order + offset,
      ymin = y_min,
      ymax = y_max,
      labels = label
    ),
    fill = c('#ffd92f','lightgrey', 'darkgrey')
  ) +
  geom_label(data = df,
             nudge_y = max(df$y_max)*.1,
             aes(
               x = order,
               y = y_max,
               label = label),
             size = 2) +
  # geom_tex
  scale_y_continuous(labels = comma) +
  labs(x = 'Year',
       y = 'Consumption (metric tons)',
       title = 'Global demand for animal protein in 2050') +
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())

ggsave(filename = "Global animal protein.pdf", width = 8, height = 6, units = 'in', dpi = 600)
