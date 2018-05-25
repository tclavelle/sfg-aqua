##################################################
## Project: Indonesia aquaculture data exploration
## Author: Jen Bone
##################################################

library(tidyverse)
library(ggplot2)

data <- read_csv("sfg-aqua-data/indo-data/indo_aqua_stats.csv", na = c('-'))
# data[is.na(data)] <- 0

target<- c("brackish pond", "marine culture")

hadatareshaped<- data %>%
  filter(type %in% target & metric == "area_hectares") %>%
  gather(year, area, `2005`:`2015`) %>%
  mutate(year = as.numeric(year)) %>% 
  # mutate(year= ifelse(year=="X2005", "2005",
  #                     ifelse (year == "X2006", "2006",
  #                             ifelse (year == "X2007", "2007",
  #                                     ifelse (year == "X2008", "2008",
  #                                             ifelse (year == "X2009", "2009",
  #                                                     ifelse (year == "X2010", "2010",
  #                                                             ifelse (year == "X2011", "2011",
  #                                                                     ifelse (year == "X2012", "2012",
  #                                                                             ifelse (year == "X2013", "2013",
  #                                                                                     ifelse (year == "X2014", "2014",
  #                                                                                             ifelse (year == "X2015", "2015","no"))))))))))))%>%
  spread(type,area)%>%
  #mutate(totalarea= brackish_pond + marine_culture)%>%
  filter(province != "Indonesia")


plotha<- hadatareshaped %>%
  ggplot(aes(x = year, y = marine_culture, group = province))+
  geom_line(aes(color = province), size = 1)+
  labs(x= "Year", y= "ha")+
  scale_fill_manual(name= "Area by province")+
  theme_classic()+
  facet_wrap(~region, scales= "free") +
  theme(axis.text.x=element_blank(),
        legend.position="bottom")

plotha2<-plotha +
  geom_line(aes(x = year, y = brackish_pond, group = province, color = province), 
            linetype = "twodash", size = 1)

png(filename="Area_type_region.png",width= 10, height= 10, units= 'in', res=300)
plot(plotha2)
dev.off()


#####
#intensification plot
#####
target<- c("brackish_pond", "marine_culture")
tonsdatareshaped<- data %>%
  filter(type%in%target & metric == "tons")%>%
  gather(year, area, X2005:X2015)%>%
  mutate(year= ifelse(year=="X2005", "2005",
                      ifelse (year == "X2006", "2006",
                              ifelse (year == "X2007", "2007",
                                      ifelse (year == "X2008", "2008",
                                              ifelse (year == "X2009", "2009",
                                                      ifelse (year == "X2010", "2010",
                                                              ifelse (year == "X2011", "2011",
                                                                      ifelse (year == "X2012", "2012",
                                                                              ifelse (year == "X2013", "2013",
                                                                                      ifelse (year == "X2014", "2014",
                                                                                              ifelse (year == "X2015", "2015","no"))))))))))))%>%
  spread(type,area)%>%
  #mutate(totaltons= brackish_pond + marine_culture)%>%
  filter(province != "Indonesia")



intensificationjoin<-left_join(tonsdatareshaped,hadatareshaped, by=c("year", "region", "province"))

intensificationvalue<-intensificationjoin%>%
  mutate(brackintense=brackish_pond.x/brackish_pond.y)%>%
  mutate(marineintense=marine_culture.x/marine_culture.y)

intensificationvalue$brackintense[is.infinite(intensificationvalue$brackintense)] <- NA
intensificationvalue$marineintense[is.infinite(intensificationvalue$marineintense)] <- NA

#plottons<- intensificationvalue %>%
#  ggplot(aes(x = year, y = marineintense, group = province))+
#  geom_line(aes(color = province), size = 1)+
#  labs(x= "Year", y= "tons/ha")+
#  scale_fill_manual(name= "Intensification by province")+
#  theme_classic()+
#  facet_wrap(~region, scales= "free") +
#  theme(axis.text.x=element_blank(),
#        legend.position="bottom")


#png(filename="Intensification_type_region.png",width= 10, height= 10, units= 'in', res=300)
#plot(plottons2)
#dev.off()


gatheredintensification<- intensificationvalue %>%
  select(province, region, year, brackintense, marineintense)%>%
  gather(type,intensity,brackintense:marineintense)

plottons2<- gatheredintensification %>%
  ggplot(aes(x = year, y = intensity, group = province))+
  geom_line(aes(color = province), size = 1)+
  labs(x= "Year", y= "tons/ha")+
  scale_fill_manual(name= "Intensification by region and type")+
  theme_classic()+
  facet_wrap(~region + type, scales= "free") +
  theme(axis.text.x=element_blank(),
        legend.position="bottom")

####
#look at Papua and Java
##CURRENTLY NOT ACCURATE
####
countries<- c("Jawa Barat","Papua Barat")
subsetplottons<- gatheredintensification %>%
  filter(province == countries) %>%
  ggplot(aes(x = year, y = intensity, group = province))+
  geom_line(aes(color = province), size = 1)+
  labs(x= "Year", y= "tons/ha")+
  scale_fill_manual(name= "Intensification by province")+
  theme_classic()+
  facet_wrap(~province, scales= "free") +
  theme(axis.text.x=element_blank(),
        legend.position="bottom")

subsetplottons2<-subsetplottons+
  geom_line(aes(x = year, y = brackintense, group = province, color = province), 
            linetype = "twodash", size = 1)
