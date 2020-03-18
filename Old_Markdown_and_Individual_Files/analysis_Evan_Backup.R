
require(sf)
require(rgdal)
require(ggplot2)
require(readxl)
library(dplyr)
library(geojsonio)
library(magrittr)
library(ggrepel)
library(ggpmisc)
library(ggthemes)
require(lavaan)
require(BBmisc)
require(RColorBrewer)
library(tmap)

#Setting working directory to local folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Importaing our own Dataset
github_Link <-"https://raw.githubusercontent.com/EvanLih/PUBPOL-543-BEES-Project/master/Final_Data.csv"
finalData <- read.csv(github_Link)

finalData %<>% select(1:17)

finalData <- finalData[complete.cases(finalData), ]

finalData$Income..GNI.per.capita.constant.2011..PPP. %<>% as.numeric()

modelNUM = '
demox=~Sustainable.Development.Index + ResourceRent +Forest + Agricultural + accessElectricity + Arable.Land + RenewConsumption'

fitNUM<-cfa(modelNUM, data = finalData)
indexCFA=lavPredict(fitNUM)
indexCFANorm=normalize(indexCFA, 
                       method = "range", 
                       margin=2, # by column
                       range = c(0, 10))

finalData$indexCFA=(indexCFANorm)

plot(finalData$Sustainable.Development.Index, finalData$indexCFA)





t.test(finalData$`Sustainable.Development.Index`, finalData$accessElectricity, na.rm = TRUE)

test <- subset(finalData, !is.na(finalData$`Sustainable.Development.Index` | !is.na(finalData$accessElectricity)))

#Renaming the column "country" to COUNTRY in order to match the shapefile/json we are pulling in. Although it is possible to 
finalData <- rename(finalData, COUNTRY = Country)

#Getting mapping data we uploaded. 
mapLink="https://github.com/EvanLih/PUBPOL-543-BEES-Project/raw/master/UIA_World_Countries_Boundaries.json"
#Getting projection which we will plot our map data on. 
PROJmap="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#PLot map data
worldMap=topojson_read(mapLink,crs=PROJmap,stringsAsFactors = FALSE)


#Now we are merging our own data with the map data by COUNTRY. This will allow us to plot the data. "all.x = F" means that we are choosing NOT to include columns that don't match the "COUNTRY" from our original data 
mapMerge <- merge(finalData, worldMap, by = "COUNTRY", all.x = F)





sustainElectricity <- finalData %>% select(c("Continent", "accessElectricity", "Sustainable.Development.Index", "COUNTRY"))

sustainElectricity$accessElectricity %<>% as.numeric()

sustainElectricity %<>% rename(SDI = "Sustainable.Development.Index", country = "COUNTRY")

sustainElectricity %<>% filter(!is.na(accessElectricity))

#t.test to see correlation between access Electricity and SDI
t.test(x =sustainElectricity$accessElectricity, y = sustainElectricity$SDI,  paired = FALSE)
m <- lm(sustainElectricity$accessElectricity ~ sustainElectricity$SDI)
summary(m)$r.squared

interestCountry <- sustainElectricity %>%
  filter(country == "United States" |
           country == "China" |
           country == "Nigeria" |
           country == "Australia" |
           country == "Singapore" |
           country == "Peru" |
           country == "Germany")


ggplot(sustainElectricity, aes(x = accessElectricity, y = SDI)) +
  geom_point(aes(color = Continent)) +
  labs(x = "Access to Electricity (% of population)", 
       y = "Sustainable.Development.Index (SDI)",
       caption = "Source : Sustainable Development Project and The World Bank (2015)",
       title = "Is Electricity Access Related to SDI?") +
  theme_stata() +
  geom_text_repel(data = interestCountry,  aes(x = accessElectricity, y = SDI,label=country)) +
  facet_wrap(~Continent, ncol = 1) 



sustainElectricity1 <- sustainElectricity %>%
  filter(!is.na(accessElectricity)) %>%
  group_by(Continent) %>%
  summarise(meanAccess = mean(accessElectricity), 
            meanSDI = mean(SDI))
  

ggplot(sustainElectricity1, aes(x = meanAccess, y = meanSDI)) +
  geom_text_repel(aes(label = Continent)) +
  geom_point() +
  labs(x = "Access To Electricity (Mean)", y = "Sustainability Development Index Rating (Mean)")+
  theme_stata()

only100sustain <- sustainElectricity %>%
  filter(!is.na(accessElectricity)) %>%
  filter(accessElectricity > 90) %>%
  group_by(Continent) %>%
  summarise(meanAccess = mean(accessElectricity), 
            meanSDI = mean(SDI))
  
  
ggplot(only100sustain, aes(x = meanAccess, y = meanSDI)) +
  geom_text_repel(aes(label = Continent)) +
  geom_point() +
  labs(x = "Average Access To Electricity (% of Population)", y = "Average Sustainability Development Index Rating (Higher is Better)",
       caption = "Source : Sustainable Development Project and The World Bank (2015)")+
  ggtitle("How does SDI Compare Across Continents for Countries with Widespread Electricity Access?") +
  theme_stata()
  
  
#Converting our new dataframe into a "sf" dataframe. "sf" stands for "simple feature", a standardized way to encode spatial vector data. 

#We are replacing the "geometry" of finalData with the "geomtry" from the finalData$geometry column. 
st_geometry(mapMerge) <- mapMerge$geometry

#Now we will create the plot. We call on the data from worldmap and its "simple feature" geom. Over that, we will overaly the data from "mapMerge" onto this plot.

##Creating "cut" intervals to plot

mapMerge$cut=cut_number(mapMerge$indexCFA,5,
                            ordered_result=T,
                            dig.lab=5)

mapMerge$cutSDI=cut_number(mapMerge$Sustainable.Development.Index,5,
                        ordered_result=T,
                        dig.lab=5)

#Getting average SDI Index for each continent. 
install.packages("gridExtra")
install.packages("maptools")
library(rgdal)
library(maptools)

mapMerge$cutSDI %<>% as.numeric()

mapMerge %<>%
  group_by(Continent) %>%
  mutate_at(vars(cutSDI, cut),
            funs(as.numeric)) %>%
  mutate(meanSDI = round(mean(cutSDI), digits= 2),
         meanNewSDI = round(mean(cut), digits = 2)) %>%
    mutate_at(vars(meanSDI, meanNewSDI),
              funs(as.factor))


mapMerge$meanSDI %<>% as.factor()


ggplot(data=worldMap) + 
  geom_sf() +
  geom_sf(data = mapMerge, aes(fill=meanNewSDI),color=NA,show.legend = T) +
  theme_map() +
  scale_fill_brewer(palette = 'YlGnBu',
                    name = "New Index (Higher is Better)") +
  labs( caption = "Source: Sustainable Development Project, World Bank 2015",
        title = "New SDI index") +
  theme(plot.title = element_text(hjust = .5),
        plot.caption = element_text(hjust= 0))


ggplot(data=worldMap) + 
  geom_sf() +
  geom_sf(data = mapMerge, aes(fill=meanSDI),color=NA,show.legend = T) +
  scale_fill_brewer(palette = 'YlGnBu',
                    name = "SDI Index (Higher is Better)") +
  theme_map()

ggplot(data=worldMap) + 
  geom_sf() +
  geom_sf(data = mapMerge, aes(fill = accessElectricity),color="red",show.legend = F) +
  labs( caption = "Source : The World Bank (2015)",
       title = "Is Electricity Access Related to SDI?")
  
geom_sf_text(aes(label=CONTINENT))



#numericnumeric data - correlation analysis