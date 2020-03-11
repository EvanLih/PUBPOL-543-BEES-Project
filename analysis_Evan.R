
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

#Setting working directory to local folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Importaing our own Dataset
github_Link <-"https://raw.githubusercontent.com/EvanLih/PUBPOL-543-BEES-Project/master/Final_Data.csv"
finalData <- read.csv(github_Link)

t.test(finalData$`Sustainable Development Index`, finalData$accessElectricity, na.rm = TRUE)

test <- subset(finalData, !is.na(finalData$`Sustainable Development Index` | !is.na(finalData$accessElectricity)))

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

sustainElectricity <- finalData %>% select(c("Continent", "accessElectricity", "Sustainable Development Index", "COUNTRY"))

sustainElectricity$accessElectricity %<>% as.numeric()

sustainElectricity %<>% rename(SDI = "Sustainable Development Index", country = "COUNTRY")

sustainElectricity %<>% filter(!is.na(accessElectricity))

#t.test to see correlation between access Electricity and SDI
t.test(x =sustainElectricity$accessElectricity, y = sustainElectricity$SDI,  paired = FALSE)
m <- lm(sustainElectricity$accessElectricity ~ sustainElectricity$SDI)
summary(m)$r.squared

ggplot(sustainElectricity, aes(x = accessElectricity, y = SDI)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 2.5) +
  labs(x = "Access to Electricity (% of population)", 
       y = "Sustainable Development Index") +
  theme_stata()

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
#Converting our new dataframe into a "sf" dataframe. "sf" stands for "simple feature", a standardized way to encode spatial vector data. 

#We are replacing the "geometry" of finalData with the "geomtry" from the finalData$geometry column. 
st_geometry(mapMerge) <- mapMerge$geometry

#Now we will create the plot. We call on the data from worldmap and its "simple feature" geom. Over that, we will overaly the data from "mapMerge" onto this plot. 
ggplot(data=worldMap) + 
  geom_sf() +
  geom_sf(data = mapMerge, aes(fill=`Sustainable Development Index`),color=NA,show.legend = T) +
  theme_map()

ggplot(data=worldMap) + 
  geom_sf() +
  geom_sf(data = mapMerge, aes(fill = accessElectricity),color="red",show.legend = F)



#numericnumeric data - correlation analysis