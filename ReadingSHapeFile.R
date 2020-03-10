
require(sf)
require(rgdal)
require(ggplot2)
require(readxl)

#Setting working directory to local folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Importaing our own Dataset
finalData <- read_xlsx("Final Data (1).xlsx")

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

#Converting our new dataframe into a "sf" dataframe. "sf" stands for "simple feature", a standardized way to encode spatial vector data. 

#We are replacing the "geometry" of finalData with the "geomtry" from the finalData$geometry column. 
st_geometry(mapMerge) <- mapMerge$geometry

#Now we will create the plot. We call on the data from worldmap and its "simple feature" geom. Over that, we will overaly the data from "mapMerge" onto this plot. 
ggplot(data=worldMap) + 
  geom_sf() +
  geom_sf(data = mapMerge, aes(fill=`Sustainable Development Index`),color=NA,show.legend = T)
