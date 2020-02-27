library(sf)
library(rgdal)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# test <- readOGR(dsn = "UIA_World_Countries_Boundaries",
#                 layer = "UIA_World_Countries_Boundaries",
#                 verbose = FALSE)
# 
# test1 <- fortify(test)
# 
# 
# ggplot() +
#   geom_path(data = test1, 
#             aes(x = long, y = lat, group = group),
#             color = 'gray', fill = 'white', size = .2)
# 
# ggplot() + geom_polygon(data = test, aes(x = long, y = lat, group = group, fill = COUNTRY))

testImport <- read_xlsx("Final Data (1).xlsx")

testImport <- rename(testImport, COUNTRY = Country)

mapLink="https://github.com/EvanLih/PUBPOL-543-BEES-Project/raw/master/UIA_World_Countries_Boundaries.json"

PROJmap="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
worldMap=topojson_read(mapLink,crs=PROJmap,stringsAsFactors = FALSE)

testMerge <- merge.sf(testImport, worldMap, by = "COUNTRY", all.x = F)
st_geometry(testMerge) <- testMerge$geometry


ggplot(data=worldMap) + geom_sf() +
  geom_sf(data = testMerge, aes(fill=`Sustainable Development Index`),color=NA,show.legend = T)
