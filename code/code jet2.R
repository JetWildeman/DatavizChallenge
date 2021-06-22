
## new part:

library(rgdal)
library(raster)

memory.limit(size=80000)

#boundaries shapefiles
poly<-readOGR(dsn="tmp", layer="shdi2019_World")
gcode<-data.frame(ID=1:nrow(poly),
                  gdlcode=poly$gdlcode, key="ID") # IDs

# setting working directory
setwd("tmp")

#load tif file with the pop estimates from sedac
s<-stack(list.files(path="2020 sedac/all ages",full.names=T))
poly<-spTransform(poly, crs(s))
e<-extract(s, poly, cellnumbers=TRUE, df=TRUE) 
saveRDS(e, "tmp/pop_est.rds")
