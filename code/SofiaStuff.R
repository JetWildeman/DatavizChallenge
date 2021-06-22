
library(tidyverse)
library(ggplot2)
library(sf)
library(rmapshaper)
library(raster)

# Opening the Data
SHAPES<-readRDS("data/challenge_data.rds")
DATA<-SHAPES@data

SHAPES<-SHAPES%>%
    st_as_sf()#%>%
    #ms_simplify()

SHAPES2<-SHAPES%>%
    filter(iso_code.x=="NGA")%>%
    transmute(id=gdlcode)%>%
    st_transform(crs=4326)

imported_raster=read.csv("tmp/nga_pd_2019_1km_ASCII_XYZ/nga_pd_2019_1km_ASCII_XYZ.csv")

foo_sf <- imported_raster %>%
    st_as_sf(
        coords = c("X", "Y"),
        crs = 4326
    )#%>%
    # ms_simplify()

# Merging Databases
dim(foo_sf)
boo<-foo_sf%>%
    st_intersection(SHAPES2)
dim(boo)

# save(boo,file="data/BOO.RData")


#####
DATA<-DATA%>%inner_join(SHAPES,
                        by=c("GDLcode","iso_code","country","region"))

# Plotting with ggplot
# ntile(bla,20) # to make ventiles
DATA %>%
    filter(year == 2019) %>%
    group_by() %>%
    mutate(ventiles = ntile(shdi, 20) %>% as.factor()) %>%
    ggplot() +
    geom_sf(aes(fill = ventiles), color = "white") +
    theme_map() +
    scale_fill_viridis_d()




