
library(tidyverse)
library(ggplot2)
library(sf)
library(rmapshaper)
library(raster)
library(rgeos)
library(sugarbag)

# Opening the Data
SHAPES<-readRDS("data/challenge_data.rds")
DATA<-SHAPES@data

DATA<-DATA%>%
    filter(str_detect(gdlcode, "NGA", negate = FALSE))

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

ggplot() +
    geom_raster(data = imported_raster , aes(x = X, y = Y, fill = Z)) +
    scale_fill_continuous(type = "viridis")+
    ggtitle("Continuous Elevation Map - NEON Harvard Forest Field Site") +
    coord_equal()


# Merging Databases
# dim(foo_sf)
# boo<-foo_sf%>%
#     st_intersection(SHAPES2)
# dim(boo)

# save(boo,file="data/BOO.RData")
load("data/BOO.RData")

unique(boo$id)

#####

CELLS<-read.csv("tmp/cell-gdlcode-2020-c.csv")

CELLS_NIG<-CELLS%>%filter(str_detect(gdlcode, "NGA", negate = FALSE))

CELLS_NIG$gdlcode<-factor(CELLS_NIG$gdlcode,
                          levels = unique(CELLS_NIG$gdlcode),
                          labels = unique(DATA$gdlcode))

DATA$gdlcode<-factor(DATA$gdlcode,
                          levels = unique(DATA$gdlcode),
                          labels = unique(DATA$gdlcode))


levels((CELLS_NIG$gdlcode))
levels((DATA$gdlcode))

dim(CELLS_NIG)
dim(DATA)
dim(CELLS_NIG2)

CELLS_NIG2<-CELLS_NIG%>%left_join(DATA)


CENTROIDS<-st_centroid(sids)

str_split_fixed(CENTROIDS$geometry, pattern, n)

# shdi
CELLS_NIG3<-CELLS_NIG2%>%
    group_by(gdlcode)%>%
    summarise(latitude=mean(xcoordinates),
              longitude=mean(ycoordinates),
              shdi=mean(shdi),
              IQR=IQR(gpw_v4_population_count_rev11_2020_2pt5_min))

library(biscale)

SHAPES3<-SHAPES2%>%left_join(CELLS_NIG3,by=c("id"="gdlcode"))

SHAPES3%>%ggplot(aes(color=shdi,fill=IQR))+
    geom_sf()+
    theme_minimal()

COLORS <- bi_class(SHAPES3, x = shdi, y = IQR, style = "quantile", dim = 3)

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "shdi ",
                    ylab = "IQR ",
                    size = 8)

MAP<-ggplot() +
    geom_sf(data = COLORS, mapping = aes(fill = bi_class),
            color = "white", size = 0.1, show.legend = FALSE) +
    bi_scale_fill(pal = "DkBlue", dim = 3) +
    labs(
        title = "Subnational HDI by Population Density (IQR)"
    ) +
    bi_theme()+
    theme(text = element_text(size=12))

# library(cowplot)

final<-ggdraw() +
    draw_plot(MAP, 0, 0, 1, 1) +
    draw_plot(legend, 0.65, 0, 0.35, 0.35)

ggsave("out/FINALTHINGY.png",width = 7,height = 5)






