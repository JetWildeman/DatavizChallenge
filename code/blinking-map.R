#===============================================================================
# 2021-06-23 -- Rostock Retreat --  challenge
# Blinking color animated map
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(cowplot)
library(sf)
library(rmapshaper)
library(countrycode)
library(gganimate)

library(prismatic)
library(ggdark)
library(patchwork)
library(paletteer)
library(hrbrthemes)



load("data/data-simplified-robinson-prj.rda")

# subset only needed data
df19 <- df %>%
    filter(year == 2019) %>%
    transmute(
        id = gdlcode,
        code = iso_code.x,
        shdi
    )

df19 %>%
    ggplot()+
    geom_sf(aes(fill = shdi), color = NA)+
    geom_sf(data = bord, color = "#eaeaea", size = .5)+
    scale_fill_viridis_c(direction = -1)+
    theme_map()+
    theme(legend.position = "top")



# alternative world polygons ----------------------------------------------
# get world map outline (you might need to install the package)
world_outline <- spData::world %>%
    st_as_sf()

# let's use a fancy projection
world_outline_robinson <- world_outline %>%
    st_transform(crs = "ESRI:54030") %>%
    filter(!iso_a2 == "AQ") %>%
    transmute(
        code = iso_a2 %>%
            countrycode(origin = "iso2c", destination = "iso3c"),
        pop05 = pop,
        geometry = geom
    )

# produce borders layer
country_borders <- world_outline_robinson %>%
    rmapshaper::ms_innerlines()


# expand the data to blink ------------------------------------------------

blink <- df19 %>%
    st_drop_geometry() %>%
    # classify in 20 categories
    mutate(
        shdi20 = shdi %>% cut_number(20) %>%
            lvls_revalue(1:20 %>% paste)
    ) %>%
    group_by(code) %>%
    summarise(sampled_shdi = shdi20 %>% sample(size = 100, replace = T)) %>%
    mutate(aaa = sampled_shdi %>% seq_along()%>% lubridate::as_date()) %>%
    ungroup()


p <- world_outline_robinson %>%
    left_join(blink, "code") %>%
    # filter(aaa == 9) %>%
    ggplot()+
    geom_sf(aes(fill = sampled_shdi), color = NA)+
    geom_sf(data = country_borders, color = "#eaeaea", size = .5)+
    scale_fill_viridis_d(
        direction = -1,
        guide = guide_legend(
            keywidth = unit(1, "lines"),
            keyheight = unit(2, "lines"),
            title.position = "top",
            nrow = 1,
            label.position = "bottom"

        )
    )+
    theme_map()+
    theme(legend.position = "bottom")+
    labs(
        fill = "SHDI of a randomply sampled region"
    )+
    transition_manual(aaa)


animate(
    p,
    nframes = 100,
    width = 1600,
    height = 1200,
    res = 200,
    start_pause = 3,
    end_pause = 10
)

anim_save("out/test-anim.gif")
