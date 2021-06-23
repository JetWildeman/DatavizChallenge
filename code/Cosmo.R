library(tidyverse)
library(sf)
library(wpp2019)
library(countrycode)

#to get shdi data
shapes <-
    readRDS("data/challenge_data.rds") %>%
    st_as_sf() %>%
    filter(year == 2019)

#to get population size at regional level
pop_size_reg <-
    readr::read_csv(file = "tmp/cell-gdlcode-2020-c.csv") %>%
    group_by(gdlcode) %>%
    summarise(pop_count = sum(gpw_v4_population_count_rev11_2020_2pt5_min))

#to average shdi at country level
avrg_shdi_cntr <-
    left_join(pop_size_reg, shapes %>% select(gdlcode, country, shdi) %>% st_drop_geometry(), id = "gdlcode") %>%
    group_by(country) %>%
    summarise(avrg_shdi = weighted.mean(shdi, pop_count, na.rm = T)) %>%
    rename(name = country)

#to get country level population
data(pop)
rm(popF, popM, popFT, popMT)

cntr <-
    readRDS("data/challenge_data.rds") %>%
    st_as_sf() %>%
    filter(year == 2019) %>%
    select(code = iso_code.x) %>%
    st_drop_geometry() %>%
    distinct() %>%
    filter(!code == "XKO")

pop20 <-
    pop %>%
    select(name, `2020`) %>%
    mutate(code = name %>% countryname(destination = "iso3c")) %>%
    filter(!is.na(code),
           !name == "Less developed regions, excluding China")

rm(pop)

data <-
    right_join(pop20, cntr, id = "code") %>%
    mutate(prop_pop = prop.table(`2020`)) %>%
    arrange(prop_pop) %>%
    mutate(
        x = prop_pop %>% seq_along,
        prop = prop_pop %>% cumsum
    )

data <-
    left_join(data, avrg_shdi_cntr, id = "name") %>%
    arrange(prop_pop)

shapes2 <-
    left_join(shapes, data %>% rename(country = name), by = "country") %>%
    arrange(prop_pop) %>%
    select(gdlcode, country, shdi, prop, prop_pop, x) %>%
    st_drop_geometry()

data %>%
    ggplot() +
    geom_step(aes(x = avrg_shdi, y = prop)) +
    geom_point(data = shapes2, aes(x = shdi, y = prop)) +
    theme_minimal()
+
    coord_flip()
