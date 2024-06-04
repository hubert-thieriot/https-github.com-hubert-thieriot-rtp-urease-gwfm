install.packages("FAOSTAT")
library(FAOSTAT)
library(tidyverse)

source('helpers.R')


data_folder <- "fao_raw"
dir.create(data_folder)
fao_metadata <- search_dataset()


# Population
View(fao_metadata %>%
       filter(grepl('popul', label, ignore.case = TRUE)))

population_raw <- get_faostat_bulk(code = "OA", data_folder = data_folder)
population <- population_raw %>%
  filter(element == "total_population___both_sexes",
         year==2022) %>%
  dplyr::select(area_code, population=value) %>%
  mutate(population=population * 1e3)

# Land use data
View(fao_metadata %>%
       filter(grepl('land', label, ignore.case = TRUE)))
landuse_raw <- get_faostat_bulk(code = "RL", data_folder = data_folder)

landuse <- landuse_raw %>%
  filter(item == "Cropland",
         element == "area") %>%
  group_by(area) %>%
  filter(year == max(year)) %>%
  ungroup()


# Fertiliser use
View(fao_metadata %>%
       filter(grepl('ferti', label, ignore.case = TRUE)))

View(fao_metadata %>%
       filter(grepl('ferti', label, ignore.case = TRUE)))


fertiliser_raw <- get_faostat_bulk(code = "RFB", data_folder = data_folder)
fertiliser_raw %>%
  filter(grepl("urea", item, ignore.case = TRUE)) %>%
  distinct(item, element, unit)

fertiliser <- fertiliser_raw %>%
  filter(item == "Urea", unit=="t") %>%
  group_by(area) %>%
  mutate(n_elements = n_distinct(element)) %>%
  group_by(area, year) %>%
  filter(n() == n_elements) %>%
  group_by(area) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(area, area_code, year, element, value) %>%
  tidyr::spread(element, value, fill=0) %>%
  mutate(area=clean_country(area)) %>%
  filter(!is.na(area)) %>%
  clipr::write_clip()


# Join the three
result <- landuse %>%
  select(area_code, area, cropland_kha=value) %>%
  left_join(population %>%
              select(area_code, population),
            by = "area_code") %>%
  left_join(fertiliser %>% select(-c(area, year)),
            by = "area_code") %>%
  mutate(urea_t_per_kha = urea_use_t / cropland_kha) %>%
  mutate(area = clean_country(area)) %>%
  filter(!is.na(area)) %>%
  select(-c(area_code)) %>%
  clipr::write_clip()


# Plot by country
