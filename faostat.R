# Collecting fertiliser use, import/export, land use and population data from FAOSTAT
#

install.packages("FAOSTAT")
library(FAOSTAT)
library(tidyverse)

source('helpers.R')

results_folder <- "results"
data_folder <- "fao_raw"

dir.create(data_folder, showWarnings = FALSE)
dir.create(result_folder, showWarnings = FALSE)

fao_metadata <- search_dataset()


# Population --------------------------------------------------------------
View(fao_metadata %>%
       filter(grepl('popul', label, ignore.case = TRUE)))

population_raw <- get_faostat_bulk(code = "OA", data_folder = data_folder)
population <- population_raw %>%
  filter(element == "total_population___both_sexes",
         year==2022) %>%
  dplyr::select(country=area, population=value) %>%
  mutate(population=population * 1e3) %>%
  mutate(country=clean_country(country)) %>%
  filter(!is.na(country)) %>%
  clipr::write_clip() %>%
  write_csv(file.path(results_folder, "population.csv"))


# Fertiliser use ----------------------------------------------------------
View(fao_metadata %>%
       filter(grepl('ferti', label, ignore.case = TRUE)))

fertiliser_raw <- get_faostat_bulk(code = "RFB", data_folder = data_folder)

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
  clipr::write_clip() %>%
  write_csv(file.path(results_folder, "fertiliser.csv"))


# Land use ----------------------------------------------------------------
View(fao_metadata %>%
       filter(grepl('land', label, ignore.case = TRUE)))

landuse_raw <- get_faostat_bulk(code = "RL", data_folder = data_folder)
landuse <- landuse_raw %>%
  filter(item == "Cropland",
         element == "area") %>%
  group_by(area) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(country=area, year, value) %>%
  mutate(country=clean_country(country)) %>%
  filter(!is.na(area)) %>%
  clipr::write_clip() %>%
  write_csv(file.path(results_folder, "land_use.csv"))

