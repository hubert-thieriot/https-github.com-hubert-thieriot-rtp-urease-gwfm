# Extracting number of relevant regulations from https://www.nature.com/articles/s41893-020-0577-7#Sec18
# The authors of the paper stored the data in a Google Sheet: https://docs.google.com/spreadsheets/d/1hOfl5Np80oC4EXrNMi7emnhx3RByRFSvOfEr9f2GJC4/edit#gid=0

library(tidyverse)

# Load the data
url <- "https://docs.google.com/spreadsheets/d/1hOfl5Np80oC4EXrNMi7emnhx3RByRFSvOfEr9f2GJC4/export?format=csv"
regulations <- readr::read_csv(url)
names(regulations)

count <- regulations %>%
  dplyr::select(country=Country,
           descriptors=Descriptors,
           sector=Sector,
           scale=Scale) %>%
  filter(grepl("fertili", descriptors, ignore.case = TRUE),
         grepl("national", scale, ignore.case = TRUE),
         grepl("agriculture", sector, ignore.case = TRUE)) %>%
  group_by(country) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

# Export
count %>%
  head(50) %>%
  ggplot() +
  geom_bar(aes(count, reorder(country, count)), stat = "identity")


count %>%
  dplyr::select(country, n_regulations=count) %>%
  clipr::write_clip()
