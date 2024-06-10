# Extracting ammonia emissions by country from FANv2 model outputs: https://zenodo.org/records/3841723
# The model is described in Vira et al, (2019). An improved mechanistic model for ammonia volatilization in Earth system models:
# Flow of Agricultural Nitrogen, version 2 (FANv2). Geoscientific Model Development Discussions
#
# This is an average of 2010-2015 emissions
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
remotes::install_github('Mikata-Project/ggthemr')

library(tidyverse)
library(ncdf4)
library(rnaturalearth)
library(raster)
library(sf)
library(countrycode)
library(ggplot2)
library(ggthemr)

results_dir <- "results"
dir.create(results_dir, showWarnings = FALSE)

# Downlaod model output ---------------------------------------------------
url_ammonia <- "https://zenodo.org/records/3841723/files/nh3_emission_c20200524.nc"
temp_file <- tempfile(fileext = ".nc")
download.file(url_ammonia, temp_file)
nh3_emission_file <- nc_open(temp_file)

# Build ammonia emission raster -------------------------------------------
nh3_urea_var <- ncvar_get(nh3_emission_file, "NH3_UREA")
nh3_total_var <- ncvar_get(nh3_emission_file, "NH3_TOTAL")


var_to_country <- function(var, var_name){
  avg <- apply(var, c(1,2), mean, na.rm=T)
  raster <- t(avg[,])
  raster <- raster[nrow(raster):1,]
  raster <- raster(raster,
                    xmn = min(ncvar_get(nh3_emission_file, "x")),
                    xmx = max(ncvar_get(nh3_emission_file, "x")),
                    ymn = min(ncvar_get(nh3_emission_file, "y")),
                    ymx = max(ncvar_get(nh3_emission_file, "y")),
                    crs = CRS("+proj=longlat +datum=WGS84"))

  cell_area <- area(raster) * 1e6 # m2 to km2
  raster <- raster * cell_area * 60 * 60 * 24 * 365 * 1e-9

  countries <- ne_countries(scale = "medium", returnclass = "sf")
  country_nh3 <- raster::extract(raster, countries, fun = sum, na.rm = TRUE, df = TRUE, method='bilinear')

  result <- cbind(countries,
                  value = country_nh3[,2]) %>%
    as.data.frame() %>%
    select(iso3=iso_a3, value, region_un) %>%
    mutate(country = countrycode(iso3, "iso3c", "country.name"),
           unit = "kt/year") %>%
    select(country, iso3, region_un, unit, value) %>%
    filter(!is.na(country))

  # rename value to var_name
  names(result)[which(names(result) == "value")] <- var_name
  result
}


# Average across time
nh3_urea <- var_to_country(nh3_urea_var, "nh3_urea")
nh3_total <- var_to_country(nh3_total_var, "nh3_total")

nh3 <- inner_join(nh3_urea, nh3_total, by=c("country", "iso3", "region_un", "unit")) %>%
  mutate(share_nh3_from_urea = nh3_urea / nh3_total)

# Quick sanity check
sum(nh3$nh3_total, na.rm=T) # 44.6Mt/year (article has 48 Tg/year = 48 Mt/year for all fertilisers which seems to match)



# Export ------------------------------------------------------------------
ggthemr::ggthemr("dust")

nh3 %>%
  arrange(desc(nh3_urea)) %>%
  head(20) %>%
  ggplot() +
  geom_bar(aes(nh3_urea, reorder(country, nh3_urea), fill=region_un), stat = "identity") +
  geom_text(aes(nh3_urea, reorder(country, nh3_urea), label=round(nh3_urea)), hjust=-0.2, nudge_x = 0.8, size=3) +
  scale_x_continuous(labels = scales::comma, limits=c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Top 20 countries by ammonia emissions from urea",
       subtitle = "Average 2010-2015 emissions from FANv2 model in kt/year",
       x = "Ammonia emissions (kt/year)",
       y = NULL,
       fill="Region",
       caption = paste0(c("Source: Vira et al, (2019). An improved mechanistic model for ammonia volatilization in Earth system models:",
                          "Flow of Agricultural Nitrogen, version 2 (FANv2). Geoscientific Model Development Discussions"),
                          collapse="\n"))

ggsave(file.path(results_dir, "nh3_emissions_top20.png"), width=8, height=6, scale=1.2)


nh3 %>%
  arrange(desc(share_nh3_from_urea)) %>%
  head(20) %>%
  ggplot() +
  geom_bar(aes(share_nh3_from_urea, reorder(country, share_nh3_from_urea), fill=region_un), stat = "identity") +
  geom_text(aes(share_nh3_from_urea, reorder(country, share_nh3_from_urea), label=scales::percent(share_nh3_from_urea, 1)), hjust=-0.2, size=3) +
  scale_x_continuous(labels = scales::percent, limits=c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Top 20 countries by share of ammonia emissions from urea",
       subtitle = "Average 2010-2015 emissions from FANv2 model in kt/year",
       x = "Share of ammonia emissions from urea (%)",
       y = NULL,
       fill="Region",
       caption = paste0(c("Source: Vira et al, (2019). An improved mechanistic model for ammonia volatilization in Earth system models:",
                          "Flow of Agricultural Nitrogen, version 2 (FANv2). Geoscientific Model Development Discussions"),
                        collapse="\n"))

ggsave(file.path(results_dir, "nh3_share_emissions_top20.png"), width=8, height=6, scale=1.2)

nh3 %>%
  clipr::write_clip() %>%
  write_csv(file.path(results_dir, "nh3_emissions.csv"))
