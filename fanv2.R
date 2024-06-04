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
nh3_emission <- ncvar_get(nh3_emission_file, "NH3_UREA")

# Average across time
nh3_emission_avg <- apply(nh3_emission, c(1,2), mean, na.rm=T)
# Transpose and reverse data
raster_data <- t(nh3_emission_avg[,])
raster_data <- raster_data[nrow(raster_data):1,]
nh3_raster <- raster(raster_data,
                         xmn = min(ncvar_get(nh3_emission_file, "x")),
                         xmx = max(ncvar_get(nh3_emission_file, "x")),
                         ymn = min(ncvar_get(nh3_emission_file, "y")),
                         ymx = max(ncvar_get(nh3_emission_file, "y")),
                         crs = CRS("+proj=longlat +datum=WGS84"))


# convert g/m^2/s -> kilotonne per year
cell_area <- area(nh3_raster) * 1e6 # m2 to km2
nh3 <- nh3_rate * cell_area * 60 * 60 * 24 * 365 * 1e-9

# Quick sanity check
sum(nh3[], na.rm=T) # 8Mt/year (article has 11 Tg/year = 11 Mt/year for all fertilisers which seems to match)


# Extract by country ------------------------------------------------------
countries <- ne_countries(scale = "medium", returnclass = "sf")
country_nh3 <- raster::extract(nh3, countries, fun = sum, na.rm = TRUE, df = TRUE, method='bilinear')
country_nh3 <- cbind(countries, nh3_kt_year = country_nh3[,2]) %>%
  as.data.frame()



# Export ------------------------------------------------------------------
ggthemr::ggthemr("dust")

country_nh3 %>%
  arrange(desc(nh3_kt_year)) %>%
  head(20) %>%
  ggplot() +
  geom_bar(aes(nh3_kt_year, reorder(name, nh3_kt_year), fill=region_un), stat = "identity") +
  geom_text(aes(nh3_kt_year, reorder(name, nh3_kt_year), label=round(nh3_kt_year)), hjust=-0.2, nudge_x = 0.8, size=3) +
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

country_nh3 %>%
  dplyr::select(adm0_a3, nh3_kt_year) %>%
  mutate(country = countrycode(adm0_a3, "iso3c", "country.name")) %>%
  dplyr::select(country, nh3_kt_year) %>%
  clipr::write_clip() %>%
  write_csv(file.path(results_dir, "nh3_emissions.csv"))
