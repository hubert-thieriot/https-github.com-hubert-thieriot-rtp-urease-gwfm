# Extracting ammonia emissions by country from https://zenodo.org/records/3841723
# This is an average of 2010-2015 emissions
# Author: Hubert Thieriot hubert.thieriot@gmail.com

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(ncdf4)
library(rnaturalearth)
library(raster)
library(sf)
library(countrycode)


# Download ammonia emissions from https://zenodo.org/records/3841723
# Vira et al, (2019). An improved mechanistic model for ammonia volatilization in Earth system models:
# Flow of Agricultural Nitrogen, version 2 (FANv2). Geoscientific Model Development Discussions
url_ammonia <- "https://zenodo.org/records/3841723/files/nh3_emission_c20200524.nc?download=1"
temp_file <- tempfile(fileext = ".nc")
download.file(url_ammonia, temp_file)
ammonia <- nc_open(temp_file)
file.remove(temp_file)

countries <- ne_countries(scale = "medium", returnclass = "sf")

# Extract the NH3 emission data
ammonia <- nc_open("~/Downloads/nh3_emission_c20200524.nc")
nh3_emission <- ncvar_get(ammonia, "NH3_UREA")
fill_value <- ncatt_get(ammonia, "NH3_UREA", "_FillValue")$value
nh3_emission[nh3_emission == fill_value] <- NA

# Convert NH3 emission data to raster
time_steps <- dim(nh3_emission)[3]
rasters <- list()
for (t in 1:time_steps) {

  raster_data <- t(nh3_emission[,,t])
  # Reverse y dimension
  raster_data <- raster_data[nrow(raster_data):1,]
  rasters[[t]] <- raster(raster_data,
                         xmn = min(ncvar_get(ammonia, "x")),
                         xmx = max(ncvar_get(ammonia, "x")),
                         ymn = min(ncvar_get(ammonia, "y")),
                         ymx = max(ncvar_get(ammonia, "y")),
                         crs = CRS("+proj=longlat +datum=WGS84"))
}

# Average rasters over time
nh3_rate <- mean(stack(rasters), na.rm = TRUE)

# convert g/m^2/s -> kilotonne per year
cell_area <- area(nh3_sum_raster) * 1e6 # m2 to km2
nh3 <- nh3_rate * cell_area * 60 * 60 * 24 * 365 * 1e-9

# Perform zonal statistics to calculate the sum of NH3 emissions by country
country_nh3 <- extract(nh3, countries, fun = sum, na.rm = TRUE, df = TRUE, method='bilinear')
country_nh3 <- cbind(countries, nh3_kt_year = country_nh3[,2]) %>%
  as.data.frame()

# Export results
country_nh3 %>%
  # Keep top - 50
  arrange(desc(nh3_kt_year)) %>%
  head(50) %>%
  ggplot() +
  geom_bar(aes(nh3_kt_year, reorder(name, nh3_kt_year), fill=region_un), stat = "identity")

country_nh3 %>%
  dplyr::select(adm0_a3, nh3_kt_year) %>%
  mutate(country = countrycode(adm0_a3, "iso3c", "country.name")) %>%
  dplyr::select(country, nh3_kt_year) %>%
  clipr::write_clip()
