library(tidyverse)
remotes::install_github("ropensci/comtradr")
library(comtradr)
library(summarytools)

readRenviron(".Renviron")
HSCODE_UREA <- "310210"


# Get data
comtrade_data <- comtradr::ct_get_data(
  commodity_code = HSCODE_UREA,
  start_date = 2022,
  end_date = 2022
)

# Check data
summarytools::dfSummary(comtrade_data)

# Format for GWFM
comtrade_data %>%
  group_by(period, iso3=reporter_iso, country=reporter_desc, flow=flow_desc) %>%
  summarise(net_wgt=sum(net_wgt)) %>%
  tidyr::spread(flow, net_wgt, fill = 0) %>%
  mutate(net_export = Export + `Re-export` - Import - `Re-import`) %>%
  ungroup() %>%
  select(country, net_export) %>%
  mutate(unit = 'kg', hs_code=HSCODE_UREA) %>%
  arrange(desc(net_export)) %>%
  clipr::write_clip()
