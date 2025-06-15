library(arrow)
library(tzdb)
# 1. Base de PRONAF
Mun_prod <- arrow::read_parquet("data/mun_pronaf.parquet")

save(Mun_prod, file = "data/pronaf_clean.RData", compress = "xz")

# Get regions (level 1)
regions_sf <- geobr::read_region(year = 2020) %>%
  mutate(join_1_2 = as.character(code_region))

# Get states (level 2)
states_sf <- geobr::read_state(year = 2020) %>%
  mutate(
    join_1_2 = as.character(code_region),
    join_2_3 = as.character(code_state),
    abbrev_state = as.character(abbrev_state)  # Ensure character type
  )

# Get municipalities (level 3)
municipalities_sf <- geobr::read_municipality(year = 2020) %>%
  mutate(
    join_2_3 = as.character(code_state),
    name_muni = as.character(name_muni)
  )

# Convert to Spatial
spdfs_list <- list(
  as(regions_sf, "Spatial"),
  as(states_sf, "Spatial"),
  as(municipalities_sf, "Spatial")
)

save(spdfs_list, file = "data/geo_data.RData")


