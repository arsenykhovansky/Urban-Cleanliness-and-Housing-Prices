# ==============================================================================
# BLOCK CHARACTERISTICS — Unified Dataset

# GOAL: produce one dataset with all block-level characteristics

# KEY: GEOID (Census 2010 block)

# OUTPUTS:
#   - block_characteristics.csv

# TABLE OF CONTENTS
#   0) Packages
#   1) Load Geographies (blocks, block groups)
#   2) Load Inputs (census, litter, land shares, crime)
#   3) Join → sf (align to census IDs and attach geometry)
#   4) NA Handling & Filters
#   5) Block-Group Averages (append *_bg means for non-*_bg numerics)
#   6) Export
# ==============================================================================


# 0) Packages -------------------------------------------------------------------
# install helper (if needed)
# if (!requireNamespace("tidycensus", quietly=TRUE))     install.packages("digest")

library(sf)
library(dplyr)
library(tidyr)
library(readr)


# 1) Load Geographies -----------------------------------------------------------

# 1.1) 2010 blocks (geometry target)
# https://opendataphilly.org/datasets/census-blocks/  
blocks10 <- st_read(
  "Census_Blocks_2010.geojson",
  layer = "b84176c7-6f02-4a33-9054-d76f7c136cfb2020329-1-flphyh.9ftec"
) %>%
  dplyr::select(GEOID = GEOID10) %>%
  st_transform(26918)

# 1.2) 2010 block groups
# https://opendataphilly.org/datasets/census-block-groups/
groups10 <- st_read(
  "Census_Block_Groups_2010.geojson",
  layer = "Census_Block_Groups_2010"
) %>%
  dplyr::select(GEOID = GEOID10) %>%
  st_transform(26918)


# 2) Load Inputs ----------------------------------------------------------------

# 2.1) Read block-level inputs (all must have block-level GEOID as character)
census <- read_csv(
  "census_data.csv",
  col_types = cols(GEOID = col_character())
)
litter <- read_csv(
  "litter.csv",
  col_types = cols(GEOID = col_character())
)
land   <- read_csv(
  "land_shares.csv",
  col_types = cols(GEOID = col_character())
)
crime  <- read_csv(
  "crime.csv",
  col_types = cols(GEOID = col_character())
)

# 2.2) Quick diagnostic (optional)
land <- land %>% mutate(total = res_share + com_share + vac_share)
sum(land$total == 0) # 1761


# 3) Join → sf (align to census IDs and attach geometry) ------------------------

# 3.1) Keep only rows whose GEOID exists in census, then join TO census
ids <- as.character(census$GEOID)

census_merged <- census %>%
  mutate(GEOID = as.character(GEOID)) %>%
  left_join(
    litter %>%
      mutate(GEOID = as.character(GEOID)) %>%
      filter(GEOID %in% ids),
    by = "GEOID"
  ) %>%
  left_join(
    land %>%
      mutate(GEOID = as.character(GEOID)) %>%
      filter(GEOID %in% ids),
    by = "GEOID"
  ) %>%
  left_join(
    crime %>%
      mutate(GEOID = as.character(GEOID)) %>%
      filter(GEOID %in% ids),
    by = "GEOID"
  ) %>%
  left_join(
    blocks10 %>%
      mutate(GEOID = as.character(GEOID)) %>%
      filter(GEOID %in% ids),
    by = "GEOID"
  ) %>%
  # 3.2) Tibble -> sf using the geometry column name from blocks10
  st_as_sf(sf_column_name = attr(blocks10, "sf_column"), crs = st_crs(blocks10))


# 4) NA Handling & Filters ------------------------------------------------------

# 4.1) Inspect missingness in land shares
sum(is.na(census_merged$vac_share))
# (res_share, com_share, vac_share each have ~65 NAs)

# 4.2) Drop rows with any NA across current columns (drops ~65)
census_merged <- census_merged %>% drop_na()

# 4.3) Keep blocks with positive residential share
census_merged <- census_merged %>% filter(res_share != 0)


# 5) Block-Group Averages (append *_bg means for non-*_bg numerics) ------------

# Preserve geometry column name
gcol <- attr(census_merged, "sf_column")

# 5.1) Extract block-group ID
census_merged <- census_merged %>%
  mutate(BG = substr(GEOID, 1, 12))

census_merged <- census_merged %>%
  mutate(no_vacant = as.numeric(no_vacant))

# 5.2) Numeric columns to aggregate (exclude existing *_bg)
num_vars <- census_merged %>%
  st_drop_geometry() %>%
  dplyr::select(where(is.numeric)) %>%
  names()
compute_vars <- num_vars[!grepl("_bg$", num_vars)]

# 5.3) Compute BG means for those variables only
bg_means <- census_merged %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(BG) %>%
  dplyr::summarize(
    across(all_of(compute_vars), \(x) mean(x, na.rm = TRUE))
  )

# 5.4) Suffix new BG means with "_bg" and join back
bg_means_ren <- bg_means %>%
  rename_with(~ paste0(.x, "_bg"), all_of(compute_vars))

census_merged <- census_merged %>%
  left_join(bg_means_ren, by = "BG") %>%
  dplyr::select(-BG)

# Restore sf geometry attribute (defensive)
attr(census_merged, "sf_column") <- gcol



# 6) Export --------------------------------------------------------------------
# CSV
write.csv(
  census_merged %>% st_drop_geometry(),
  "block_characteristics.csv",
  row.names = FALSE
)


