# ==============================================================================
# BLOCK-LEVEL LAND USE — Residential, Commercial, Vacant

# GOAL: data frame of blocks with key selection characteristics per block/ block group
# this script: land use per block (residential, commercial, vacant)

# GEOID = block id (primary key)

# OUTPUTS:
#   - land_shares.csv

# TABLE OF CONTENTS
#   0) Packages
#   1) Data Import
#   2) Land-use bucketing and overlays
#   3) Block-level land shares
#   4) Export
# ==============================================================================


# 0) Packages -------------------------------------------------------------------
# package installation helper (if needed)
# if (!requireNamespace("esri2sf", quietly=TRUE))     install.packages("esri2sf")

library(sf)
library(lwgeom)
library(dplyr)


# 1) Data Import ----------------------------------------------------------------

# 1.1) blocks
# https://opendataphilly.org/datasets/census-blocks/  
blocks10 <- st_read(
  "Census_Blocks_2010.geojson"
) %>%
  dplyr::select(GEOID = GEOID10) %>%
  st_transform(2272) %>%
  st_make_valid()

# 1.2) land use
# https://opendataphilly.org/datasets/land-use/ 
shp_files <- list.files(
  "Land_Use_2012_2018",
  pattern = "\\.shp$", full.names = TRUE, recursive = TRUE
)
lu <- st_read(shp_files[1], quiet = FALSE) %>%
  st_transform(2272) %>%
  st_make_valid()


# 2) Land-use bucketing and overlay --------------------------------------------

# 2.1) Map to buckets (res/com by major class; vacant = 91 = vacant land)
lu <- lu %>%
  filter(!(C_DIG1 %in% c(5, 8))) %>%  # exclude water and transport
  mutate(
    bucket = case_when(
      C_DIG1 == 1  ~ "res",
      C_DIG1 == 2  ~ "com",
      C_DIG2 == 91 ~ "vac",
      TRUE         ~ "other"
    )
  ) %>%
  dplyr::select(bucket)

# 2.2) Overlay + areas
areas <- st_intersection(lu["bucket"], blocks10["GEOID"]) %>%
  mutate(area = as.numeric(st_area(.))) %>%
  st_drop_geometry()


# 3) Block-level land shares ----------------------------------------------------

# 3.1) Shares per block (denominator = ALL parcel area in block) 
land_shares <- areas %>%
  group_by(GEOID) %>%
  summarise(
    total     = sum(area),
    res_share = sum(area[bucket == "res"]) / total,
    com_share = sum(area[bucket == "com"]) / total,
    vac_share = sum(area[bucket == "vac"]) / total,
    no_vacant = sum(area[bucket == "vac"]) == 0,
    .groups   = "drop"
  ) %>%
  dplyr::select(GEOID, res_share, com_share, vac_share, no_vacant)

# 3.2) sanity check
colMeans(land_shares[, c("res_share", "com_share", "vac_share", "no_vacant")], na.rm = TRUE)


# 4) Export ---------------------------------------------------------------------
# sum(is.na(shares_block$res_share)) 0 nas

write.csv(
  land_shares,
  "land_shares.csv",
  row.names = FALSE
)


