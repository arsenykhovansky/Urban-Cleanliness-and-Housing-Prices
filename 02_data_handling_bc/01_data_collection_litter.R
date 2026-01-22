# ==============================================================================
# LITTER INDEX — Block-Level 

# GOAL: Datatable of blocks with key selection characteristics per block
# this file: Litter Index

# GEOID = block id (primary key)


# OUTPUTS:
#   - litter.csv

# TABLE OF CONTENTS
#   0) Packages
#   1) Load inputs
#   2) Define score year (2018 preferred; 2017 fallback)
#   3) Block boundary ring & intersections
#       3.1) Build rings along block boundaries
#       3.2) Intersect segments with rings and interiors
#       3.3) Collapse to unique (GEOID, seg_id)
#   4) Length-weighted litter index per block
#   5) Fill missing/zero values from block-group means
#   6) Export
# ==============================================================================


# 0) Packages -------------------------------------------------------------------
# install helper (if needed)
# if (!requireNamespace("tidycensus", quietly=TRUE))     install.packages("digest")

library(sf)
library(dplyr)
library(tidyr)      
library(readr)      

sf_use_s2(FALSE)  # planar ops in EPSG:26918


# 1) Load inputs ----------------------------------------------------------------
#    - Litter Index segments (2017/2018)
#    - 2010 Census blocks
#    - 2010 Census block groups

# 1.1) Litter Index segments (2017/2018)
# https://opendataphilly.org/datasets/litter-index/
li <- st_read(
  "Litter_Index_Blocks.geojson"
  ) %>%
  dplyr::select(seg_id = SEG_ID, score = HUNDRED_BLOCK_SCORE, year = YEAR) %>%
  st_transform(26918)

# 1.2) 2010 blocks
# https://opendataphilly.org/datasets/census-blocks/  
blocks10 <- st_read(
  "Census_Blocks_2010.geojson"
) %>%
  dplyr::select(GEOID = GEOID10) %>%
  st_transform(26918)

# 1.3) 2010 block groups
# https://opendataphilly.org/datasets/census-block-groups/
groups10 <- st_read(
  "Census_Block_Groups_2010.geojson"
) %>%
  dplyr::select(GEOID = GEOID10) %>%
  st_transform(26918)


# 2) Define score year (2018 preferred; 2017 fallback) -------------------------
#    Treat 0 as “no score”

li <- li %>%
  tidyr::pivot_wider(
    names_from  = year,
    values_from = score,
    names_prefix = "score_"
  ) %>%
  mutate(
    score_2018 = dplyr::na_if(score_2018, 0),
    score_2017 = dplyr::na_if(score_2017, 0),
    score      = dplyr::coalesce(score_2018, score_2017)
  ) %>%
  dplyr::select(seg_id, score, geometry)

# Simple NA check
sum(is.na(li$score))  # 993


# 3) Block boundary ring & intersections ---------------------------------------
#    - Create narrow rings along block boundaries
#    - Intersect segments with rings (edges) and with block interiors

# 3.1) Build rings along block boundaries (meters)
bnd       <- st_boundary(blocks10)
ring_geom <- st_buffer(st_geometry(bnd), 15)    # 15 m ring
ring      <- st_sf(GEOID = blocks10$GEOID, geometry = ring_geom)

# 3.2) Intersections: edges + true interior
edge <- st_intersection(li, ring) %>%
  mutate(len = as.numeric(st_length(geometry)))

inside <- st_intersection(li, blocks10) %>%
  mutate(len = as.numeric(st_length(geometry)))

# 3.3) Combine, de-NA, and compress per (GEOID, seg_id)
pieces <- bind_rows(edge, inside) %>%
  st_drop_geometry() %>%
  filter(!is.na(score), score >= 1, len > 0) %>%
  group_by(GEOID, seg_id) %>%
  summarise(
    score_len = sum(score * len),
    len       = sum(len),
    .groups   = "drop"
  )


# 4) Length-weighted litter index per block ------------------------------------

li_block <- pieces %>%
  group_by(GEOID) %>%
  summarise(
    litter_index = sum(score_len) / sum(len),
    .groups      = "drop"
  )

# Sanity checks
inherits(ring, "sf")                     # TRUE
sum(is.na(edge$GEOID))                   # 0
min(li_block$litter_index, na.rm = TRUE) # >= 1


# 5) Fill missing/zero values from block-group means ---------------------------

li_block <- blocks10 %>%
  st_drop_geometry() %>%
  transmute(
    GEOID,
    BG = substr(GEOID, 1, 12)
  ) %>%
  left_join(li_block, by = "GEOID") %>%
  group_by(BG) %>%
  mutate(
    bg_mean      = mean(na_if(litter_index, 0), na.rm = TRUE),
    litter_index = coalesce(na_if(litter_index, 0), bg_mean)
  ) %>%
  ungroup() %>%
  dplyr::select(GEOID, litter_index)

# Final NA check
sum(is.na(li_block$litter_index))  # 0


# 6) Export (CSV) -------------------------------------------------------------
write.csv(
  li_block,
  "litter.csv",
  row.names = FALSE
)



