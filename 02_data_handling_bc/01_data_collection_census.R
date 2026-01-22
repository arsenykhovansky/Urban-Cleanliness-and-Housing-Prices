# ==============================================================================
# BLOCK-LEVEL CENSUS CHARACTERISTICS — Selection Controls

# GOAL: data frame of blocks with key selection characteristics per block/ block group
# this script: census data (vacancy, income, tenure, education, uninsured)

# GEOID = block id (primary key)

# OUTPUTS:
#   - census_data.csv

# TABLE OF CONTENTS
#   0) Packages
#   1) Blocks and Block Groups
#   2) Census data (Decennial, ACS, Planning Database)
#       2.1) API key & geography
#       2.2) Decennial 2020 PL (block): vacancy
#       2.3) ACS 2015–2019 (BG): income, tenure, education
#        .
#       2.8) PDB 2021 (BG): percent uninsured → blocks
#   3) Finalising census data
#   4) Export
# ==============================================================================


# 0) Packages -------------------------------------------------------------------
# package installation helper (if needed)
# if (!requireNamespace("tigris", quietly=TRUE))     install.packages("tigris")

library(sf)
library(dplyr)
library(tidyr)
library(tidycensus)
library(jsonlite)
library(tibble)     

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)


# 1) Blocks and Block Groups ----------------------------------------------------

# 1.1) census blocks
# census blocks 2010
# https://opendataphilly.org/datasets/census-blocks/  
blocks10 <- st_read("Census_Blocks_2010.geojson")

# create block group ids for each block
blocks10 <- blocks10 %>%
  mutate(
    GEOID = as.character(GEOID10),
    bg_geoid = substr(GEOID10, 1, 12)) %>% 
  dplyr::select(
    GEOID, 
    bg_geoid, 
    tract = TRACTCE10
  )

# census blocks 2020
blocks20 <- st_read("Census_Blocks_2020.geojson")
# https://data-phl.opendata.arcgis.com/datasets/phl::census-blocks-2020/about 

# create block group ids for each block
blocks20 <- blocks20 %>%
  mutate(
    GEOID = as.character(GEOID20),
    bg_geoid = substr(GEOID20, 1, 12)) %>% 
  dplyr::select(
    GEOID, 
    bg_geoid, 
    tract = TRACTCE20
  )


# 1.2) census block groups 
# block groups 2010 - https://opendataphilly.org/datasets/census-block-groups/ 
bg10 <- st_read(
  "Census_Block_Groups_2010.geojson",
  layer = "Census_Block_Groups_2010"
) %>%
  dplyr::select(
    bg_geoid = GEOID10,
    tract    = TRACTCE10
  )

# block groups 2020 - https://geo.btaa.org/catalog/288a35733ac14e8f9386cc112b79df46_0 
bg20 <- st_read(
  "Census_Block_Groups_2020.geojson",
  layer = "Census_Block_Groups_2020"
) %>%
  dplyr::select(
    bg_geoid = GEOID,
    tract    = TRACTCE
  )

# 1.3) Make sure CRS matches for spatial ops
bg10     <- st_transform(bg10,     st_crs(blocks20))
blocks10 <- st_transform(blocks10, st_crs(blocks20))


# 2) Census data (Decennial, American Community Survey, Planning Database) -----

# 2.1) API key & geography
# ----  API key (get one: https://api.census.gov/data/key_signup.html)
census_api_key("43ba9e59903aaa6591f92c033f395f1a8f4eded4", install = FALSE)

state_fips  <- "42"   # Pennsylvania
county_fips <- "101"  # Philadelphia County


# -----------------------------------------------------------
# 2.2) DECENNIAL 2020 PL (BLOCK): Black % and Vacancy rate
#      Docs: https://api.census.gov/data/2020/dec/pl.html
# -----------------------------------------------------------
# aggregate to block group, then assign to block
#
# Variables:
#   P1_001N = total population
#   P1_004N = Black or African American alone
#   H1_001N = total housing units
#   H1_003N = vacant housing units

# geometry api failed - so make use of the external census block shape files
# Pull 2020 PL counts for blocks (no geom)
dec_20 <- get_decennial(
  year      = 2020,
  dataset   = "pl",
  variables = c(
    "P1_001N", "P1_004N",    # total pop; Black alone
    "H1_001N", "H1_003N"     # total units; vacant units
  ),
  geography = "block",
  state     = state_fips,
  county    = county_fips,
  output    = "wide",
  geometry  = FALSE
) %>%
  rename(
    pop_tot_block    = P1_001N,
    black_tot_block  = P1_004N,
    units_tot_block  = H1_001N,
    vacant_tot_block = H1_003N
  ) %>%
  filter(pop_tot_block != 0) %>%
  filter(!is.na(pop_tot_block))

# Attach PL counts to 2020 block polygons
blocks20_counts <- blocks20 %>%
  dplyr::select(GEOID, geometry) %>%
  left_join(dec_20, by = "GEOID") %>%
  dplyr::filter(!is.na(pop_tot_block) | !is.na(units_tot_block))

# One point per 2020 block (inside the polygon) and put both layers in same CRS
b10  <- st_transform(blocks10, 5070)
b20p <- blocks20_counts %>%
  st_transform(5070) %>%
  mutate(geometry = st_point_on_surface(geometry)) %>%  
  st_as_sf()

# For each 2020 block point, find the containing 2010 block polygon
b20_to_b10 <- st_join(
  b20p,
  b10 %>% dplyr::select(GEOID10 = GEOID),   # keep only the 2010 block id
  join = st_within,
  left = TRUE
)

# Aggregate 2020 counts to 2010 blocks, then compute shares on the 2010 geometry
decennial_blocks <- b20_to_b10 %>%
  st_drop_geometry() %>%
  dplyr::filter(!is.na(GEOID10)) %>%
  group_by(GEOID = GEOID10) %>%
  summarise(
    pop_total_2010    = sum(pop_tot_block,    na.rm = TRUE),
    black_total_2010  = sum(black_tot_block,  na.rm = TRUE),
    units_total_2010  = sum(units_tot_block,  na.rm = TRUE),
    vacant_total_2010 = sum(vacant_tot_block, na.rm = TRUE),
    .groups           = "drop"
  ) %>%
  mutate(
    black_share  = if_else(pop_total_2010   > 0, black_total_2010  / pop_total_2010,   NA_real_),
    vacancy_rate = if_else(units_total_2010 > 0, vacant_total_2010 / units_total_2010, NA_real_)
  ) %>%
  dplyr::select(GEOID, black_share, vacancy_rate)  # <- 2010 block IDs with decennial metrics

# sum(is.na(decennial_blocks$black_share))   # 0
# sum(is.na(decennial_blocks$vacancy_rate))  # 70
# sum(dec_20$pop_tot_block == 0)             # 0

decennial_blocks <- decennial_blocks %>% drop_na()


# -----------------------------------------------------------
# 2.3) ACS 2015-2019 (BLOCK GROUP): income, tenure, education (+ BLOCK)
#      Docs: https://api.census.gov/data/2019/acs/acs5.html
# -----------------------------------------------------------
# Variables:
#   B19013_001E = median household income in the past 12 months (estimate)
#   B25003_001E = occupied housing units (tenure base) - occupied total
#   B25003_002E = owner-occupied
#   B15003_001E = population 25+ (education base)
#   B15003_022E..025E = Bachelor's, Master's, Professional, Doctorate degrees
#   B23025_003E; B23025_005E = labour force, unemployed

# Block-group ACS (2015–2019)
acs_bg <- get_acs(
  year       = 2019,              # 2015-2019 5-year ACS
  survey     = "acs5",
  geography  = "block group",
  state      = state_fips,
  county     = county_fips,
  variables  = c(
    "B01003_001",                 # total population
    "B19013_001",                 # median household income
    "B25003_001", "B25003_002",   # tenure: total occupied, owner-occupied
    "B15003_001", "B15003_022", "B15003_023", "B15003_024", "B15003_025",  # education
    "B23025_003", "B23025_005"    # labor force, unemployed
  ),
  output     = "wide",
  geometry   = FALSE
) %>%
  mutate(
    homeownership_rate = if_else(B25003_001E > 0, B25003_002E / B25003_001E, NA_real_),
    ba_plus_share      = if_else(
      B15003_001E > 0,
      (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E,
      NA_real_
    ),
    unemployment_rate  = if_else(B23025_003E > 0, B23025_005E / B23025_003E, NA_real_),
    median_income_bg   = B19013_001E,
    population_bg      = B01003_001E,
    tract_geoid        = substr(GEOID, 1, 11)
  ) %>%
  dplyr::select(
    bg_geoid = GEOID, tract_geoid,
    homeownership_rate, ba_plus_share, median_income_bg, population_bg,
    unemployment_rate
  )

# sum(is.na(acs_bg$homeownership_rate))
# sum(is.na(acs_bg$ba_plus_share))
# sum(is.na(acs_bg$median_income_bg))


# 2.4) Tract-level ACS (2015–2019) for income backfill 
# some census block groups (164) have nas for median income (e.g. when margins of error 
# produce unrealistic estimates). those are filled by the median income counts per tract
acs_tract_income <- get_acs(
  year       = 2019,
  survey     = "acs5",
  geography  = "tract",
  state      = state_fips,
  county     = county_fips,
  variables  = c("B19013_001"),   # median household income
  output     = "wide",
  geometry   = FALSE
) %>%
  transmute(
    tract_geoid         = GEOID,
    median_income_tract = B19013_001E
  )
# 10 tracts have nas in median income
# those are basically parks, airports, industrial zones - drop them


# 2.5) Join & fill; create flag 
acs_bg <- acs_bg %>%
  left_join(acs_tract_income, by = "tract_geoid") %>%
  mutate(
    inc_imputed   = as.integer(is.na(median_income_bg)),           # 1 if BG was NA before fill
    median_income = coalesce(median_income_bg, median_income_tract)
  ) %>%
  dplyr::select(
    bg_geoid,                         # block-group ID
    homeownership_rate, ba_plus_share, unemployment_rate,
    median_income, inc_imputed,       # filled income + flag
    population_bg
  ) %>%
  filter(
    population_bg > 5
  )
# two block groups have nas for median income
# sum(acs_bg$inc_imputed)
# # 156 block groups did not have median income


# 2.6) attach block group estimate to each block 
acs_blocks <- blocks10 %>%
  left_join(acs_bg, by = "bg_geoid")

# handling nas
{
# in addition to two block groups with nas, ~600 blocks have nas 
# those are unpopulated blocks in industrial areas etc. (drop those)

# acs and decennial does not keep those blocks but blocks dataset does, so the resulting 
# join contains those nas
# those come from non- residential areas - safe to drop them
}

acs_blocks <- acs_blocks %>% drop_na(population_bg)
# nas: median income- 113, ba_plus_share - 21, homeownership 38


# 2.7) attach _bg to know those are block bg level estimates
acs_blocks <- acs_blocks %>%
  rename_with(~ paste0(.x, "_bg"), where(is.numeric)) 


# -----------------------------------------------------------
# 2.8) PDB 2021 (BLOCK GROUP): percent uninsured (ACS 2015–2019)
#      -> compute uninsured_share and assign to 2010 blocks
#      Docs & vars:
#        - 2021 PDB uses ACS 2015–2019, 2010 geographies (BG/tract).
#        - Variable: pct_No_Health_Ins_ACS_15_19 (percent uninsured)
# -----------------------------------------------------------

# Build API URL for your state & county (block-group level)  
pdb_vars <- c("GIDBG", "pct_No_Health_Ins_ACS_15_19")
pdb_url  <- paste0(
  "https://api.census.gov/data/2021/pdb/blockgroup?get=",
  paste(pdb_vars, collapse = ","),
  "&for=block%20group:*&in=state:", state_fips, "+county:", county_fips
)

# Fetch + tidy
pdb_raw <- fromJSON(pdb_url)
colnames(pdb_raw) <- pdb_raw[1, ]

pdb_bg <- as_tibble(pdb_raw[-1, ]) %>%
  mutate(
    GIDBG                       = as.character(GIDBG),
    pct_No_Health_Ins_ACS_15_19 = as.numeric(pct_No_Health_Ins_ACS_15_19)
  ) %>%
  transmute(
    bg_geoid          = GIDBG,
    uninsured_share_bg = pct_No_Health_Ins_ACS_15_19 / 100   # 0–1 uninsured share
  )

# Attach BG uninsured to each 2010 block (broadcast BG -> blocks)
pdb_blocks <- blocks10 %>%
  left_join(pdb_bg, by = "bg_geoid") %>%
  dplyr::select(GEOID, uninsured_share_bg) %>%
  st_drop_geometry()

# ~500 nas - come from non residential industrial areas as well - safe to drop


# 3) Finalising census data -----------------------------------------------------

# join census data
census_data <- full_join(acs_blocks, decennial_blocks, by = "GEOID") %>%
  left_join(pdb_blocks, by = "GEOID") %>%
  dplyr::select(
    GEOID,
    homeownership_rate_bg, ba_plus_share_bg, median_income_bg,
    unemployment_rate_bg,
    uninsured_share_bg,
    black_share, vacancy_rate
  ) %>%
  st_drop_geometry()

# sum(is.na(census_data$ba_plus_share))  # about ~ 600 nas after join, 
# likely because decennial data only contains blocks with people living in them 
# acs does it on block group level (we then assign it to each block within, inc. with no people)

census_data <- census_data %>% drop_na() 


# 4) Export ---------------------------------------------------------------------

# CSV 
write.csv(
  census_data,
  "census_data.csv",
  row.names = FALSE
)

