# ==============================================================================
# MODELING DATA — Hybrid Hedonic & Autoregressive HPI (Eq. 1)

# GOAL: prepare data for the hybrid hedonic and autoregressive price model (eq.1)

# OUTPUTS:
#   - sales_master_prepared.gpkg  (layer = "sales_master")

# TABLE OF CONTENTS
#   0) Packages
#   1) Data Import
#   2) Geometry prep (treated areas)
#   3) Price history (previous sale)
#   4) Treatment flags (100 m buffer)
#   5) Attach block GEOIDs (spatial join)
#   6) Feature engineering for Eq. (1)
#   7) Export: modeling-ready dataset
# ==============================================================================


# 0) Packages -------------------------------------------------------------------
# package installation helper (if needed)
# if (!requireNamespace("janitor", quietly=TRUE)) install.packages("janitor")

library(sf)
library(lwgeom)
library(dplyr)
library(lubridate)


# 1) Data Import ----------------------------------------------------------------

# 1.1) Post-2016 cleaned sales (with geometry)
sm <- st_read("sales_master_1625_clean.gpkg", layer = "sales_master") %>%
  filter(!is.na(opa_account_number))

# 1.2) Pre-2016 cleaned sales (with geometry)
s <- st_read("sales_pre16_clean.gpkg", layer = "sales_pre16") %>%
  filter(!is.na(opa_account_number))

# 1.3) Treated areas (phase 2 + expansion)
ta  <- st_read("treated_areas_phase2.gpkg",  layer = "treated_areas_phase2")
ta2 <- st_read("treated_areas_phase2ex.gpkg", layer = "treated_areas_phase2ex")

# 1.4) 2010 Census blocks (GEOID)
# https://opendataphilly.org/datasets/census-blocks/ 
blocks10 <- st_read("Census_Blocks_2010.geojson") %>%
  transmute(GEOID = as.character(GEOID10)) %>%
  st_make_valid()


# 2) Geometry prep (treated areas) ---------------------------------------------

# 2.1) Align polygons to sales CRS
ta  <- st_transform(ta,  st_crs(sm))
ta2 <- st_transform(ta2, st_crs(sm))

# 2.2) Build geometry unions (treated areas)
ta_u  <- st_union(st_make_valid(ta))
ta_u2 <- st_union(st_make_valid(ta2))


# 3) Price history (previous sale) ---------------------------------------------

# join sales post 2016 with sales pre 2016
hist_all <- bind_rows(
  s  %>% st_drop_geometry() %>% dplyr::select(opa_account_number, date, price) %>% mutate(src = "pre"),
  sm %>% st_drop_geometry() %>% dplyr::select(opa_account_number, date, price) %>% mutate(src = "post")
) %>%
  arrange(opa_account_number, date) %>%
  group_by(opa_account_number) %>%
  mutate(
    last_date  = lag(date),
    last_price = lag(price)
  ) %>%
  ungroup()

sm <- sm %>%
  left_join(
    hist_all %>%
      filter(src == "post") %>%
      dplyr::select(opa_account_number, date, last_date, last_price),
    by = c("opa_account_number", "date")
  ) %>%
  filter(!is.na(last_date)) %>%  # keep only sales with a past sale
  mutate(
    years_since_last = as.numeric(difftime(date, last_date, units = "days")) / 365.25,
    ln_price_last    = log(last_price),
    lnP_last_x_years = ln_price_last * years_since_last
  )


# 4) Treatment flags (100 m buffer) --------------------------------------------

# 4.1) Buffers 
## Robustness check 2 - varying buffer sizes
################################################################################
# adjust buffers for robustness check
################################################################################

# CRS units confirmed meters → buffer directly
buf50_p2   <- st_make_valid(st_buffer(ta_u,  100))
buf50_p2ex <- st_make_valid(st_buffer(ta_u2, 100))

# 4.2) Treatment & post-period indicators
sm$in_phase2_zone   <- as.integer(st_within(sm, buf50_p2,   sparse = FALSE)[, 1])
sm$in_phase2ex_zone <- as.integer(st_within(sm, buf50_p2ex, sparse = FALSE)[, 1])

# Policy “go-live” dates
phase2   <- as.Date("2021-08-09")
phase2ex <- as.Date("2022-04-04")

## Robustness check 3 - placebo 
################################################################################
# Policy placebo dates
# phase2   <- as.Date("2018-08-09")
# phase2ex <- as.Date("2019-04-04")
################################################################################

sm$treated_phase2   <- as.integer(sm$in_phase2_zone   == 1 & sm$date >= phase2)
sm$treated_phase2ex <- as.integer(sm$in_phase2ex_zone == 1 & sm$date >= phase2ex)
sm$treated_any      <- as.integer(sm$treated_phase2 == 1 | sm$treated_phase2ex == 1)

# Post-period dummies
sm$post_phase2   <- as.integer(sm$date >= phase2)
sm$post_phase2ex <- as.integer(sm$date >= phase2ex)


# 5) Attach block GEOIDs (spatial join) ----------------------------------------
blocks10 <- st_transform(blocks10, st_crs(sm))
sm <- st_join(sm, blocks10["GEOID"], join = st_within)  


# 6) Feature engineering for Eq. (1) -------------------------------------------

# 6.1) Lot & size transforms
sm <- sm %>%
  mutate(
    lot_sqft   = coalesce(total_area, lot_width * lot_depth),
    sqrt_floor = sqrt(livable_area),
    sqrt_lot   = sqrt(lot_sqft),
    ln_floor   = log(pmax(livable_area, 1)),
    ln_lot     = log(pmax(lot_sqft, 1)),
    age        = year(date) - year_built,
    age2       = age^2
  )

# 6.2) Time & geography fixed-effect labels
sm$qdate    <- factor(paste0(year(sm$date), "Q", quarter(sm$date)))
sm$tract_id <- substr(sm$GEOID, 1, 11)
sm$bg_id    <- substr(sm$GEOID, 1, 12)

# 6.3) Buckets for bedrooms & build year
sm <- sm %>%
  mutate(
    bedrooms_bin = cut(
      bedrooms,
      breaks = c(-Inf, 1, 2, 3, 4, Inf),
      labels = c("1", "2", "3", "4", "5plus"),
      right  = TRUE
    ),
    bldg_bin = cut(
      year_built,
      breaks = c(-Inf, 1900, 1920, 1940, 1960, 1980, 2000, Inf),
      labels = c("<=1900", "1901-1920", "1921-1940", "1941-1960",
                 "1961-1980", "1981-2000", ">2000"),
      right  = TRUE
    )
  )

# Quick tally
sum(sm$treated_any == 1)


# 7) Export: modeling-ready ----------------------------------------------------

# 7.1) Variable selection (modeling-ready)
smf <- sm %>%
  dplyr::select(
    # core identifiers
    pin, date, qdate, GEOID, tract, tract_id, bg_id,
    # dependent variable
    price, ln_price,
    # previous sale information
    last_date, last_price, ln_price_last, years_since_last, lnP_last_x_years,
    # building characteristics
    livable_area, total_area, bedrooms, bedrooms_bin, bathrooms, stories,
    year_built, bldg_bin, age, age2,
    # lot characteristics
    ln_lot, ln_floor,
    # features & amenities
    garage, basements, fireplaces, central_air, heating,
    quality, exterior_condition, topography, view,
    # building type & category
    rowhouse_flag,
    # time / policy controls (optional)
    treated_any, post_phase2, post_phase2ex
  ) %>%
  mutate(pin = as.character(pin))

# 7.2) GeoPackage export
st_write(
  smf,
  "sales_master_prepared.gpkg",
  layer  = "sales_master",
  append = FALSE
)

