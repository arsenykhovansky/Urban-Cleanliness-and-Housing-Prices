# ==============================================================================
# SALES DATASET — Post-2016 Sales & Structural Characteristics

# GOAL: sales dataset with all post 2016 sales and properties' structural characteristics.
# Variables required for joining are cleaned in this script, 
# other variables are cleaned in the data cleaning script

# OUTPUTS:
#   - sales_master_1625.gpkg  (layer = "sales_master")

# TABLE OF CONTENTS
#   0) Packages
#   1) Data Import
#       1.1) Parcels
#       1.2) Transfers
#       1.3) Properties
#   2) Data Wrangling
#       2.1) Parcels
#       2.2) Transfers
#       2.3) Properties
#   3) Spatial Join
#       3.1) Transfers → Parcels
#       3.2) Transfers/Parcels → Properties
#   4) Finalizing
#   5) Export
# ==============================================================================


# --- 0) Packages --------------------------------------------------------------
# install packages helper (if needed)
# install.packages("sf", type = "binary", repos = "https://cloud.r-project.org")
# if (!requireNamespace("digest", quietly=TRUE))     install.packages("digest")

library(sf)
library(lwgeom)
library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)
library(digest)


# extracting zip files helper (if needed)
{
# unzip("DOR_Parcel.zip", exdir = "DOR_Parcel", unzip = "internal")
# unzip("transfers2016_17.zip",
# 
# check
# shp_files <- list.files("DOR_Parcel", pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
# shp_files   # should see at least one .shp path here
}


# --- 1) Data Import -----------------------------------------------------------

# 1.1) PARCELS -----------------------------------------------------------------
# ==============================================================================
# PARCELS 
# ==============================================================================
# https://opendataphilly.org/datasets/department-of-records-property-parcels/ 
#  Import parcels.shp
shp_files <- list.files(
  "DOR_Parcel",
  pattern = "\\.shp$", full.names = TRUE, recursive = TRUE
)
stopifnot(length(shp_files) == 1)  # fail fast if ambiguous
parcels <- st_read(shp_files[1], quiet = FALSE) %>% st_make_valid()

# Quick checks (optional)
{
  # st_geometry_type(parcels)[1]   # should be POLYGON or MULTIPOLYGON
  # st_crs(parcels)                # CRS info
  # names(parcels)[1:10]           # confirm you see pin, basereg/mapreg, parcel, etc.
}


# 1.2) TRANSFERS ---------------------------------------------------------------
# ==============================================================================
# TRANSFERS
# ==============================================================================
# https://opendataphilly.org/datasets/real-estate-transfers/ 
# Import transfers.shp
shp_files1 <- list.files(
  "transfers2016_17",
  pattern = "\\.shp$", full.names = TRUE, recursive = TRUE
)
stopifnot(length(shp_files1) == 1)  # fail fast if ambiguous
t1617 <- st_read(shp_files1[1], quiet = FALSE)

shp_files2 <- list.files(
  "transfers2018_19",
  pattern = "\\.shp$", full.names = TRUE, recursive = TRUE
)
stopifnot(length(shp_files2) == 1)
t1819 <- st_read(shp_files2[1], quiet = FALSE)

shp_files3 <- list.files(
  "transfers2020",
  pattern = "\\.shp$", full.names = TRUE, recursive = TRUE
)
stopifnot(length(shp_files3) == 1)
t20 <- st_read(shp_files3[1], quiet = FALSE)


# ==============================================================================
# PROPERTIES 
# ==============================================================================
# https://opendataphilly.org/datasets/philadelphia-properties-and-assessment-history/ 
# Import properties.csv
properties <- read_csv("opa_properties_public.csv")

# sanity check (optional) - same pin yields same address
{
# subset(parcels, pin == 1001570166, select = c(pin, addr_std))
# subset(properties, pin == 1001570166, select = c(pin, location))
}


# # --- 2) Data Wrangling ------------------------------------------------------

# ==============================================================================
# PARCELS 
# ==============================================================================
# 1) select needed variables
parcels <- parcels %>%
  mutate(
    pin       = as.character(pin),
    mapreg    = as.character(mapreg),
    parcel    = as.character(parcel),
    condoflag = as.integer(condoflag)
  ) %>%
  select(pin, mapreg, parcel, condoflag)
gc()

# 2) drop nas and duplicates

# # inspect of nas (optional)
# sum(is.na(parcels$parcel)) # 379 duplicated - um(table(parcels$parcel)>1) - 1536
# sum(is.na(parcels$pin)) # 27840
# sum(is.na(parcels$condoflag)) #0
# sum(is.na(parcels$geometry) | st_is_empty(parcels)) #37

# drop nas
parcels <- parcels %>% 
  drop_na(pin, condoflag) %>% 
  filter(!is.na(geometry) & !st_is_empty(geometry))

# # inspect duplicate pins (optional)
# parcels_pin_repeats <- parcels %>%
#   filter(!is.na(pin)) %>% 
#   group_by(pin) %>%
#   filter(n() > 1)
# nrow(parcels_pin_repeats) # 552

# drop duplicate pins
parcels <- parcels %>%
  group_by(pin) %>%
  filter(n() == 1) %>%
  ungroup()

# handle duplicate geometries
st_crs(parcels)  # verify EPSG and units - ftUS
snap <- 1  # tolerance in CRS units

# # inspect duplicate geometries (optional)
# parcels_dups <- parcels %>%
#     st_snap_to_grid(snap) %>%
#     st_buffer(0) %>%
#     # returns same nrow as  mutate(key = st_as_text(geometry)) but faster
#     mutate(key = vapply(st_as_binary(geometry), digest, character(1))) %>%
#     group_by(key) %>%
#     filter(n() > 1) %>%
#     ungroup() %>%
#     select(-key)

# drop duplicate geometries
parcels <- parcels %>%
  st_snap_to_grid(snap) %>%
  st_buffer(0) %>%
  mutate(key = vapply(st_as_binary(geometry), digest, character(1))) %>%
  add_count(key) %>%
  filter(n == 1) %>%
  select(-key, -n)


# ==============================================================================
# TRANSFERS
# ==============================================================================

# 2016 - 2017
# drop nas and property count > 1
t1617 <- t1617 %>%
  filter(property_c == 1) %>% 
  st_make_valid() %>% 
  filter(!is.na(geometry) & !st_is_empty(geometry))

t1617 <- t1617 %>%
  mutate(
    recording_ = as.Date(recording_),
    total_cons = as.numeric(total_cons),  # readr::parse_number(as.character(total_cons))
    discrepanc = as.logical(discrepanc)
  ) %>%
  filter(
    !is.na(total_cons), total_cons != 0
  ) %>% 
  select(
    date                  = display_da,  # canonical date
    recording_date        = recording_,
    total_consideration   = total_cons,
    cash_consideration    = cash_consi, 
    other_consideration   = other_cons,
    assessed_value        = assessed_v, 
    street_address        = street_add, 
    common_level_ratio    = common_lev, 
    fair_market_value     = fair_marke, 
    zip_code              = zip_code,
    unit_number           = unit_num,
    condo_name            = condo_name,
    document_type         = document_t,
    document_id           = document_i, # id for transfers
    matched_regmap        = matched_re,
    reg_map_id            = reg_map_id, 
    discrepancy           = discrepanc,
    property_count        = property_c,
    grantors, 
    grantees,
    local_tax_amount      = local_tax_, # arms lengh transcations helpers
    local_tax_percent     = local_ta_1,
    state_tax_amount      = state_tax_, 
    state_tax_percent     = state_ta_1, 
    opa_account_number    = opa_accoun
  )
gc()

# 2018 - 2019
# drop nas and property count > 1
t1819 <- t1819 %>%
  filter(property_c == 1) %>% 
  st_make_valid() %>% 
  filter(!is.na(geometry) & !st_is_empty(geometry))

t1819 <- t1819 %>%
  mutate(
    recording_ = as.Date(recording_),
    total_cons = as.numeric(total_cons),  # readr::parse_number(as.character(total_cons))
    discrepanc = as.logical(discrepanc)
  ) %>%
  filter(
    !is.na(total_cons), total_cons != 0
  ) %>% 
  select(
    date                  = display_da,
    recording_date        = recording_,
    total_consideration   = total_cons,
    cash_consideration    = cash_consi, 
    other_consideration   = other_cons,
    assessed_value        = assessed_v, 
    street_address        = street_add, 
    common_level_ratio    = common_lev, 
    fair_market_value     = fair_marke, 
    zip_code              = zip_code,
    unit_number           = unit_num,
    condo_name            = condo_name,
    document_type         = document_t,
    document_id           = document_i, # id for transfers
    matched_regmap        = matched_re,
    reg_map_id            = reg_map_id, 
    discrepancy           = discrepanc, 
    property_count        = property_c,
    grantors, 
    grantees, 
    local_tax_amount      = local_tax_, # arms lengh transcations helpers
    local_tax_percent     = local_ta_1,
    state_tax_amount      = state_tax_, 
    state_tax_percent     = state_ta_1, 
    opa_account_number    = opa_accoun
  )
gc()

# 2020+
t20 <- t20 %>%
  filter(property_c == 1) %>% 
  st_make_valid() %>% 
  filter(!is.na(geometry) & !st_is_empty(geometry))

t20 <- t20 %>%
  mutate(
    recording_ = as.Date(recording_),
    total_cons = as.numeric(total_cons),  # readr::parse_number(as.character(total_cons))
    discrepanc = as.logical(discrepanc)
  ) %>%
  filter(
    !is.na(total_cons), total_cons != 0, 
    recording_ <= as.Date("2024-04-01")   # phase 4 begins on 4th Aug 2024
  ) %>% 
  select(
    date                  = display_da,
    recording_date        = recording_,
    total_consideration   = total_cons,
    cash_consideration    = cash_consi, 
    other_consideration   = other_cons,
    assessed_value        = assessed_v, 
    street_address        = street_add, 
    common_level_ratio    = common_lev, 
    fair_market_value     = fair_marke, 
    zip_code              = zip_code,
    unit_number           = unit_num,
    condo_name            = condo_name,
    document_type         = document_t,
    document_id           = document_i, # id for transfers
    matched_regmap        = matched_re,
    reg_map_id            = reg_map_id, 
    discrepancy           = discrepanc,
    property_count        = property_c,
    grantors, 
    grantees, 
    local_tax_amount      = local_tax_, # arms length transactions helpers
    local_tax_percent     = local_ta_1,
    state_tax_amount      = state_tax_, 
    state_tax_percent     = state_ta_1, 
    opa_account_number    = opa_accoun
  )
gc()


# ==============================================================================
# PROPERTIES 
# ==============================================================================
# inspect nas and duplicates key variables for joining

# nas
# inspect
sum(is.na(properties$pin))
sum(is.na(properties$parcel_number))

# drop
properties <- properties %>% drop_na(pin, parcel_number)

# duplicates
sum(duplicated(properties$pin))
sum(duplicated(properties$parcel_number))
# 0

# select needed variables
properties <- properties %>%
  mutate(
    pin = as.character(pin),
    basements = as.factor(basements)
  ) %>%
  select(
    basements,
    building_code,
    building_code_description, 
    building_code_description_new,
    building_code_new,
    category_code,
    category_code_description,
    census_tract,
    central_air,
    condo_unit = unit,
    exterior_condition,
    fireplaces,
    fuel,
    garage_spaces,
    interior_condition,
    location,
    lot_depth = depth,
    lot_width = frontage,
    mailing_address = mailing_address_1,
    number_of_bathrooms,
    number_of_bedrooms,
    number_of_rooms,
    number_stories,
    parcel_number, # id 
    pin,
    quality_grade,
    sale_date,
    sale_price,
    separate_utilities,
    site_type,
    topography,
    total_area,
    total_livable_area,
    type_heater,
    unit, # condo
    view = view_type,
    year_built,
    year_built_estimate,
    zip_code
  ) %>%
  distinct(pin, .keep_all = TRUE)
gc()


# # --- 3) Spatial Join --------------------------------------------------------

# 3.1) transfers -> parcels (point in polygon join)
# Same CRS for spatial joins
t1617 <- st_transform(t1617, st_crs(parcels))
t1819 <- st_transform(t1819, st_crs(parcels))
t20   <- st_transform(t20,   st_crs(parcels))

# join
t1617_parc <- st_join(t1617, parcels, join = st_within)
t1819_parc <- st_join(t1819, parcels, join = st_within)
t20_parc   <- st_join(t20,   parcels, join = st_within)

# remove rows
# (i) where a point matches more than one parcel polygon (e.g. because of overlap)
# (ii) with nas (where a point did not fall within any polygon in parcels)
t1617_parc <- t1617_parc %>%
  filter(!is.na(pin)) %>% 
  group_by(document_id) %>%   
  mutate(n_parcels = n_distinct(pin)) %>%
  ungroup() %>%
  filter(n_parcels == 1) %>%  
  select(-n_parcels)
nrow(t1617_parc)

t1819_parc <- t1819_parc %>%
  filter(!is.na(pin)) %>% 
  group_by(document_id) %>%   
  mutate(n_parcels = n_distinct(pin)) %>%
  ungroup() %>%
  filter(n_parcels == 1) %>%  
  select(-n_parcels)
nrow(t1819_parc)

t20_parc <- t20_parc %>%
  filter(!is.na(pin)) %>% 
  group_by(document_id) %>%   
  mutate(n_parcels = n_distinct(pin)) %>%
  ungroup() %>%
  filter(n_parcels == 1) %>%  
  select(-n_parcels)
nrow(t20_parc)

# % of rows dropped at each join
{
  # print((nrow(t1617) - nrow(t1617_parc)) / nrow(t1617))
  # print((nrow(t1819) - nrow(t1819_parc)) / nrow(t1819))
  # print((nrow(t20)   - nrow(t20_parc))   / nrow(t20))
}

# 3.2) transfers_parcels -> properties 
t1617_ready <- left_join(t1617_parc, properties, by = "pin")
t1819_ready <- left_join(t1819_parc, properties, by = "pin")
t20_ready   <- left_join(t20_parc,   properties, by = "pin")

# check if rows matched one to one
sum(nrow(t1617_ready) + nrow(t1819_ready) + nrow(t20_ready)) == sum(nrow(t1617_parc) + nrow(t1819_parc) + nrow(t20_parc))


# --- 4) Finalizing ------------------------------------------------------------

# combine
sales_master <- bind_rows(t1617_ready, t1819_ready, t20_ready)

# order column names in logical order
order_cols <- c(
  # 1) Keys & main date
  "parcel_number", "pin",
  "date", "opa_account_number",
  
  # 2) Price-related (most important first)
  "total_consideration", "sale_price", "assessed_value", "fair_market_value",
  "common_level_ratio", "cash_consideration", "other_consideration",
  "property_count", "local_tax_amount", "local_tax_percent",
  "state_tax_amount", "state_tax_percent",
  
  # 3) Structure (most important to the left)
  "number_of_rooms", "total_livable_area", "total_area",
  "number_of_bedrooms", "number_of_bathrooms",
  "lot_width", "lot_depth", "number_stories",
  "year_built", "year_built_estimate",
  "garage_spaces", "basements", "fireplaces",
  "central_air", "separate_utilities", "type_heater", "fuel",
  "quality_grade", "exterior_condition", "interior_condition",
  "site_type", "topography", "view",
  "building_code", "building_code_description",
  "building_code_new", "building_code_description_new",
  "category_code", "category_code_description",
  
  # 4) Location & condo
  "street_address", "mailing_address", "unit_number",
  "condo_name", "condo_unit", "condoflag",
  "location", "census_tract", "zip_code.x", "zip_code.y",
  
  # 5) Other IDs & mapping
  "parcel", "mapreg", "reg_map_id", "matched_regmap",
  
  # 6) Docs, parties, other dates
  "document_type", "document_id", "grantors", "grantees",
  "recording_date", "sale_date",
  
  # 7) Misc
  "discrepancy",
  
  # 8) Geometry last
  "geom"
)

sales_master <- sales_master %>%
  select(any_of(order_cols), everything())


# --- 5) Export ----------------------------------------------------------------

# GeoPackage 
st_write(
  sales_master,
  "sales_master_1625.gpkg",
  layer = "sales_master", append = FALSE
)
