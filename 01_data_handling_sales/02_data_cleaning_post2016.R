# ==============================================================================
# SALES DATASET — Clean Post-2016 Sales & Structural Characteristics

# GOAL: clean sales dataset with all post 2016 sales and properties' structural characteristics 
#
# OUTPUTS:
#   - sales_master_1625_clean.gpkg  (layer = "sales_master")
#
# TABLE OF CONTENTS
#   0) Packages
#   1) Data Import
#   2) Basic screens
#   3) De-duplication (exact & near)
#   4) Normalize prices to $2019M1
#   5) Structural controls
#   6) Non–arms-length & property type
#   7) Arms-length & plausibility screens
#   8) Anomalies (timing & extreme jumps)
#   9) Outlier treatment
#   10) Handling NAs in non-key controls
#   11) Finalize
#   12) Export
# ==============================================================================


# 0) Packages -------------------------------------------------------------------
# package installation helper (if needed)
# if (!requireNamespace("quantmod", quietly=TRUE))     install.packages("quantmod")

library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(quantmod)


# 1) Data Import ----------------------------------------------------------------

# Sales master
sm <- st_read(
  "sales_master_1625.gpkg",
  layer = "sales_master"
)

## Robustness Check 1 - year inclusion/ exclusion
################################################################################
# exclude 2020 (covid) - baseline - check for robustness
sm <- sm %>% filter(!(date >= as.Date("2020-01-01") & date <= as.Date("2020-12-31")))
################################################################################

################################################################################
## exclude 2019 (pilot) - check for robustness
# sm <- sm %>% filter(!(date >= as.Date("2019-01-01") & date <= as.Date("2019-12-31")))
################################################################################


## Robustness Check 3 - placebo 
################################################################################
## treatment effect on to be treated areas - drop data after treatment began
# sm <- sm %>% filter(!date >= as.Date("2021-08-09"))
################################################################################


# 2) Basic screens --------------------------------------------------------------

# 2.1) Check primary key consistency 
sm <- sm %>% drop_na(pin, parcel_number)

# 2.2) Check if each pin corresponds to more than one parcel number
any(
  sm %>%
    distinct(pin, parcel_number) %>%
    count(pin) %>%
    filter(n > 1) %>%
    nrow() > 0
) 
# false, will use pin for consistency from now on

# 2.3) Remove price recording errors & restrict date range
sm <- sm %>% 
  filter(!is.na(total_consideration)) %>%
  mutate(.price_diff = abs(total_consideration - (cash_consideration + other_consideration))) %>% 
  filter(.price_diff <= 1) %>%                                                                     
  dplyr::select(-.price_diff) %>%                                                                         
  filter(!is.na(date)) %>% 
  filter(date <= as.Date("2024-04-01"))

# 2.4) Combine two zip columns
sm$zip_code <- ifelse(!is.na(sm$zip_code.x), sm$zip_code.x, sm$zip_code.y)
sm$zip_code.x <- NULL
sm$zip_code.y <- NULL

# 2.5) Clean zip code
sm$zip_code <- ifelse(
  is.na(sm$zip_code),
  NA_character_,
  substring(sm$zip_code, 1, 5)
)

# 2.6) Filter discrepancy
sm <- sm %>% filter(discrepancy == FALSE)

# 2.7) Remove vacant buildings
sm <- sm %>% filter(! exterior_condition %in% c(7, 8, 9))

# 2.8) Exclude condos (because of the spatial join we do)
sm <- sm %>% filter(condoflag == 0)


# 3) De-duplication (exact & near) ---------------------------------------------

# 3.1) Deduplicate obvious repeats 
# same document id and parcel number 
any(duplicated(sm[c("document_id", "pin")]))            # false
any(duplicated(sm[c("document_id", "parcel_number")]))  # false
# if true
# sm <- sm %>%
#   group_by(document_id, parcel_number) %>% 
#   filter(n() == 1) %>%
#   ungroup()

# same document id per date (only relevant if you work with property_count = 1)
any(duplicated(sm[c("document_id", "date")]))  # false
# if true
# sm <- sm %>%
#   arrange(recording_date) %>%
#   distinct(document_id, .keep_all = TRUE)

# 3.2) "Near duplicates": same date × pin × rounded price => keep first
sm <- sm %>%
  mutate(price_r = round(total_consideration, -2)) %>%
  { 
    message("Any duplicates? ", any(duplicated(.[c("date", "pin", "price_r")])))
    distinct(., date, pin, price_r, .keep_all = TRUE)
  } %>% 
  dplyr::select(-price_r)


# 4) Price preparation and cleaning --------------------------------------------

# Normalize prices to $2019M1
# 4.1) Preserve nominal for FMV/tax
sm <- sm %>%
  mutate(total_consideration_nominal = total_consideration)

# 4.2) Build CPI table and deflate to $2019M1
suppressMessages(getSymbols("CPIAUCSL", src = "FRED", auto.assign = TRUE))

cpi_tbl <- data.frame(
  month = floor_date(as.Date(index(CPIAUCSL)), "month"),
  cpi   = as.numeric(CPIAUCSL$CPIAUCSL)
) %>%
  distinct(month, .keep_all = TRUE)

base_cpi <- cpi_tbl$cpi[cpi_tbl$month == as.Date("2019-01-01")]

sm <- sm %>%
  mutate(month = floor_date(date, "month")) %>%
  left_join(cpi_tbl, by = "month") %>%
  mutate(total_consideration = total_consideration * (base_cpi / cpi)) %>%  
  dplyr::select(-cpi, -month)

# cleaning and handling duplicates
# 4.3) $10k real floor (after deflation)
sm <- sm %>% filter(total_consideration > 10000)

# 4.4) Within-quarter duplicates: keep highest price per pin × quarter
sm <- sm %>%
  mutate(sale_qtr = paste0(year(date), "Q", quarter(date))) %>%
  group_by(pin, sale_qtr) %>%
  slice_max(order_by = total_consideration, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(-sale_qtr)


# 5) Structural controls --------------------------------------------------------

# 5.1) Key controls present
sm <- sm %>%
  filter(
    !is.na(total_livable_area),
    !is.na(number_of_bedrooms),
    !is.na(number_of_bathrooms)
  )

# 5.2) Plausibility ranges
sm <- sm[sm$total_livable_area >= 300  & sm$total_livable_area <= 3000, ]
sm <- sm[sm$number_of_bedrooms >= 0 & sm$number_of_bedrooms <= 10, ]
sm <- sm[sm$number_of_bathrooms >= 0 & sm$number_of_bathrooms <= 10, ]


# 6) Non–arms-length & property type -------------------------------------------

# 6.1) Remove likely non–arms-length transactions
# remove where LLC is in both columns, INC, LP, LLC, INC, Corporation, trust
sm <- sm %>%
  filter(
    !(
      grepl("LLC|INC|CORP|LP|LLP|Corporation|Trust", grantors, ignore.case = TRUE) &
        grepl("LLC|INC|CORP|LP|LLP|Corporation|Trust", grantees, ignore.case = TRUE)
    )
  )

# 6.2) Keep residential properties only
sm <- sm %>% 
  filter(!is.na(category_code)) %>% 
  filter(category_code_description %in% c("SINGLE FAMILY", "MULTI FAMILY"))

# 6.3) Rowhouse / townhouse flag
sm <- sm %>%
  mutate(
    rowhouse_flag = as.integer(
      grepl("(^ROW\\b|\\bROW\\b)", building_code_description_new, ignore.case = TRUE) |
        grepl("(^ROW\\b|\\bROW\\b)", building_code_description,      ignore.case = TRUE)
    )
  )


# 7) Arms-length & plausibility screens ----------------------------------------

# 7.1) Document types (deed-type transactions)
sm <- sm %>% 
  filter(document_type %in% c(
    "DEED",
    "DEED MISCELLANEOUS TAXABLE",
    "MISCELLANEOUS DEED TAXABLE",
    "MISCELLANEOUS DEED",
    "DEED MISCELLANEOUS",
    "DEED RTT - OTHER"
  )) 

# 7.2) FMV/price plausibility band (use nominal denominator saved earlier)
sm <- sm %>%                                                                  
  mutate(ratio = fair_market_value / total_consideration_nominal) %>%         
  filter(ratio >= 0.1, ratio <= 3) %>%                                        
  dplyr::select(-ratio)                                                       

# 7.3) Taxes (keep between 2% and 6%) 
sm <- sm %>%
  mutate(
    actual_tax_percent = (coalesce(state_tax_amount, 0) + coalesce(local_tax_amount, 0)) /
      total_consideration_nominal
  ) %>%
  filter(actual_tax_percent >= 0.02, actual_tax_percent <= 0.06) %>%
  dplyr::select(-actual_tax_percent)


# 8) Anomalies (timing & extreme jumps) ----------------------------------------
MIN_GAP_DAYS <- 365
MAX_ABS_DLN  <- 1  # |Δln(price)| > 1 ⇒ drop the second sale

sm <- sm %>%
  arrange(pin, recording_date) %>%                                
  group_by(pin) %>%                                              
  mutate(gap_days = as.numeric(recording_date - lag(recording_date))) %>%
  filter(is.na(gap_days) | gap_days >= MIN_GAP_DAYS) %>%
  mutate(dln_price = log(total_consideration) - log(lag(total_consideration))) %>%
  filter(is.na(dln_price) | abs(dln_price) <= MAX_ABS_DLN) %>%
  ungroup() %>%
  dplyr::select(-gap_days, -dln_price)


# 9) Outlier treatment ----------------------------------------------------------
# Log transform (prices > 10,000 already dropped above)
sm <- sm %>%
  mutate(ln_price = log(total_consideration))


# 10) Handling NAs in non-key controls -----------------------------------------
# # n stories 3
# sum(is.na(sm$number_stories))
# # year built 0
# sum(is.na(sm$year_built))
# # quality grade 463
# sum(is.na(sm$quality_grade))
# # exterior condition 1
# sum(is.na(sm$exterior_condition))
# # lot width 15
# sum(is.na(sm$lot_width))
# # lot depth 10
# sum(is.na(sm$lot_depth))
# 
# nas in non key controls
# basements 12657
# heater 19164
# fireplace 386
# center air 20357
# topography view 792


# 11) Finalize ------------------------------------------------------------------

# 11.1) Drop unneeded columns and relocate ln_price
sm <- sm %>%
  dplyr::select(
    -assessed_value,
    -building_code,
    -building_code_description,
    -cash_consideration,
    -common_level_ratio,
    -condo_name,
    -condo_unit,
    -discrepancy,
    -fuel,
    -interior_condition,
    -local_tax_amount,
    -local_tax_percent,
    -location,
    -mailing_address,
    -matched_regmap,
    -number_of_rooms,
    -other_consideration,
    -parcel_number,
    -property_count,
    -recording_date,
    -reg_map_id,
    -sale_date,
    -sale_price,
    -separate_utilities,
    -site_type,
    -state_tax_amount,
    -state_tax_percent,
    -year_built_estimate
  ) %>% 
  rename(
    price                     = total_consideration, 
    building_code             = building_code_new,
    building_code_description = building_code_description_new
  ) %>% 
  relocate(ln_price, .after = price)
# (price is now **real $2019M1** after deflation above)

# 11.2) Format types
sm <- sm %>%
  mutate(
    date               = ymd(date),             
    zip_code           = as.character(zip_code), 
    census_tract       = factor(census_tract),
    basements          = factor(basements),
    central_air        = factor(central_air),
    type_heater        = factor(type_heater),
    quality_grade      = factor(quality_grade),
    exterior_condition = factor(exterior_condition), 
    topography         = factor(topography),
    view               = factor(view),
    building_code      = factor(building_code),
    category_code      = factor(category_code),
    opa_account_number = str_pad(opa_account_number, 9, pad = "0")
  )

# 11.3) Rename variables for final dataset
sm <- sm %>%
  rename(
    fmv            = fair_market_value,
    livable_area   = total_livable_area,
    bedrooms       = number_of_bedrooms,
    bathrooms      = number_of_bathrooms,
    stories        = number_stories,
    garage         = garage_spaces,
    heating        = type_heater,
    quality        = quality_grade,
    bldg_code      = building_code,
    bldg_code_desc = building_code_description,
    category_desc  = category_code_description,
    address        = street_address,
    condo_flag     = condoflag,
    tract          = census_tract,
    zip            = zip_code
  )


# 12) Export --------------------------------------------------------------------

# GeoPackage (to preserve formatting)
st_write(
  sm,
  "sales_master_1625_clean.gpkg",
  layer  = "sales_master",
  append = FALSE
)
