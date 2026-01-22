# ==============================================================================
# PRE-2016 SALES — Autoregressive Component for the Quality Adjusted Price Index (eq. 1)

# GOAL: gather data for the auto regressive component of the price index (eq.1) 
# keep helper variables needed for further data cleaning

# OUTPUTS:
#   - sales_pre16.gpkg  (layer = "sales_pre16")
#
# TABLE OF CONTENTS
#   0) Packages
#   1) Data Import (transfers CSVs)
#   2) Join
#   3) Export
# ==============================================================================


# --- 0) Packages --------------------------------------------------------------
# package installation helper (if needed)
# if (!requireNamespace("janitor", quietly=TRUE))     install.packages("janitor")

library(sf)
library(dplyr)
library(readr)


# --- 1) Data Import -----------------------------------------------------------

# ==============================================================================
# TRANSFERS
# ==============================================================================
# https://opendataphilly.org/datasets/real-estate-transfers/ 

# set column variables types to character to avoid parsing issues
t1415 <- read_csv(
  "transfers_2014-2015.csv",
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A"),
  show_col_types = FALSE
)

t1213 <- read_csv(
  "transfers_2012-2013.csv",
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A"),
  show_col_types = FALSE
)

t1011 <- read_csv(
  "transfers_2010-2011.csv",
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A"),
  show_col_types = FALSE
)

t89 <- read_csv(
  "transfers_2008-2009.csv",
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A"),
  show_col_types = FALSE
)

t67 <- read_csv(
  "transfers_2006-2007.csv",
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A"),
  show_col_types = FALSE
)

t45 <- read_csv(
  "transfers_2004-2005.csv",
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A"),
  show_col_types = FALSE
)

t23 <- read_csv(
  "transfers_2002-2003.csv",
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A"),
  show_col_types = FALSE
)


# --- 2) Join ------------------------------------------------------------------

sales_pre16 <- rbind(t1415, t1213, t1011, t89, t67, t45, t23) %>%
  mutate(
    display_date        = as.Date(display_date),
    total_consideration = as.numeric(total_consideration),  # readr::parse_number(as.character(total_cons))
    discrepancy         = as.logical(discrepancy),
    property_count      = as.numeric(property_count),
    cash_consideration  = as.numeric(cash_consideration),
    other_consideration = as.numeric(other_consideration),
    assessed_value      = as.numeric(assessed_value),
    fair_market_value   = as.numeric(fair_market_value),
    common_level_ratio  = as.numeric(common_level_ratio),
    state_tax_amount    = as.numeric(state_tax_amount),
    state_tax_percent   = as.numeric(state_tax_percent),
    local_tax_amount    = as.numeric(local_tax_amount),
    local_tax_percent   = as.numeric(local_tax_percent),
    recording_date      = as.Date(recording_date)
  ) %>%
  filter(property_count == 1) %>%
  filter(
    !is.na(total_consideration), total_consideration != 0
  ) %>%
  dplyr::select(
    date  = display_date,
    recording_date,
    total_consideration,
    cash_consideration,
    other_consideration,
    assessed_value,
    street_address,
    common_level_ratio,
    fair_market_value,
    zip_code,
    unit_number = unit_num,
    condo_name,
    document_type,
    document_id,
    matched_regmap,
    reg_map_id,
    discrepancy,
    property_count,
    grantors,
    grantees,
    local_tax_amount,
    local_tax_percent,
    state_tax_amount,
    state_tax_percent,
    opa_account_number  = opa_account_num
  )


# --- 3) Export ----------------------------------------------------------------

# GeoPackage (to preserve formatting)
st_write(
  sales_pre16,
  "sales_pre16.gpkg",
  layer = "sales_pre16", append = FALSE
)
