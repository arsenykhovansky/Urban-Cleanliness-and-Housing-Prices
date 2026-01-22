# ==============================================================================
# PRE-2016 SALES — Autoregressive Component for Price Index (eq. 1)

# GOAL: clean data for auto regressive component of the price index (eq.1) 

# OUTPUTS:
#   - sales_pre16_clean.gpkg  (layer = "sales_pre16")

# TABLE OF CONTENTS
#   0) Packages
#   1) Data Import
#   2) Basic screens
#   3) De-duplication (exact & near)
#   4) Normalize prices to $2019M1
#   5) Arms-length & plausibility screens (relevant subset)
#   6) Anomalies (timing & extreme jumps)
#   7) Finalize (minimal fields for linking & lag price)
#   8) Export
# ==============================================================================


# 0) Packages -------------------------------------------------------------------
# package installation helper (if needed)
# if (!requireNamespace("janitor", quietly=TRUE))     install.packages("janitor")

library(sf)
library(dplyr)
library(lubridate)
library(stringr)
library(quantmod)


# 1) Data Import ----------------------------------------------------------------
# transfers (geopackage - to preserve data formatting)
s <- st_read(
  "sales_pre16.gpkg",
  layer = "sales_pre16"
)


# 2) Basic screens --------------------------------------------------------------

# 2.1) keep rows with sale info and a property id
s <- s %>%
  filter(!is.na(date)) %>%
  filter(date <= as.Date("2015-12-31")) %>%
  filter(!is.na(total_consideration)) %>%
  mutate(.price_diff = abs(total_consideration - (cash_consideration + other_consideration))) %>%  # tolerant price consistency
  filter(.price_diff <= 1) %>%   # allow small rounding error
  dplyr::select(-.price_diff) %>%
  filter(discrepancy == FALSE)

# 2.2) ZIP 
s <- s %>%
  mutate(zip = ifelse(is.na(zip_code), NA_character_, substr(as.character(zip_code), 1, 5)))

# 2.3) remove institutional-to-institution transfers 
s <- s %>%
  filter(!(grepl("LLC|INC|CORP|LP|LLP|Corporation|Trust", grantors,  ignore.case = TRUE) &
             grepl("LLC|INC|CORP|LP|LLP|Corporation|Trust", grantees, ignore.case = TRUE)))


# 3) De-duplication (exact & near) ---------------------------------------------

# 3.1) exact duplicates by document × OPA
any(duplicated(s[c("document_id", "opa_account_number")]))

# 3.2) "near duplicates": same date × OPA × rounded price => keep first
s <- s %>%
  mutate(price_r = round(total_consideration, -2)) %>%
  { 
    message("Any near duplicates? ", any(duplicated(.[c("date", "opa_account_number", "price_r")])))
    distinct(., date, opa_account_number, price_r, .keep_all = TRUE)
  } %>%
  dplyr::select(-price_r)


# 4) Price preparation and cleaning --------------------------------------------

# 4.1) put prices in real $2019M1, preserving nominal for FMV/tax checks
s <- s %>%
  mutate(total_consideration_nominal = total_consideration)  # keep nominal for FMV/tax checks

# 4.2) build CPI table and deflate to $2019M1
suppressMessages(getSymbols("CPIAUCSL", src = "FRED", auto.assign = TRUE))
cpi_tbl <- data.frame(
  month = floor_date(as.Date(index(CPIAUCSL)), "month"),
  cpi   = as.numeric(CPIAUCSL$CPIAUCSL)
) %>%
  distinct(month, .keep_all = TRUE)

base_cpi <- cpi_tbl$cpi[cpi_tbl$month == as.Date("2019-01-01")]

s <- s %>%
  mutate(month = floor_date(date, "month")) %>%
  left_join(cpi_tbl, by = "month") %>%
  mutate(total_consideration = total_consideration * (base_cpi / cpi)) %>%  # real $2019M1
  dplyr::select(-cpi, -month)

# 4.3) $10k real floor 
s <- s %>%
  filter(total_consideration > 10000)  # threshold in real $2019M1

# 4.4) Within-quarter duplicates: keep highest price per OPA × quarter
s <- s %>%
  mutate(sale_qtr = paste0(year(date), "Q", quarter(date))) %>%
  group_by(opa_account_number, sale_qtr) %>%
  slice_max(order_by = total_consideration, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(-sale_qtr)


# 5) Arms-length & plausibility screens (relevant subset) ----------------------

# 5.1) Document types: keep deed-type transactions
s <- s %>%
  filter(document_type %in% c(
    "DEED",
    "DEED MISCELLANEOUS TAXABLE",
    "MISCELLANEOUS DEED TAXABLE",
    "MISCELLANEOUS DEED",
    "DEED MISCELLANEOUS",
    "DEED RTT - OTHER"
  ))

# 5.2) FMV/price plausibility band (use *nominal* denominator saved earlier)
s <- s %>%
  mutate(ratio = fair_market_value / total_consideration_nominal) %>%
  filter(ratio >= 0.1, ratio <= 3) %>%
  dplyr::select(-ratio)

# 5.3) Tax-rate plausibility band (2%–6%)
s <- s %>%
  mutate(
    actual_tax_percent =
      (coalesce(state_tax_amount, 0) + coalesce(local_tax_amount, 0)) /
      total_consideration_nominal
  ) %>%
  filter(actual_tax_percent >= 0.02, actual_tax_percent <= 0.06) %>%
  dplyr::select(-actual_tax_percent)


# 6) Anomalies (timing & extreme jumps) ----------------------------------------
MIN_GAP_DAYS <- 365
MAX_ABS_DLN  <- 1  # |Δln(price)| > 1 ⇒ drop later sale

s <- s %>%
  arrange(opa_account_number, recording_date) %>%
  group_by(opa_account_number) %>%
  mutate(
    gap_days  = as.numeric(recording_date - lag(recording_date)),
    dln_price = log(total_consideration) - log(lag(total_consideration))
  ) %>%
  filter(is.na(gap_days) | gap_days >= MIN_GAP_DAYS) %>%
  filter(is.na(dln_price) | abs(dln_price) <= MAX_ABS_DLN) %>%
  ungroup() %>%
  dplyr::select(-gap_days, -dln_price)


# 7) Finalize (minimal fields for linking & lag price) -------------------------
s <- s %>%
  mutate(
    ln_price           = log(total_consideration), # real $2019M1
    opa_account_number = str_pad(opa_account_number, 9, pad = "0")
  ) %>%
  dplyr::select(
    opa_account_number, date, recording_date,
    price = total_consideration, ln_price,
    document_id, document_type, street_address, zip,
    matched_regmap, reg_map_id
  )
# note: price reported in 2019 Q1 prices


# 8) Export ---------------------------------------------------------------------
# GeoPackage (to preserve formatting)
st_write(
  s,
  "sales_pre16_clean.gpkg",
  layer  = "sales_pre16",
  append = FALSE
)
