# ==============================================================================
# BLOCK CHARACTERISTICS — Prepared for Modelling

# GOAL: prepare block characteristics data for modelling

# OUTPUTS:
#   - block_characteristics_prepared.csv

# TABLE OF CONTENTS
#   0) Packages
#   1) Load data
#   2) Prepare features (z-scores + squares)
#   3) Export
# ==============================================================================


# 0) Packages -------------------------------------------------------------------
# install helper (if needed)
# if (!requireNamespace("tidycensus", quietly=TRUE))     install.packages("digest")
library(dplyr)
library(readr)


# 1) Load data ------------------------------------------------------------------
bc <- read_csv(
  "block_characteristics.csv",
  col_types = cols(GEOID = col_character())
)


# 2) Prepare features (z-scores + squares) -------------------------------------

# 2.1) Standardize non-GEOID variables (z-scores)
bc <- bc %>%
  mutate(
    across(
      !matches("^GEOID$"),
      .fns   = ~ as.numeric(scale(.x)),
      .names = "z_{col}"
    )
  )

# 2.2) Create squared terms and filter complete z-features
bc <- bc %>%
  mutate(
    across(
      starts_with("z_"),
      ~ .x^2,
      .names = "{col}_sq"
    )
  ) %>%
  filter(if_all(starts_with("z_")))


# 3) Export ---------------------------------------------------------------------
bc <- bc %>%
  mutate(no_vacant = as.integer(no_vacant))

write.csv(
  bc,
  "block_characteristics_prepared.csv",
  row.names = FALSE
)
