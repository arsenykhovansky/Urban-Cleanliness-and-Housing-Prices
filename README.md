# Urban Cleanliness and Housing Prices

This repository contains the R code and prepared datasets used in my bachelor thesis in Economics.

**Thesis:** *The Impact of Urban Cleanliness Initiatives on Housing Prices: Evidence from Philadelphia*  
**Author:** Arseny Khovanskiy

---

## What is included

This repository contains:

- **Prepared datasets (ready to run the models):**
  - `sales_master_prepared.gpkg`
  - `block_characteristics_prepared.csv`

- **Data handling / preparation scripts (optional):**
  - `01_data_handling_sales/` – scripts used to prepare the sales dataset
  - `02_data_handling_bc/` – scripts used to prepare the block characteristics dataset

- **Model estimation scripts:**
  - `model_master_script.R` – unified “master” script containing the code for all model specifications.
    It is based on the baseline model and uses `if/else` branches to switch the parts that differ
    across Model 1, Model 2, and Model 3 (as most code is shared/reused between models).
  - `03_model_scripts/` – individual scripts for each of the 3 models (separate script per model).

---

## Quick start (run models directly with prepared data)

You do **not** need to run the full data-handling pipeline in `01_` and `02_` to estimate the models.
The models can be run directly using the prepared datasets provided in this repository.

### Step 1 — Download the repository

### Step 2 — Open R / RStudio and set the working directory

### Step 3 — Run the unified master script

