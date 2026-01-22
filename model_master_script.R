# ==============================================================================
# MASTER SCRIPT — MODELS 1–3 
#
# Model 1 - baseline
# Model 2 - time caliper
# Model 3 - pre-trend
#
# This script unifies models 1-3 into one script. To model 1 only model-specific 
# deviations are added via if/else branches:
#   - MODEL 2: MATCHING block uses (ps, scaled quarter index) + calipers
#   - MODEL 3: HPI block uses pre-trend normalization (pre-only tract trends)
#
# This script firstly computes the housing price index (HPI) - the residual from
# equation 1. Then, it estimates the selection model and extracts propensity
# scores for each sale. The propensity scores are used for matching, on the basis
# of which the sample weights are built. Finally, the ATT and AI SE are estimated
# by running selection and outcome equation jointly.
# Additional graphs presented in the paper can be found in the appendix.
#
#
# TABLE OF CONTENTS
#   PREPARATION
#     - Packages
#     - data import
#   A: HOUSING PRICE INDEX (HPI)
#     - model: repeated-sales hybrid
#   B: SELECTION EQUATION
#     - correlations
#     - logit (selection equation)
#     - Goodness of fit
#   C: MATCHING
#     - Matching ATT + AI SE
#     - Building (extracting) weights (for diagnostic metrics)
#   C(i): Global matching diagnostics
#     - Overlap and common support metrics (matched sample)
#     - Rubin's B & R (logit-PS) and ESS (weighted on matched sample)
#   C(ii): Balance check
#     - BEFORE: Unweighted balance table
#     - AFTER: Weighted (matched) balance table
#   D: OUTCOME EQUATION
#   E: FALSIFICATIONS
#     - ATT by treated–control quarter gap (same matches/weights)
#     - Plotting ATT by quarter gap
#     - Within‑quarter WLS diagnostic (same matched sample/weights)
#   APPENDIX
#     - Calibration plot (goodness of fit)
#     - Plotting kernel densities of propensity scores
#   RESULTS SUMMARY
#
# ==============================================================================

# specify model to run (model1, model2 or model3)
MODEL <- "model1" 

results <- list()

# ============================================================================
### PREPARATION
# ============================================================================
# if (!requireNamespace("Hmisc", quietly = TRUE)) install.packages("Hmisc")

# --- Packages ---------------------------------------------------------------
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(fixest)
library(Matching)
library(margins)
library(cobalt)
library(survey)
library(tibble)
library(ggplot2)
library(zoo)
library(Hmisc)


# --- data import ------------------------------------------------------------
# block characteristics
bc  <- read_csv("block_characteristics_prepared.csv", col_types = cols(GEOID = col_character()))

# sales
sm <- st_read("sales_master_prepared.gpkg", layer = "sales_master") %>%
  st_drop_geometry()

# merge; join block characteristics on block GEOID
dat0 <- sm %>% left_join(bc,by = "GEOID")

# remove blocks with no sales
dat0 <- dat0 %>%
  filter(GEOID %in% bc$GEOID) ;  anti_join(dat0, bc, by = "GEOID") %>%
  nrow() # check - 0


# ============================================================================
### A: HOUSING PRICE INDEX (HPI)
# ============================================================================
# --- model: repeated-sales hybrid -------------------------------------------
mhpi <- feols(
  ln_price ~ ln_price_last + years_since_last + lnP_last_x_years +
    ln_lot + ln_floor +
    i(bedrooms_bin, ref = "1") +
    i(bldg_bin,     ref = "<=1900") +
    i(rowhouse_flag, ref = 0) | qdate + tract_id,
  data = dat0
)
summary(mhpi)

if (MODEL != "model3") {
  
  # Quality-adjusted HPI residuals
  dat0$hpi <- dat0$ln_price - predict(mhpi, newdata = dat0)
  dat0 <- dat0 %>%  drop_na(hpi) #3
  
} else {
  ### ADDED PART (to the baseline model) - NEW HPI FOR MODEL 3 ###
  
  # Baseline HPI residual (named hpi_base)
  dat0$hpi_base <- dat0$ln_price - predict(mhpi, newdata = dat0)
  
  # Quarter index (from "YYYYQk"/"YYYY Qk") and PRE flag required by the added block
  qlev <- sort(unique(as.character(dat0$qdate)))
  dat0$t_idx <- match(as.character(dat0$qdate), qlev)
  
  pre_cut <- as.Date("2021-08-09")
  dat0$pre <- dat0$date < pre_cut
  
  # de-trended (adjusted) residual named "hpi"
  dat0 <- dat0 %>%
    group_by(tract_id) %>%
    mutate(
      n_pre = sum(pre),
      # tract-specific anchor time: PRE mean if any PRE rows, else overall mean
      t_ref = if (n_pre[1] > 0) mean(t_idx[pre]) else mean(t_idx),
      # tract-specific slope estimated on PRE only (else 0)
      theta = if (n_pre[1] >= 2 && sd(t_idx[pre]) > 0)
        coef(lm(hpi_base ~ t_idx, subset = pre))[2] else 0,
      # de-trended series
      hpi = hpi_base - theta * (t_idx - t_ref)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-t_ref) %>%
    drop_na(hpi)
  
  ### END OF NEW PART ###
  
}


# ============================================================================
### B: SELECTION EQUATION
# ============================================================================
# --- correlations -----------------------------------------------------------
# correlation matrix
cor2 <- bc %>%
  dplyr::select(where(is.numeric), -starts_with("z")) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3) %>%
  as.data.frame()

# --- logit (selection equation) ---------------------------------------------
form_sel <- treated_any ~
  z_litter_index + z_litter_index_sq +
  z_crime + z_crime_bg +
  z_median_income_bg + z_median_income_bg_sq +
  z_unemployment_rate_bg + z_unemployment_rate_bg_sq +
  z_vac_share +
  z_no_vacant_bg

# fit the logit and save ps
ps_mod <- glm(form_sel, data = dat0, family = binomial())
dat0$ps  <- as.numeric(predict(ps_mod, type = "response"))


# --- Goodness of fit --------------------------------------------------------
# Common support / overlap (unmatched sample)
# proportion of treated/untreated units whose ps lie within the ps range of untreated/treated
t <- dat0$ps[dat0$treated_any==1]; c0 <- dat0$ps[dat0$treated_any==0]
support_t <- mean(t >= min(c0) & t <= max(c0))
support_c <- mean(c0 >= min(t) & c0 <= max(t))
c(Support_Treated = support_t, Support_Control = support_c)

# marginal probability of selection into treatment
# average marginal effects (change in probability per +1 unit; here ≈ +1 SD because we use z scores)
ame_tbl <- summary(margins(ps_mod)) %>%
  filter(factor != "(Intercept)") %>%
  transmute(
    variable = factor,
    AME_pp = 100*AME,        # percentage-point change in P(treat=1)
    SE_pp  = 100*SE,
    z, p,
    stars = case_when(p < .001 ~ "***", p < .01 ~ "**",
                      p < .05 ~ "*", TRUE ~ "")
  ) %>%
  mutate(AME = sprintf("%.2f%s", AME_pp, stars),
         SE  = sprintf("(%.2f)", SE_pp)) %>%
  dplyr::select(variable, AME, SE, z, p)

print(ame_tbl)


# ============================================================================
### C: MATCHING
# ============================================================================
# --- Matching ATT + AI SE ---------------------------------------------------

# 1) Bias-adjustment covariates (Eq. 2 controls, standardized)
vars_fs <- all.vars(formula(form_sel))[-1]  # RHS variables

# exclude the square terms
vars_fs_nosq <- vars_fs[!grepl("_sq$", vars_fs)]

# build the bias-adjustment covariate matrix
Z <- as.matrix(dat0[, vars_fs_nosq])

# 2) Run matching (with replacement, keep all ties and split weight evenly between them)
set.seed(123)

if (MODEL != "model2") {
  
  mt <- Match(Y = dat0$hpi, Tr = dat0$treated_any, X = dat0$ps,
              M = 1, replace = TRUE, ties = TRUE,
              BiasAdjust = TRUE, Z = Z,
              Var.calc = TRUE, estimand = "ATT",
              distance.tolerance = 1e-10)
  
} else {
  
  ### ADDED PART (to the baseline model) - MODEL 2 MATCHING ###
  
  # --- Time-constrained distance: (ps, scaled quarter index) ----------------
  # Quarter index from qdate (format "YYYYQk")
  qlev <- sort(unique(as.character(dat0$qdate)))
  qnum <- match(as.character(dat0$qdate), qlev)
  
  # scaling factor
  lambda <- 0.07 * stats::sd(dat0$ps) / stats::sd(qnum)
  
  # 2D distance
  X2 <- cbind(ps = dat0$ps, tq = lambda * qnum)
  
  mt <- Match(
    Y = dat0$hpi, Tr = dat0$treated_any, X = X2,
    M = 1, replace = TRUE, ties = TRUE,
    BiasAdjust = TRUE, Z = Z,
    Var.calc = TRUE, estimand = "ATT",
    caliper = c(0.02, 2)   # PS caliper, time caliper (as specified)
  )
  ### END OF NEW PART ###
}

# 3) Baseline effect and AI SE
ATT   <- as.numeric(mt$est)
AI_SE <- as.numeric(mt$se)

# ATT / SE
c(ATT = ATT, AI_SE = AI_SE)


# --- Building (extracting) weights (for diagnostic metrics) -----------------
# Build weights from the exact pairs that got matched - first, extract matched pairs
pairs <- tibble(
  treated = mt$index.treated,
  control = mt$index.control
)

# Equal split within each treated's tie set
k_map <- pairs %>% count(treated, name = "k")    # k = number of kept controls for each treated
pairs <- pairs %>% left_join(k_map, by = "treated")

# Weights on the original row index scale of dat0
w <- numeric(nrow(dat0))                                     # create vector of zeros
w[unique(pairs$treated)] <- 1                                # each matched treated gets weight 1
w_ctrl <- pairs %>% count(control, wt = 1 / k, name = "w_c") # controls accumulate 1/k per use
w[w_ctrl$control] <- w_ctrl$w_c

# Analysis sample (exactly the matched rows)
ids  <- sort(unique(c(unique(pairs$treated), unique(pairs$control))))
datm <- dat0[ids, , drop = FALSE]
datm$w <- w[ids]



# ==============================================================================
### C(i): Global matching diagnostics
# ==============================================================================
# --- Overlap and common support metrics (matched sample) ----------------------
# treated within control 1–99% PS; control within treated 1–99% PS (disregarding weights)
t1 <- datm$ps[datm$treated_any==1]; c1 <- datm$ps[datm$treated_any==0]

b_c <- quantile(c1, c(.01,.99)) # without extreme tails
b_t <- quantile(t1,  c(.01,.99))

support_t <- mean(t1  >= b_c[1] & t1  <= b_c[2])
support_c <- mean(c1 >= b_t[1] & c1 <= b_t[2])
support_t; support_c


# treated within control 1–99% PS; control within treated 1–99% PS (with respect to weights)
wt <- w[ids][datm$treated_any == 1]
wc <- w[ids][datm$treated_any == 0]

b_c <- wtd.quantile(c1, weights=wc, probs=c(.01,.99))
b_t <- wtd.quantile(t1,  weights=wt, probs=c(.01,.99))

support_tw <- weighted.mean(t1  >= b_c[1] & t1  <= b_c[2], wt)
support_cw <- weighted.mean(c1 >= b_t[1] & c1 <= b_t[2], wc)
support_tw; support_cw

# # sanity check
# sum(datm$w[datm$treated_any == 1]) ; sum(datm$w[datm$treated_any == 0])


# --- Rubin's B & R (logit-PS) and ESS (weighted on matched sample) ------------
eps <- 1e-6
lt <- qlogis(pmin(pmax(t1, eps), 1 - eps))  # treated logit-PS
lc <- qlogis(pmin(pmax(c1, eps), 1 - eps))  # control logit-PS

# weighted means and variances using logit odds from above
m1 <- weighted.mean(lt, wt);  m0 <- weighted.mean(lc, wc)
v1 <- weighted.mean((lt - m1)^2, wt);  v0 <- weighted.mean((lc - m0)^2, wc)

rubin_B <- abs(m1 - m0) / sqrt(0.5 * (v1 + v0))   # target: < 0.25
rubin_R <- v1 / v0                                 # target: 0.5–2

# effective sample size (ESS)
ESS_t   <- (sum(wt)^2) / sum(wt^2)
ESS_c   <- (sum(wc)^2) / sum(wc^2)
ESS_all <- ((sum(wt) + sum(wc))^2) / sum(c(wt, wc)^2)

# summarise
cat(sprintf("Rubin's B (logit-PS): %.3f | Rubin's R: %.3f | ESS_t: %s | ESS_c: %s | ESS_all: %s\n",
            rubin_B, rubin_R,
            format(round(ESS_t),   big.mark = ",", scientific = FALSE),
            format(round(ESS_c),   big.mark = ",", scientific = FALSE),
            format(round(ESS_all), big.mark = ",", scientific = FALSE)))


# Efficiency = ESS / sum of weights (per group and overall)
sum_wt   <- sum(wt)
sum_wc   <- sum(wc)
sum_wall <- sum(c(wt, wc))

eff_t   <- ESS_t   / sum_wt
eff_c   <- ESS_c   / sum_wc
eff_all <- ESS_all / sum_wall

cat(sprintf("Weight efficiency (ESS/Σw): treated=%.3f | control=%.3f | overall=%.3f\n",
            eff_t, eff_c, eff_all))


# ==============================================================================
### C(ii): Balance check
# ==============================================================================
# Balance check on the variables of interest (below)
vars <- c("litter_index", "litter_index_bg",
          "ba_plus_share_bg", "median_income_bg",
          "homeownership_rate_bg", "vacancy_rate_bg", "crime",
          "res_share", "com_share", "vac_share", "no_vacant_bg",
          "unemployment_rate_bg", "uninsured_share_bg",
          "crime_bg", "com_share_bg", "res_share_bg", "vac_share_bg")


# --- BEFORE: Unweighted balance table -----------------------------------------
# fixed pre-match pooled SD for SMD denominator
sd_pre_ref <- setNames(sapply(vars, function(v) {
  x1 <- dat0[[v]][dat0$treated_any == 1]
  x0 <- dat0[[v]][dat0$treated_any == 0]
  sqrt((var(x1) + var(x0)) / 2)
}), vars)

bal_pre <- purrr::map_dfr(vars, function(v) {
  x1 <- dat0[[v]][dat0$treated_any == 1]
  x0 <- dat0[[v]][dat0$treated_any == 0]
  mt <- mean(x1); mc <- mean(x0)
  vt <- var(x1);  vc <- var(x0)
  tt <- t.test(x1, x0)  # for p-value only
  tibble::tibble(
    var            = v,
    mean_treat_pre = mt,
    mean_ctrl_pre  = mc,
    diff_pre       = mt - mc,
    smd_pre        = (mt - mc) / sd_pre_ref[v],
    var_ratio_pre  = vt / vc,
    p_value_pre    = unname(tt$p.value)
  )
}) %>% dplyr::slice(match(vars, var))

print(bal_pre, n = Inf, width = Inf)


# --- AFTER: Weighted (matched) balance table ----------------------------------
des <- svydesign(ids = ~1, weights = ~w, data = datm)

bal_post <- purrr::map_dfr(vars, function(v) {
  f  <- as.formula(paste0("~", v))
  mt <- unname(coef(svymean(f, subset(des, treated_any == 1))))
  mc <- unname(coef(svymean(f, subset(des, treated_any == 0))))
  vt <- unname(coef(svyvar (f, subset(des, treated_any == 1))))
  vc <- unname(coef(svyvar (f, subset(des, treated_any == 0))))
  tt <- svyttest(as.formula(paste(v, "~ treated_any")), design = des)  # for p-value only
  tibble::tibble(
    var           = v,
    mean_treat_w  = mt,
    mean_ctrl_w   = mc,
    diff_w        = mt - mc,
    smd_w         = (mt - mc) / sd_pre_ref[v],
    var_ratio_w   = vt / vc,
    p_value_w     = unname(tt$p.value)
  )
}) %>% dplyr::slice(match(vars, var))

print(bal_post, n = Inf, width = Inf)


# SMD & VR for the selection covariates (weighted on matched sample)
bal_post <- cobalt::bal.tab(form_sel, data = datm, weights = datm$w,
                            stats = c("m","v"), s.d.denom = "pooled")
post_tbl <- tibble::rownames_to_column(as.data.frame(bal_post$Balance), "var") %>%
  dplyr::select(var, SMD = Diff.Adj, VarRatio = V.Ratio.Adj)
print(tibble::as_tibble(post_tbl), n = Inf)
post_tbl %>%
  summarise(
    mean_SMD = mean(abs(SMD)),
    median_SMD = median(SMD),
    mean_VarRatio = mean(VarRatio),
    median_VarRatio = median(VarRatio)
  )

# # sanity check
# mean(dat0$litter_index[dat0$treated_any == 0], na.rm = TRUE)



# ==============================================================================
### D: OUTCOME EQUATION
# ==============================================================================
# The treatment effect is estimated by estimating eq.2 and eq.3 jointly and is reported
# above. Sensitivity analysis is performed here by allowing SE correlate at qdate
# to address simultaneous activation of the treatment.
fml2 <- as.formula(
  paste("hpi ~ treated_any +", paste(vars_fs_nosq, collapse = " + "))
)

m2 <- feols(
  fml2,
  data    = datm,
  weights = ~ w,
  cluster = ~ qdate
)
summary(m2)


# ==============================================================================
### E: FALSIFICATIONS
# ==============================================================================
# --- ATT by treated–control quarter gap (same matches/weights) ----------------
# Check if the estimated ATT depends on how far in time the treated sale and its matched
# control are form one another (quarter gap)

# Build quarter index from qdate (format "YYYYQk")
qlev <- sort(unique(as.character(dat0$qdate)))
qnum <- match(as.character(dat0$qdate), qlev)

# Pair-level data
dy  <- dat0$hpi[pairs$treated] - dat0$hpi[pairs$control]
gap <- qnum[pairs$treated]      - qnum[pairs$control]
w_p <- 1 / pairs$k

# ATT by quarter gap and weights table
ATT_by_gap <- tibble(gap = gap, dy = dy, w = w_p) %>%
  dplyr::group_by(gap) %>%
  dplyr::summarise(
    att = Hmisc::wtd.mean(dy, w),  # weighted mean within gap
    wsum    = sum(w),                   # total matched weight in this gap
    n_pairs = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(gap)

print(ATT_by_gap, n = 50)

# "Weighted" slope of the fitted line ATT vs quarter gap
slope_fit_w <- lm(att ~ gap, data = ATT_by_gap, weights = wsum)
summary(slope_fit_w)$coef["gap", c("Estimate","Std. Error","Pr(>|t|)")]

# the ATT increases with gap, indicating a positive pre-trend in the treated areas
# (compared to the untreated areas). The mean quarter gap is about ~13. This means that the
# overall estimated ATT from matching is biased, if pre-trends are not addressed.

# mean weighted gap
wmean_abs_gap <- sum(ATT_by_gap$wsum * abs(ATT_by_gap$gap)) / sum(ATT_by_gap$wsum)
wmean_gap     <- sum(ATT_by_gap$wsum * ATT_by_gap$gap)       / sum(ATT_by_gap$wsum)

c(weighted_mean_abs_gap = wmean_abs_gap,
  weighted_mean_gap     = wmean_gap)


# --- Plotting ATT by quarter gap ----------------------------------------------
# weight share
ATT_by_gap$wshare <- ATT_by_gap$wsum / sum(ATT_by_gap$wsum)

# fixed bins
size_breaks_share <- c(0, 0.02, 0.05, 0.10, 0.20, Inf)   # <2%, 2–5%, 5–10%, 10–20%, ≥20%
size_labels       <- c("<2%", "2–5%", "5–10%", "10–20%", "≥20%")
size_values_mm    <- c(1.5, 3, 5, 8, 11)                 # five circle sizes (mm)

ATT_by_gap$wbin <- cut(
  ATT_by_gap$wshare,
  breaks = size_breaks_share,
  labels = size_labels,
  include.lowest = TRUE, right = FALSE
)

# Plot
ggplot(ATT_by_gap, aes(gap, att)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(size = wbin)) +
  geom_smooth(method = "lm", se = FALSE, aes(weight = wsum)) +
  scale_size_manual(
    values = size_values_mm,
    breaks = size_labels,
    limits = size_labels,     # ensures same legend order even if some bins are empty
    drop   = FALSE,
    name   = "Gap weight share"
  ) +
  labs(title = "ATT by Treated–Control Quarter Gap",
       x = "Quarter gap (treated − control)", y = "ATT") +
  theme_minimal()


# --- Within‑quarter WLS diagnostic (same matched sample/weights) --------------
datm$qdate_d <- as.Date(as.yearqtr(as.character(datm$qdate), format = "%YQ%q"))
m_wls0 <- fixest::feols(
  fml = fml2,
  fixef = "qdate_d",
  data = datm,
  weights = ~ w,
  cluster = "qdate_d"
)
fixest::etable(m_wls0)


# ==============================================================================
### APPENDIX
# ==============================================================================
# --- Calibration plot (goodness of fit) ---------------------------------------
dat0 %>%
  mutate(ps_decile = ntile(ps, 10)) %>%
  group_by(ps_decile) %>%
  summarise(
    treated_share = mean(treated_any),
    mean_ps = mean(ps)
  ) %>%
  ggplot(aes(x = mean_ps, y = treated_share)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Mean Predicted Propensity", y = "Observed Treatment Share",
       title = "Propensity Score Calibration by Decile") +
  theme_minimal()


# --- Plotting kernel densities of propensity scores ---------------------------
# Readable legend labels
lab_treat <- function(x) factor(x, levels = c(0,1), labels = c("Control","Treated"))

# Unmatched
p_ps_unmatched <- dat0 %>%
  dplyr::mutate(group = lab_treat(treated_any)) %>%
  ggplot(aes(x = ps, colour = group, linetype = group)) +
  stat_density(geom = "line", adjust = 1, na.rm = TRUE) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(
    title = "Kernel density of propensity scores — Unmatched sample",
    x = "Propensity score", y = "Kernel density", colour = NULL, linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Lock y-scale from the UNMATCHED plot and reuse for the matched plot
gb_unmatched   <- ggplot_build(p_ps_unmatched)
ymax_unmatched <- max(unlist(lapply(gb_unmatched$data, function(d) max(d$y))))
ymax_common    <- ymax_unmatched * 1.02  # tiny headroom to avoid clipping

# Apply the common y-limit to the unmatched plot itself
p_ps_unmatched <- p_ps_unmatched +
  scale_y_continuous(limits = c(0, ymax_common), expand = expansion(mult = 0))

# Matched
p_ps_matched <- datm %>%
  dplyr::mutate(group = lab_treat(treated_any)) %>%
  ggplot(aes(x = ps, colour = group, linetype = group, weight = w)) +
  stat_density(geom = "line", adjust = 1, na.rm = TRUE) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, ymax_common), expand = expansion(mult = 0)) +
  labs(
    title = "Kernel density of propensity scores — Matched sample",
    x = "Propensity score", y = "Kernel density", colour = NULL, linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Print
print(p_ps_unmatched)
print(p_ps_matched)


# ==============================================================================
# RESULTS SUMMARY
# ==============================================================================
results <- append(results,list(list(ATT = ATT, AI_SE = AI_SE)))

print(do.call(
  rbind,
  lapply(results, as.data.frame)
))
