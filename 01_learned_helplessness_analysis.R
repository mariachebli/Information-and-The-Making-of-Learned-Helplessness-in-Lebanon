# =============================================================================
# LEARNED POLITICAL HELPLESSNESS - ANALYSIS SCRIPT (REAL DATA)
# =============================================================================
# For: Maria (honors) - Lebanon. Runs on REAL Qualtrics data: reads CSV from
# 00_config.R, drops header rows, recodes items (5 cognitive + 6 emotional),
# then reliability, EFA, CFA, scoring, and validity analyses.
# =============================================================================

library(dplyr)
library(tidyr)
library(psych)
library(lavaan)

# ---- Load config and read real data ----
if (file.exists("00_config.R")) source("00_config.R") else {
  data_file <- "~/LocalWork/Consults/maria_honors/Learned Political Helplessness in Lebanon_March 5, 2026_19.17.csv"
  allow_preview_for_testing <- FALSE
}
if (!file.exists(data_file)) stop("Data file not found: ", data_file)
raw <- read.csv(data_file, check.names = TRUE, stringsAsFactors = FALSE)
raw <- raw[-c(1, 2), ]
dat <- raw %>% filter(Finished %in% c("1", "True"))
if (!allow_preview_for_testing && "Status" %in% names(dat)) dat <- dat %>% filter(!as.character(Status) %in% c("Survey Preview", "Survey Test", "Response Type"))
message("Finished responses: N = ", nrow(dat))
if (nrow(dat) == 0) stop("No rows left. Check data file and Finished.")

new_format <- "state_efficacy_1" %in% names(dat)
if (new_format) {
  to_num <- function(x) { x <- as.character(x); x[x == ""] <- NA; as.numeric(x) }
  for (col in c(paste0("state_efficacy_", 1:5), paste0("emotional_indicator_", 1:6), paste0("collec_action_likely_", 1:5),
                paste0("info_availability_", 1:4), paste0("info_credibility_", 1:4), paste0("media_crisis_cred_", 1:3),
                paste0("tv_channels_freq_", 1:9), paste0("newspaper_freq_", 1:8), "whatsapp_freq_1", paste0("social_media_freq_", 1:6))) {
    if (col %in% names(dat)) dat[[col]] <- to_num(dat[[col]])
  }
  for (i in 1:5) dat[[paste0("Q24_", i)]] <- dat[[paste0("state_efficacy_", i)]]
  for (i in 1:6) dat[[paste0("Q26_", i)]] <- dat[[paste0("emotional_indicator_", i)]]
  for (i in 1:5) dat[[paste0("Q31_", i)]] <- dat[[paste0("collec_action_likely_", i)]]
  for (i in 1:4) dat[[paste0("Q13_", i)]] <- dat[[paste0("info_availability_", i)]]
  for (i in 1:4) dat[[paste0("Q12_", i)]] <- dat[[paste0("info_credibility_", i)]]
  for (i in 1:3) dat[[paste0("Q15_", i)]] <- dat[[paste0("media_crisis_cred_", i)]]
  media_cols_new <- c(paste0("tv_channels_freq_", 1:9), paste0("newspaper_freq_", 1:8), "whatsapp_freq_1", paste0("social_media_freq_", 1:6))
  media_cols_new <- media_cols_new[media_cols_new %in% names(dat)]
}

# ---- Recode functions (Qualtrics text -> numeric) ----
recode_agree4 <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  out[tolower(x) %in% c("strongly disagree")] <- 1
  out[tolower(x) %in% c("somewhat disagree")] <- 2
  out[tolower(x) %in% c("somewhat agree")] <- 3
  out[tolower(x) %in% c("strongly agree")] <- 4
  out
}
recode_freq4 <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  out[tolower(x) %in% c("often", "very often")] <- 1
  out[tolower(x) %in% c("sometimes")] <- 2
  out[tolower(x) %in% c("rarely")] <- 3
  out[tolower(x) %in% c("never")] <- 4
  out
}
recode_likely4 <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  out[tolower(x) %in% c("extremely unlikely")] <- 1
  out[tolower(x) %in% c("somewhat unlikely")] <- 2
  out[tolower(x) %in% c("somewhat likely")] <- 3
  out[tolower(x) %in% c("extremely likely")] <- 4
  out
}
recode_media_freq5 <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  out[tolower(x) %in% c("never")] <- 1
  out[tolower(x) %in% c("once a week")] <- 2
  out[tolower(x) %in% c("2-3 times a week", "2–3 times a week")] <- 3
  out[tolower(x) %in% c("4-6 times a week", "4–6 times a week")] <- 4
  out[tolower(x) %in% c("daily")] <- 5
  out
}

# ---- Qualtrics column names: 5 cog (Q24_1..5), 6 emo (Q26_1..6) ----
Q_cog <- paste0("Q24_", 1:5)
Q_emo <- paste0("Q26_", 1:6)
Q_ca <- paste0("Q31_", 1:5)
info_avail_cols <- paste0("Q13_", 1:4)
crisis_norm_cols <- paste0("Q15_", 1:3)
trust_cols <- paste0("Q12_", 1:4)
if (!new_format) {
  misinfo_cols <- paste0("Q14_", 1:4)
  tv_cols <- paste0("Q9_", 1:9)
  press_cols <- paste0("Q10_", 1:8)
  whatsapp_cols <- "Q11_1"
  media_cols <- c(tv_cols, press_cols, whatsapp_cols)
  for (col in c(Q_cog, info_avail_cols, misinfo_cols, crisis_norm_cols, trust_cols)) {
    if (col %in% names(dat)) dat[[col]] <- recode_agree4(dat[[col]])
  }
  for (col in Q_emo) if (col %in% names(dat)) dat[[col]] <- recode_freq4(dat[[col]])
  for (col in Q_ca) if (col %in% names(dat)) dat[[col]] <- recode_likely4(dat[[col]])
  for (col in media_cols) if (col %in% names(dat)) dat[[col]] <- recode_media_freq5(dat[[col]])
} else {
  misinfo_cols <- character(0)
}

# Listwise on 12 LPH items; reverse emotional except Q26_5 (hopeful)
item_cols_q <- c(Q_cog, Q_emo)
dat <- dat[complete.cases(dat[, item_cols_q[item_cols_q %in% names(dat)]]), ]
message("Sample after listwise on 12 helplessness items: N = ", nrow(dat))
for (col in setdiff(Q_emo, "Q26_5")) if (col %in% names(dat)) dat[[col]] <- 5 - dat[[col]]

# Rename to cog1..cog5, emo1..emo6 for rest of script
for (i in seq_along(Q_cog)) dat[[paste0("cog", i)]] <- dat[[Q_cog[i]]]
for (i in seq_along(Q_emo)) dat[[paste0("emo", i)]] <- dat[[Q_emo[i]]]
cog_cols <- paste0("cog", 1:5)
emo_cols <- paste0("emo", 1:6)
item_cols <- c(cog_cols, emo_cols)

# Composites
dat$collective_action_intent <- if (all(Q_ca %in% names(dat))) rowMeans(dat[, Q_ca], na.rm = TRUE) else NA_real_
dat$info_availability <- if (all(info_avail_cols %in% names(dat))) rowMeans(dat[, info_avail_cols], na.rm = TRUE) else NA_real_
dat$crisis_normalizing <- if (all(crisis_norm_cols %in% names(dat))) rowMeans(dat[, crisis_norm_cols], na.rm = TRUE) else NA_real_
dat$info_credibility <- if (all(trust_cols %in% names(dat))) rowMeans(dat[, trust_cols], na.rm = TRUE) else NA_real_
if (new_format && length(media_cols_new) > 0) {
  dat$media_consumption <- rowMeans(dat[, media_cols_new], na.rm = TRUE)
} else if (!new_format && length(media_cols) > 0 && all(media_cols %in% names(dat))) {
  dat$media_consumption <- rowMeans(dat[, media_cols], na.rm = TRUE)
} else {
  dat$media_consumption <- NA_real_
}
if (length(misinfo_cols) > 0 && all(misinfo_cols %in% names(dat))) {
  dat$misinfo_exposure <- rowMeans(dat[, misinfo_cols], na.rm = TRUE)
} else {
  dat$misinfo_exposure <- NA_real_
}
if (new_format && "perso_2019" %in% names(dat)) {
  dat$collective_action_past <- dplyr::case_when(dat$perso_2019 == "1" | dat$perso_2019 == 1 ~ 1, dat$perso_2019 == "2" | dat$perso_2019 == 2 ~ 0, TRUE ~ NA_real_)
} else if ("Q34" %in% names(dat)) {
  dat$collective_action_past <- dplyr::case_when(
    dat$Q34 %in% c("Yes, I participated in person in Lebanon", "Yes, I participated through online engagement", "Yes, I participated in person abroad") ~ 1,
    dat$Q34 == "No, I did not participate" ~ 0, TRUE ~ NA_real_)
} else {
  dat$collective_action_past <- NA_real_
}

# Ensure numeric for regressions
for (col in c("collective_action_intent", "collective_action_past", "info_availability", "info_credibility", "misinfo_exposure", "media_consumption")) {
  if (col %in% names(dat)) dat[[col]] <- as.numeric(as.character(dat[[col]]))
}


# -----------------------------------------------------------------------------
# RELIABILITY: Cronbach's alpha for each subscale
# -----------------------------------------------------------------------------
# Alpha tells us how well the items in a scale "hang together" (internal
# consistency). Rule of thumb: alpha > 0.70 is acceptable, > 0.80 is good.
# We also look at "alpha if item deleted" and item-total correlation: if
# removing one item would raise alpha a lot, that item may not belong.
# psych::alpha() fails if items have no variance, so we drop zero-variance
# columns before calling it and require at least 2 items per scale.

cog_items <- dat[, cog_cols, drop = FALSE]
emo_items <- dat[, emo_cols, drop = FALSE]
# Ensure numeric
for (j in seq_len(ncol(cog_items))) cog_items[[j]] <- as.numeric(cog_items[[j]])
for (j in seq_len(ncol(emo_items))) emo_items[[j]] <- as.numeric(emo_items[[j]])

# Keep only columns with variance (sd > 0) so alpha() doesn't error
has_var_cog <- sapply(cog_items, function(x) sd(x, na.rm = TRUE) > 0)
has_var_emo <- sapply(emo_items, function(x) sd(x, na.rm = TRUE) > 0)
cog_for_alpha <- cog_items[, has_var_cog, drop = FALSE]
emo_for_alpha <- emo_items[, has_var_emo, drop = FALSE]

if (ncol(cog_for_alpha) < 2 || ncol(emo_for_alpha) < 2) {
  stop("Reliability: need at least 2 items with variance per subscale. ",
       "Cognitive with variance: ", ncol(cog_for_alpha), "; emotional: ", ncol(emo_for_alpha), ". ",
       "Run the full script from the top (data load + recode) so cog/emo items are numeric 1-4, or check your data file.")
}
if (nrow(cog_for_alpha) < 2) {
  stop("Reliability: need at least 2 rows. Run the full script from the top so data are loaded and listwise deletion is applied.")
}

alpha_cog <- psych::alpha(cog_for_alpha, check.keys = FALSE)
alpha_emo <- psych::alpha(emo_for_alpha, check.keys = FALSE)

message("\n--- RELIABILITY ---")
message("Cognitive subscale alpha = ", round(alpha_cog$total$raw_alpha, 3), " (", ncol(cog_for_alpha), " items with variance)")
message("Emotional subscale alpha = ", round(alpha_emo$total$raw_alpha, 3), " (", ncol(emo_for_alpha), " items with variance)")
if (ncol(cog_for_alpha) < ncol(cog_items)) message("Note: ", ncol(cog_items) - ncol(cog_for_alpha), " cognitive item(s) had no variance and were omitted from alpha.")
if (ncol(emo_for_alpha) < ncol(emo_items)) message("Note: ", ncol(emo_items) - ncol(emo_for_alpha), " emotional item(s) had no variance and were omitted from alpha.")

# Alpha if item deleted (and item-total corr) are inside the alpha object
print(alpha_cog$alpha.drop)
print(alpha_emo$alpha.drop)


# -----------------------------------------------------------------------------
# EFA: EXPLORATORY FACTOR ANALYSIS
# -----------------------------------------------------------------------------
# EFA asks: how many factors do the items empirically form? We use oblique
# rotation (allowed factors to correlate) because cognitive and emotional
# helplessness are likely related. Rules of thumb: loadings > 0.4 on one
# factor and much smaller on others = item belongs to that factor; big
# cross-loadings = item may tap both constructs.

# Parallel analysis: compares your eigenvalues to random data to suggest
# how many factors to retain. Keep factors where your eigenvalue is above
# the 95th percentile of random eigenvalues.
fa_parallel <- psych::fa.parallel(dat[, item_cols], fm = "ml", fa = "fa", n.iter = 50)
# fa_parallel tells you "Suggested number of factors"

# EFA with 1 factor (all 12 items)
efa1 <- psych::fa(dat[, item_cols], nfactors = 1, rotate = "oblimin", fm = "ml")
print(efa1$loadings, cutoff = 0.3)

# EFA with 2 factors (oblique rotation)
efa2 <- psych::fa(dat[, item_cols], nfactors = 2, rotate = "oblimin", fm = "ml")
print(efa2$loadings, cutoff = 0.3)
# Interpret: which items load on factor 1 vs 2? Do they match cog vs emo?


# -----------------------------------------------------------------------------
# PCA: PRINCIPAL COMPONENTS ANALYSIS
# -----------------------------------------------------------------------------
# PCA is NOT a measurement model. It just finds linear combinations of items
# that explain the most variance. Useful as a robustness check or to get a
# single "index" score, but it does not assume latent factors. We run it on
# all 12 items, look at variance explained by PC1, and create a PC1 score.

pca_fit <- prcomp(dat[, item_cols], center = TRUE, scale. = TRUE)
summary(pca_fit)  # Proportion of Variance for PC1, PC2, ...
dat$pca1_score <- pca_fit$x[, 1]


# -----------------------------------------------------------------------------
# CFA: CONFIRMATORY FACTOR ANALYSIS
# -----------------------------------------------------------------------------
# CFA tests a pre-specified structure. We fit (1) one factor for all 12 items,
# (2) two correlated factors (cog vs emo). Then we compare fit indices.
# Fit indices: CFI/TLI > 0.95 good, > 0.90 acceptable; RMSEA < 0.06 good,
# < 0.08 acceptable; SRMR < 0.08 good. "Fit" means the model's predicted
# covariances match the data reasonably well.

model_1f <- "
  helplessness =~ cog1 + cog2 + cog3 + cog4 + cog5 + emo1 + emo2 + emo3 + emo4 + emo5 + emo6
"
model_2f <- "
  cognitive =~ cog1 + cog2 + cog3 + cog4 + cog5
  emotional =~ emo1 + emo2 + emo3 + emo4 + emo5 + emo6
  cognitive ~~ emotional
"

fit_1f <- cfa(model_1f, data = dat, std.lv = TRUE)
fit_2f <- cfa(model_2f, data = dat, std.lv = TRUE)

message("\n--- CFA FIT INDICES ---")
message("1-factor model:")
print(fitmeasures(fit_1f, c("cfi", "tli", "rmsea", "srmr")))
message("2-factor model:")
print(fitmeasures(fit_2f, c("cfi", "tli", "rmsea", "srmr")))
# Typically 2-factor will fit better if cognitive and emotional are distinct.


# -----------------------------------------------------------------------------
# SCORING: subscale means, z-scores, combined index
# -----------------------------------------------------------------------------
# cognitive_score = mean of 5 cog items, emotional_score = mean of 6 emo items.
# Then we z-score each (subtract mean, divide by SD) so they are on the same
# scale. Combined_helplessness = mean of the two z-scores. Z-scoring lets us
# average two subscales that may have different means/ranges.

dat <- dat %>%
  mutate(
    cognitive_score = rowMeans(select(., all_of(cog_cols))),
    emotional_score = rowMeans(select(., all_of(emo_cols)))
  )
# Z-score: center (subtract mean) and scale (divide by SD)
dat$z_cog <- scale(dat$cognitive_score)[, 1]
dat$z_emo <- scale(dat$emotional_score)[, 1]
dat$combined_helplessness <- (dat$z_cog + dat$z_emo) / 2


# -----------------------------------------------------------------------------
# UTILITY ANALYSES: incremental validity and distinct prediction
# -----------------------------------------------------------------------------
# Incremental validity: does adding the second subscale improve prediction of
# outcomes beyond the first? We regress collective_action_intent on (1) cog
# only, (2) emo only, (3) both. Compare R-squared; the increase when adding
# the second is "incremental validity."
#
# Distinct prediction: do information-environment variables predict cognitive
# vs emotional helplessness differently? We run two regressions: cog_score
# on info vars, emo_score on info vars. Different patterns support utility of
# separating the two subscales.

# Only run if we have enough complete cases on outcome and subscale scores.
# (We do not require info_availability here, so incremental validity runs whenever
# collective_action_intent and both subscales are present; add info_availability
# to the formula later if the variable has sufficient non-NA values.)
need_vars <- c("collective_action_intent", "cognitive_score", "emotional_score")
need_vars <- need_vars[need_vars %in% names(dat)]
valid_n <- if (length(need_vars) >= 3) sum(complete.cases(dat[, need_vars])) else 0
if (valid_n >= 10) {
  message("\n--- INCREMENTAL VALIDITY (collective_action_intent), N = ", valid_n, " ---")
  mod_cog_only <- lm(collective_action_intent ~ cognitive_score, data = dat)
  mod_emo_only <- lm(collective_action_intent ~ emotional_score, data = dat)
  mod_both     <- lm(collective_action_intent ~ cognitive_score + emotional_score, data = dat)
  summary(mod_cog_only)
  summary(mod_emo_only)
  summary(mod_both)
} else {
  message("\n--- INCREMENTAL VALIDITY --- Skipped (only ", valid_n, " rows with non-NA collective_action_intent + cognitive_score + emotional_score; check Q31 recoding if needed).")
  mod_cog_only <- mod_emo_only <- mod_both <- NULL
}

# Distinct prediction: do info vars predict cog vs emo differently?
# Only run if we have enough complete cases on outcome + all info vars
info_vars <- c("info_availability", "info_credibility", "misinfo_exposure", "media_consumption")
info_vars <- info_vars[info_vars %in% names(dat)]
# Use only predictors with enough non-NA (misinfo_exposure may be absent in new export)
info_vars_use <- info_vars[sapply(dat[info_vars], function(x) sum(!is.na(x))) >= 20]
n_cog_info <- if (length(info_vars_use) >= 2) sum(complete.cases(dat[, c("cognitive_score", info_vars_use)])) else 0
n_emo_info <- if (length(info_vars_use) >= 2) sum(complete.cases(dat[, c("emotional_score", info_vars_use)])) else 0
if (n_cog_info >= 20 && n_emo_info >= 20) {
  message("\n--- DISTINCT PREDICTION (info environment -> subscales) ---")
  mod_cog_info <- lm(cognitive_score ~ ., data = dat[, c("cognitive_score", info_vars_use)])
  mod_emo_info <- lm(emotional_score ~ ., data = dat[, c("emotional_score", info_vars_use)])
  summary(mod_cog_info)
  summary(mod_emo_info)
} else {
  message("\n--- DISTINCT PREDICTION --- Skipped (complete cases on info vars: cognitive = ", n_cog_info, ", emotional = ", n_emo_info, "; need >= 20).")
  mod_cog_info <- mod_emo_info <- NULL
}
# If different predictors are significant for cog vs emo, that supports keeping two subscales.

# Note on causation: this is cross-sectional. Be careful not to claim mediation
# or causation without longitudinal or experimental design. You can describe
# "associations" and "unique prediction" without implying direction.


# -----------------------------------------------------------------------------
# PRINT CLEAN SUMMARY
# -----------------------------------------------------------------------------
message("\n========== SUMMARY ==========")
message("Reliabilities: Cognitive alpha = ", round(alpha_cog$total$raw_alpha, 3),
        "; Emotional alpha = ", round(alpha_emo$total$raw_alpha, 3))
message("Factor structure: Check EFA loadings and CFA fit (2-factor typically better).")
if (!is.null(mod_both)) {
  message("Incremental validity: models mod_cog_only, mod_emo_only, mod_both saved; use summary(mod_both) etc. to save results.")
} else {
  message("Incremental validity: Skipped (insufficient collective_action_intent data).")
}
if (!is.null(mod_cog_info)) {
  message("Distinct prediction: mod_cog_info, mod_emo_info saved; use summary(mod_cog_info), summary(mod_emo_info) to save results.")
} else {
  message("Distinct prediction: Skipped (insufficient info-environment data).")
}
message("=============================")
