# =============================================================================
# 02_reliability_and_factor_structure.R – Alpha, EFA, CFA, PCA
# =============================================================================
#
# GOAL: Run reliability (Cronbach's alpha), exploratory factor analysis (EFA),
#       confirmatory factor analysis (CFA), and PCA on the 12 LPH items. Add
#       PCA first-component score to the dataset and save the analysis dataset
#       (cleaned + pca1_score) for downstream scripts. Save reliability and
#       factor-structure results for reporting.
#
# INSTRUCTIONS:
#   1. Run 01_cleaning.R first so output/learned_helplessness_cleaned.csv exists.
#   2. Working directory = folder containing 00_config.R and this script.
#   3. Run this script. Outputs: output/learned_helplessness_analysis_data.csv,
#      output/02_reliability_factor_results.rds, and console/report text.
#
# INPUT:  output/learned_helplessness_cleaned.csv (from 01_cleaning.R)
# OUTPUT: output/learned_helplessness_analysis_data.csv, output/02_reliability_factor_results.rds
# =============================================================================

if (file.exists("00_config.R")) source("00_config.R")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

library(dplyr)
library(psych)
library(lavaan)

# Path to cleaned data (from 01)
cleaned_path <- file.path(output_dir, "learned_helplessness_cleaned.csv")
if (!file.exists(cleaned_path)) stop("Run 01_cleaning.R first. Missing: ", cleaned_path)

dat_main <- read.csv(cleaned_path, stringsAsFactors = FALSE)
message("Loaded cleaned data: ", nrow(dat_main), " rows from ", cleaned_path)

# Column lists (must match 01)
cog_cols <- paste0("Q24_", 1:5)
emo_cols <- paste0("Q26_", 1:6)
item_cols <- c(cog_cols, emo_cols)

# -----------------------------------------------------------------------------
# 1. Reliability: Cronbach's alpha
# -----------------------------------------------------------------------------
cog_items <- dat_main[, cog_cols, drop = FALSE]
emo_items <- dat_main[, emo_cols, drop = FALSE]
# Ensure numeric
for (j in seq_len(ncol(cog_items))) cog_items[[j]] <- as.numeric(cog_items[[j]])
for (j in seq_len(ncol(emo_items))) emo_items[[j]] <- as.numeric(emo_items[[j]])

# Keep only columns with variance so psych::alpha() doesn't error
has_var_cog <- sapply(cog_items, function(x) sd(x, na.rm = TRUE) > 0)
has_var_emo <- sapply(emo_items, function(x) sd(x, na.rm = TRUE) > 0)
cog_for_alpha <- cog_items[, has_var_cog, drop = FALSE]
emo_for_alpha <- emo_items[, has_var_emo, drop = FALSE]

if (ncol(cog_for_alpha) < 2 || ncol(emo_for_alpha) < 2) {
  stop("Reliability: need at least 2 items with variance per subscale. ",
       "Cognitive with variance: ", ncol(cog_for_alpha), "; emotional: ", ncol(emo_for_alpha), ". ",
       "Check that cleaned data has numeric item columns (Q24_1..5, Q26_1..6) with variation.")
}
if (nrow(cog_for_alpha) < 2) {
  stop("Reliability: need at least 2 rows. Check cleaned data.")
}

alpha_cog <- psych::alpha(cog_for_alpha, check.keys = FALSE)
alpha_emo <- psych::alpha(emo_for_alpha, check.keys = FALSE)

message("\n--- RELIABILITY ---")
message("Cognitive alpha = ", round(alpha_cog$total$raw_alpha, 3), " (", ncol(cog_for_alpha), " items with variance)")
message("Emotional alpha = ", round(alpha_emo$total$raw_alpha, 3), " (", ncol(emo_for_alpha), " items with variance)")
if (ncol(cog_for_alpha) < ncol(cog_items)) message("Note: ", ncol(cog_items) - ncol(cog_for_alpha), " cognitive item(s) had no variance and were omitted from alpha.")
if (ncol(emo_for_alpha) < ncol(emo_items)) message("Note: ", ncol(emo_items) - ncol(emo_for_alpha), " emotional item(s) had no variance and were omitted from alpha.")
message("Alpha if item deleted (cognitive):"); print(alpha_cog$alpha.drop[, c("raw_alpha", "std.alpha")])
message("Alpha if item deleted (emotional):"); print(alpha_emo$alpha.drop[, c("raw_alpha", "std.alpha")])

# -----------------------------------------------------------------------------
# 2. EFA: parallel analysis and 1- vs 2-factor solutions
# -----------------------------------------------------------------------------
message("\n--- EFA ---")
fa_parallel <- psych::fa.parallel(dat_main[, item_cols], fm = "ml", fa = "fa", n.iter = 50)
efa1 <- psych::fa(dat_main[, item_cols], nfactors = 1, rotate = "oblimin", fm = "ml")
efa2 <- psych::fa(dat_main[, item_cols], nfactors = 2, rotate = "oblimin", fm = "ml")
message("EFA 1-factor loadings (cutoff .30):"); print(efa1$loadings, cutoff = 0.30)
message("EFA 2-factor loadings (cutoff .30):"); print(efa2$loadings, cutoff = 0.30)

# -----------------------------------------------------------------------------
# 3. CFA: 1-factor vs 2-factor models
# -----------------------------------------------------------------------------
message("\n--- CFA ---")
cog_part <- paste(cog_cols, collapse = " + ")
emo_part <- paste(emo_cols, collapse = " + ")
model_1f <- paste0("helplessness =~ ", paste(item_cols, collapse = " + "))
model_2f <- paste0("cognitive =~ ", cog_part, "\n", "emotional =~ ", emo_part, "\n", "cognitive ~~ emotional")
fit_1f <- lavaan::cfa(model_1f, data = dat_main, std.lv = TRUE)
fit_2f <- lavaan::cfa(model_2f, data = dat_main, std.lv = TRUE)
fit_indices <- c("cfi", "tli", "rmsea", "srmr")
message("1-factor fit:"); print(lavaan::fitmeasures(fit_1f, fit_indices))
message("2-factor fit:"); print(lavaan::fitmeasures(fit_2f, fit_indices))

# -----------------------------------------------------------------------------
# 4. PCA and add PC1 score to dataset
# -----------------------------------------------------------------------------
message("\n--- PCA ---")
pca_fit <- prcomp(dat_main[, item_cols], center = TRUE, scale. = TRUE)
print(summary(pca_fit))
dat_main$pca1_score <- pca_fit$x[, 1]

# -----------------------------------------------------------------------------
# 5. Save analysis dataset (cleaned + pca1_score) and results
# -----------------------------------------------------------------------------
write.csv(dat_main, file.path(output_dir, "learned_helplessness_analysis_data.csv"), row.names = FALSE)

results_02 <- list(
  alpha_cognitive = alpha_cog$total$raw_alpha,
  alpha_emotional = alpha_emo$total$raw_alpha,
  alpha_cog_drop = alpha_cog$alpha.drop,
  alpha_emo_drop = alpha_emo$alpha.drop,
  efa_1f_loadings = efa1$loadings,
  efa_2f_loadings = efa2$loadings,
  cfa_1f_fit = as.list(lavaan::fitmeasures(fit_1f, fit_indices)),
  cfa_2f_fit = as.list(lavaan::fitmeasures(fit_2f, fit_indices)),
  pca_variance = summary(pca_fit)$importance["Proportion of Variance", ]
)
saveRDS(results_02, file.path(output_dir, "02_reliability_factor_results.rds"))

message("\nSaved: ", file.path(output_dir, "learned_helplessness_analysis_data.csv"))
message("Saved: ", file.path(output_dir, "02_reliability_factor_results.rds"))
