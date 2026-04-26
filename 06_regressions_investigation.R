# =============================================================================
# 06_regressions_investigation.R – Descriptives, correlations, simple regressions
# =============================================================================
# Run after 01–04. No new files—just checks that variables look right and prints
# bivariate relations (each IV → DV, each IV → M, each M → DV). Use before
# over-interpreting the mediation. In our run, crisis_normalizing was the IV
# that predicted helplessness; helplessness → collective action was weak.
# =============================================================================

if (file.exists("00_config.R")) source("00_config.R")
analysis_path <- file.path(output_dir, "learned_helplessness_analysis_data.csv")
if (!file.exists(analysis_path)) stop("Run 01_cleaning.R and 02 first. Missing: ", analysis_path)

dat <- read.csv(analysis_path, stringsAsFactors = FALSE)
message("Loaded ", nrow(dat), " rows from ", analysis_path)

# ---- Key variables ----
# Outcomes
dv_col          <- "collective_action_intent"
mediator_cols   <- c("cognitive_score", "emotional_score", "combined_helplessness")
# Info environment (potential IVs)
iv_cols         <- c("info_availability", "info_credibility", "crisis_normalizing", "media_consumption")
all_vars        <- c(dv_col, mediator_cols, iv_cols)
# Keep only columns that exist and have some variance
all_vars        <- all_vars[all_vars %in% names(dat)]

# -----------------------------------------------------------------------------
# 1. Descriptives (check variables were created correctly)
# -----------------------------------------------------------------------------
message("\n========== 1. DESCRIPTIVES (check scales and N) ==========")
for (v in all_vars) {
  x <- dat[[v]]
  x <- as.numeric(x)
  n_ok <- sum(!is.na(x))
  if (n_ok == 0) {
    message(v, ": all NA")
    next
  }
  message(v, ": N = ", n_ok, ", mean = ", round(mean(x, na.rm = TRUE), 3),
          ", SD = ", round(sd(x, na.rm = TRUE), 3),
          ", min = ", round(min(x, na.rm = TRUE), 3),
          ", max = ", round(max(x, na.rm = TRUE), 3))
}

# -----------------------------------------------------------------------------
# 2. Correlation matrix (all key variables)
# -----------------------------------------------------------------------------
message("\n========== 2. CORRELATIONS (Pearson) ==========")
use <- dat[, all_vars]
use <- as.data.frame(lapply(use, as.numeric))
R <- cor(use, use = "pairwise.complete.obs")
print(round(R, 3))
# Highlight correlations with collective_action_intent
if (dv_col %in% colnames(R)) {
  message("\nCorrelations with ", dv_col, ":")
  print(round(R[dv_col, ], 3))
}

# -----------------------------------------------------------------------------
# 3. Simple regressions: each IV → collective_action_intent (DV)
# -----------------------------------------------------------------------------
message("\n========== 3. SIMPLE REGRESSIONS: Info environment → collective_action_intent ==========")
for (iv in iv_cols) {
  if (!iv %in% names(dat)) next
  f <- as.formula(paste0(dv_col, " ~ ", iv))
  m <- lm(f, data = dat)
  message("\n--- ", iv, " → ", dv_col, " (N = ", m$df.residual + length(coef(m)), ") ---")
  print(summary(m))
}

# -----------------------------------------------------------------------------
# 4. Simple regressions: each IV → combined_helplessness (mediator)
# -----------------------------------------------------------------------------
message("\n========== 4. SIMPLE REGRESSIONS: Info environment → combined_helplessness ==========")
for (iv in iv_cols) {
  if (!iv %in% names(dat)) next
  f <- as.formula(paste0("combined_helplessness ~ ", iv))
  m <- lm(f, data = dat)
  message("\n--- ", iv, " → combined_helplessness (N = ", m$df.residual + length(coef(m)), ") ---")
  print(summary(m))
}

# -----------------------------------------------------------------------------
# 5. Simple regressions: cognitive and emotional → collective_action_intent
# -----------------------------------------------------------------------------
message("\n========== 5. SIMPLE REGRESSIONS: Learned helplessness → collective_action_intent ==========")
for (m in c("cognitive_score", "emotional_score", "combined_helplessness")) {
  if (!m %in% names(dat)) next
  f <- as.formula(paste0(dv_col, " ~ ", m))
  mod <- lm(f, data = dat)
  message("\n--- ", m, " → ", dv_col, " (N = ", mod$df.residual + length(coef(mod)), ") ---")
  print(summary(mod))
}

# -----------------------------------------------------------------------------
# 6. Optional: Info environment → cognitive_score and → emotional_score
# -----------------------------------------------------------------------------
message("\n========== 6. SIMPLE REGRESSIONS: Info environment → cognitive_score / emotional_score ==========")
for (iv in iv_cols) {
  if (!iv %in% names(dat)) next
  for (m in c("cognitive_score", "emotional_score")) {
    if (!m %in% names(dat)) next
    f <- as.formula(paste0(m, " ~ ", iv))
    mod <- lm(f, data = dat)
    message("\n--- ", iv, " → ", m, " | b = ", round(coef(mod)[2], 4), ", p = ", round(summary(mod)$coefficients[2, 4], 4), " ---")
    print(summary(mod))
  }
}

message("\n========== END 06_regressions_investigation ==========")
message("Use this output to check: (1) variables look right, (2) which bivariate relations are significant, (3) whether mediation is plausible.")
