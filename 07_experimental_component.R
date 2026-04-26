# =============================================================================
# 07_experimental_component.R – Vignette / experimental condition vs outcomes
# =============================================================================
# Compares three randomized vignette arms (A / B / C) on LPH cognitive and
# emotional subscales, PCA composite (if available), and collective action intent.
#
# CONDITION SOURCE (two options):
#   (1) A column already in the data (set vignette_source_col).
#   (2) Reconstruct from which vignette block’s items are non-missing (default
#       when vignette_source_col is NULL). Qualtrics randomizers often leave no
#       embedded field; only one block’s questions appear per respondent.
#
# INPUT:  output/learned_helplessness_cleaned.csv (from 01_cleaning.R)
#         Optional: output/learned_helplessness_analysis_data.csv (from 02)
#                 merges pca1_score when present.
#
# OUTPUT: output/07_experimental_descriptives.csv
#         output/07_experimental_anova_table.csv
#         output/07_experimental_pairwise_tukey.csv
#         output/07_experimental_component_log.txt
#         output/07_vignette_reconstruction_audit.csv (when reconstruction runs)
#
# PREREQ: Run 01_cleaning.R first. Run 02 if you want pca1_score in the ANOVAs.
# WORKING DIRECTORY: project folder (same as 00_config.R).
# =============================================================================

# =============================================================================
# >>> MARIA — CONFIG <<<
# =============================================================================
# If your export already has a condition column, set its name here (exact match
# to a column in the cleaned CSV). Leave NULL to reconstruct from item patterns.
vignette_source_col <- NULL

# When using an existing column: map raw values to these analytic labels (optional).
vignette_raw_to_label <- NULL

# Substantive labels for methods / tables (A = high-transparency vignette,
# B = low-transparency / conflicting information, C = neutral — confirm wording
# with your advisor). Used as factor levels in ANOVA output.
condition_A_label <- "bias_optimistic"    # vignette block A (high transparency)
condition_B_label <- "bias_pessimistic"   # vignette block B (low / conflicting)
condition_C_label <- "neutral"            # vignette block C (neutral baseline)

# Order for contrasts (reference level = first).
desired_condition_order <- c(condition_A_label, condition_B_label, condition_C_label)

# Optional: alternate path to cleaned CSV
cleaned_data_path <- NULL

# Outcomes (must exist in cleaned data or be merged from 02).
outcome_vars <- c(
  "cognitive_score",
  "emotional_score",
  "pca1_score",
  "collective_action_intent"
)

# =============================================================================
# Setup
# =============================================================================

if (file.exists("00_config.R")) source("00_config.R")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

library(dplyr)

cleaned_path <- if (is.null(cleaned_data_path) || !nzchar(as.character(cleaned_data_path)[1])) {
  file.path(output_dir, "learned_helplessness_cleaned.csv")
} else {
  as.character(cleaned_data_path)[1]
}
if (!file.exists(cleaned_path)) {
  stop("Run 01_cleaning.R first (or fix cleaned_data_path). Missing: ", cleaned_path)
}

dat <- read.csv(cleaned_path, stringsAsFactors = FALSE, check.names = TRUE)
message("Loaded cleaned data: ", nrow(dat), " rows from ", cleaned_path)

# PCA score from 02 (not in 01 output)
analysis_path <- file.path(output_dir, "learned_helplessness_analysis_data.csv")
if (file.exists(analysis_path)) {
  dat_02 <- read.csv(analysis_path, stringsAsFactors = FALSE, check.names = TRUE)
  if ("ResponseId" %in% names(dat) && "ResponseId" %in% names(dat_02) && "pca1_score" %in% names(dat_02)) {
    dat <- dat %>%
      left_join(dat_02 %>% dplyr::select(ResponseId, pca1_score), by = "ResponseId")
    message("Merged pca1_score from ", analysis_path)
  } else {
    message("Note: could not merge pca1_score. Run 02 or drop pca1_score from outcome_vars.")
  }
} else {
  message("Note: ", basename(analysis_path), " not found — pca1_score unavailable unless you run 02.")
}

# =============================================================================
# Reconstruct vignette_condition from non-missing block items
# =============================================================================
# METHOD (for methods section):
#   Qualtrics showed exactly one of three vignette branches per respondent. Items
#   in other branches are skipped, so they appear as blank/NA in the export.
#
#   Format “Feb Q-ID” (e.g. Learned Political Helplessness … February 11 …):
#     • Block A (high transparency): post-vignette emotions use Q48_1–Q48_4
#       (hopeful, angry, …). Anchor: Q48_1.
#     • Block B (low / conflicting): parallel emotions use Q56_1–Q56_4
#       (includes tired, confused). Anchor: Q56_1.
#     • Block C (neutral): parallel emotions use Q63_1–Q63_4. Anchor: Q63_1.
#     These three anchors are mutually exclusive in the February export (every
#     finished vignette row has exactly one block’s emotion items present).
#
#   Format “March renamed” (e.g. … April/March …): Q48_* is usually absent. Prefer
#     • Block A: emotions_experiment_1 (first post–vignette emotion item, branch A).
#     • Block B: Q56_1
#     • Block C: Q63_1
#     (non-empty response = that branch was shown; works for Labels and Values.)
#   If emotions_experiment_1 is missing, fall back to numeric non-missing on
#     experiment_understd_1, Q53_1, Q59_1 (Values export).
#
#   Fallback if only classic Q-IDs without Q48: numeric non-missing on
#     Q43_1 (A), Q52_1 (B), Q59_1 (C) — understanding scales; use only when
#     emotion anchors are unavailable (older logic from 01_cleaning.R).
# =============================================================================

has_nonempty <- function(x) {
  x <- trimws(as.character(x))
  !is.na(x) & nzchar(x) & tolower(x) != "na"
}

to_num_safe <- function(x) {
  x <- as.character(x)
  x[x == ""] <- NA
  suppressWarnings(as.numeric(x))
}

reconstruct_vignette <- function(d) {
  nms <- names(d)

  # --- February Q-ID export: Q48_1, Q56_1, Q63_1 (no emotions_experiment_*) -----
  if (all(c("Q48_1", "Q56_1", "Q63_1") %in% nms)) {
    nA <- has_nonempty(d[["Q48_1"]])
    nB <- has_nonempty(d[["Q56_1"]])
    nC <- has_nonempty(d[["Q63_1"]])
    method_note <- paste0(
      "February-style anchors (non-empty text/response): ",
      "A = Q48_1 (emotions after high-transparency vignette); ",
      "B = Q56_1 (emotions after low-transparency vignette; block includes tired/confused); ",
      "C = Q63_1 (emotions after neutral vignette). ",
      "Verify against Qualtrics Survey Eng Final.pdf branch display logic."
    )
    anchor_cols <- list(A = "Q48_1", B = "Q56_1", C = "Q63_1")
    return(list(nA = nA, nB = nB, nC = nC, method_note = method_note, anchor_cols = anchor_cols))
  }

  # --- March / renamed export: emotions_experiment_1 + Q56_1 + Q63_1 ------------
  # Works for Values (numeric) and Labels (choice text) exports: whichever branch
  # was shown has non-empty responses; other branches are blank. Prefer this over
  # experiment_understd_1 / Q53_1 / Q59_1 when present — those understanding items
  # are often text in Labels exports and fail numeric parsing.
  if (all(c("emotions_experiment_1", "Q56_1", "Q63_1") %in% nms)) {
    nA <- has_nonempty(d[["emotions_experiment_1"]])
    nB <- has_nonempty(d[["Q56_1"]])
    nC <- has_nonempty(d[["Q63_1"]])
    method_note <- paste0(
      "March/renamed anchors (non-empty response): ",
      "A = emotions_experiment_1 (post–vignette A emotions); ",
      "B = Q56_1 (post–vignette B emotions); ",
      "C = Q63_1 (post–vignette C emotions). ",
      "Same logic as February Q48/Q56/Q63 but A uses the renamed first emotion item."
    )
    anchor_cols <- list(A = "emotions_experiment_1", B = "Q56_1", C = "Q63_1")
    return(list(nA = nA, nB = nB, nC = nC, method_note = method_note, anchor_cols = anchor_cols))
  }

  # --- Fallback March: numeric understanding fields (Values export) -------------
  if (all(c("experiment_understd_1", "Q53_1", "Q59_1") %in% nms)) {
    nA <- !is.na(to_num_safe(d[["experiment_understd_1"]]))
    nB <- !is.na(to_num_safe(d[["Q53_1"]]))
    nC <- !is.na(to_num_safe(d[["Q59_1"]]))
    method_note <- paste0(
      "March-style anchors (numeric non-missing): ",
      "A = experiment_understd_1; B = Q53_1; C = Q59_1. ",
      "Use when emotions_experiment_1 is absent; Labels exports may need the branch above instead."
    )
    anchor_cols <- list(A = "experiment_understd_1", B = "Q53_1", C = "Q59_1")
    return(list(nA = nA, nB = nB, nC = nC, method_note = method_note, anchor_cols = anchor_cols))
  }

  # --- Fallback: understanding Q43_1 / Q52_1 / Q59_1 (numeric) ----------------
  if (all(c("Q43_1", "Q52_1", "Q59_1") %in% nms)) {
    nA <- !is.na(to_num_safe(d[["Q43_1"]]))
    nB <- !is.na(to_num_safe(d[["Q52_1"]]))
    nC <- !is.na(to_num_safe(d[["Q59_1"]]))
    method_note <- paste0(
      "Q-ID understanding anchors (numeric): A = Q43_1, B = Q52_1, C = Q59_1. ",
      "Use only when Q48/Q56/Q63 emotion anchors are not in the file; confirm with survey PDF."
    )
    anchor_cols <- list(A = "Q43_1", B = "Q52_1", C = "Q59_1")
    return(list(nA = nA, nB = nB, nC = nC, method_note = method_note, anchor_cols = anchor_cols))
  }

  return(NULL)
}

guess_vignette_column <- function(d) {
  nm <- names(d)
  cand <- nm[grepl("vignette|condition|randomiz|treatment|assign|embed|exp_condition|experimental",
                  nm, ignore.case = TRUE)]
  cand <- unique(c(cand, if ("vignette_condition" %in% nm) "vignette_condition" else character(0)))
  cand
}

resolve_vignette_source <- function(d, explicit) {
  if (!is.null(explicit) && nzchar(explicit)) {
    if (!explicit %in% names(d)) {
      stop("vignette_source_col is set to \"", explicit, "\" but that column is not in the data.")
    }
    return(explicit)
  }
  if ("vignette_condition" %in% names(d)) {
    # If 01 already built it, use it unless you force reconstruction by renaming column
    nlev <- length(unique(stats::na.omit(d[["vignette_condition"]])))
    if (nlev >= 2) return("vignette_condition")
  }
  guesses <- guess_vignette_column(d)
  for (g in guesses) {
    if (g %in% names(d)) {
      nlev <- length(unique(stats::na.omit(d[[g]])))
      if (nlev >= 2) {
        message("Using column \"", g, "\" as condition (", nlev, " levels). ",
                "To reconstruct from items instead, remove/rename that column or set vignette_source_col explicitly.")
        return(g)
      }
    }
  }
  NULL
}

vcol <- resolve_vignette_source(dat, vignette_source_col)
recon <- NULL
vignette_from_reconstruction <- FALSE

if (is.null(vcol)) {
  recon <- reconstruct_vignette(dat)
  if (is.null(recon)) {
    hint <- guess_vignette_column(dat)
    hint_txt <- if (length(hint)) paste(hint, collapse = ", ") else "(none)"
    stop(
      "Could not find or reconstruct vignette condition.\n",
      "Options: (1) Set vignette_source_col to your embedded field name, or\n",
      "(2) Use an export that includes Q48_1/Q56_1/Q63_1 (Feb style), or\n",
      "    experiment_understd_1 + Q53_1 + Q59_1 (March style), or Q43_1/Q52_1/Q59_1.\n",
      "Heuristic name matches: ", hint_txt
    )
  }

  nA <- recon$nA
  nB <- recon$nB
  nC <- recon$nC
  n_hit <- nA + nB + nC

  dat$vignette_recon_flag <- dplyr::case_when(
    n_hit > 1 ~ "ambiguous_multiple_blocks",
    n_hit == 0 ~ "no_block_detected",
    nA ~ "clean_A",
    nB ~ "clean_B",
    nC ~ "clean_C",
    TRUE ~ "unknown"
  )

  dat$vignette_condition <- dplyr::case_when(
    n_hit > 1 ~ NA_character_,
    n_hit == 0 ~ NA_character_,
    nA ~ "A",
    nB ~ "B",
    nC ~ "C",
    TRUE ~ NA_character_
  )

  vignette_from_reconstruction <- TRUE
  message("\n--- Vignette reconstruction ---\n", recon$method_note)
  message("Anchor columns: A=", recon$anchor_cols$A, ", B=", recon$anchor_cols$B, ", C=", recon$anchor_cols$C)
  tab_flag <- table(dat$vignette_recon_flag, useNA = "ifany")
  message("Reconstruction flags:")
  print(tab_flag)

  audit <- data.frame(
    ResponseId = if ("ResponseId" %in% names(dat)) dat$ResponseId else seq_len(nrow(dat)),
    anchor_A_present = nA,
    anchor_B_present = nB,
    anchor_C_present = nC,
    n_blocks_nonempty = as.integer(nA) + as.integer(nB) + as.integer(nC),
    vignette_condition = dat$vignette_condition,
    vignette_recon_flag = dat$vignette_recon_flag,
    stringsAsFactors = FALSE
  )
  write.csv(audit, file.path(output_dir, "07_vignette_reconstruction_audit.csv"), row.names = FALSE)
  message("Wrote ", file.path(output_dir, "07_vignette_reconstruction_audit.csv"))

  vcol <- "vignette_condition"
} else {
  message("Using existing condition column: ", vcol)
}

# =============================================================================
# Build analysis factor (labels A/B/C -> substantive names)
# =============================================================================

raw_cond <- dat[[vcol]]
raw_chr <- trimws(as.character(raw_cond))

if (!is.null(vignette_raw_to_label) && length(vignette_raw_to_label) > 0) {
  mapped <- unname(vignette_raw_to_label[raw_chr])
  unk <- unique(raw_chr[is.na(mapped) & !is.na(raw_chr) & nzchar(raw_chr)])
  if (length(unk) > 0) {
    warning("Raw condition values not in vignette_raw_to_label (set to NA): ", paste(unk, collapse = ", "))
  }
  dat$vignette_factor <- factor(mapped, levels = desired_condition_order)
} else if (vignette_from_reconstruction) {
  dat$vignette_factor <- factor(
    raw_chr,
    levels = c("A", "B", "C"),
    labels = c(condition_A_label, condition_B_label, condition_C_label)
  )
} else {
  u <- sort(unique(stats::na.omit(raw_chr)))
  dat$vignette_factor <- factor(raw_chr, levels = u)
  message("Condition levels from data: ", paste(u, collapse = ", "))
}

# ANOVA sample: valid factor level only; drop ambiguous reconstruction rows
# Ambiguous / no-block rows already have NA vignette_condition and drop out here.
dat_an <- dat %>%
  filter(!is.na(vignette_factor), !is.na(.data[[vcol]]), as.character(vignette_factor) != "") %>%
  mutate(vignette_factor = droplevels(vignette_factor))

n_per_cell <- dat_an %>% count(vignette_factor, name = "n")
message("\n--- N by condition (analysis sample) ---")
print(as.data.frame(n_per_cell))

if (nlevels(dat_an$vignette_factor) < 2) {
  stop("Need at least 2 condition levels. Current: ",
       paste(levels(dat_an$vignette_factor), collapse = ", "))
}

if (any(n_per_cell$n < 2)) {
  message("Warning: at least one cell has n < 2; ANOVA may be unstable.")
}

# =============================================================================
# Descriptive statistics by condition
# =============================================================================

outcome_vars <- outcome_vars[outcome_vars %in% names(dat_an)]
if (length(outcome_vars) == 0) {
  stop("No outcome columns found. Check outcome_vars and 02 merge for pca1_score.")
}

desc_list <- list()
for (y in outcome_vars) {
  dd <- dat_an %>% filter(!is.na(.data[[y]]))
  desc_list[[y]] <- dd %>%
    group_by(vignette_factor) %>%
    summarise(
      outcome = y,
      n = dplyr::n(),
      mean = mean(as.numeric(.data[[y]]), na.rm = TRUE),
      sd = stats::sd(as.numeric(.data[[y]]), na.rm = TRUE),
      .groups = "drop"
    )
}
descriptives_long <- bind_rows(desc_list)
write.csv(descriptives_long, file.path(output_dir, "07_experimental_descriptives.csv"), row.names = FALSE)
message("\nWrote ", file.path(output_dir, "07_experimental_descriptives.csv"))

# =============================================================================
# One-way ANOVA and Tukey / Bonferroni
# =============================================================================

anova_rows <- list()
tukey_rows <- list()
log_txt <- character(0)

log_txt <- c(log_txt, "========== 07 experimental: ANOVA by outcome ==========")
if (vignette_from_reconstruction && !is.null(recon)) {
  log_txt <- c(log_txt, recon$method_note)
  log_txt <- c(log_txt, paste("Anchors — A:", recon$anchor_cols$A, " B:", recon$anchor_cols$B, " C:", recon$anchor_cols$C))
}
log_txt <- c(log_txt, paste("Condition column:", vcol))
log_txt <- c(log_txt, paste("Factor levels:", paste(levels(dat_an$vignette_factor), collapse = ", ")))
log_txt <- c(log_txt, "")

for (y in outcome_vars) {
  dd <- dat_an %>%
    filter(!is.na(.data[[y]])) %>%
    mutate(vignette_factor = droplevels(vignette_factor))
  dd[[y]] <- as.numeric(dd[[y]])

  if (nlevels(dd$vignette_factor) < 2) {
    log_txt <- c(log_txt, paste0("Skipping ", y, ": fewer than 2 condition levels after listwise."))
    next
  }
  if (nrow(dd) < 3) {
    log_txt <- c(log_txt, paste0("Skipping ", y, ": n < 3 after removing missing outcome."))
    next
  }

  fit <- stats::aov(as.formula(paste(y, "~ vignette_factor")), data = dd)
  sm <- summary(fit)[[1]]

  ss_factor <- sm["vignette_factor", "Sum Sq"]
  ss_resid  <- sm["Residuals", "Sum Sq"]
  df1 <- sm["vignette_factor", "Df"]
  df2 <- sm["Residuals", "Df"]
  Fstat <- sm["vignette_factor", "F value"]
  pval  <- sm["vignette_factor", "Pr(>F)"]
  partial_eta_sq <- ss_factor / (ss_factor + ss_resid)

  anova_rows[[y]] <- data.frame(
    outcome = y,
    n = nrow(dd),
    df_condition = df1,
    df_residual = df2,
    F = Fstat,
    p = pval,
    partial_eta_squared = partial_eta_sq,
    stringsAsFactors = FALSE
  )

  log_txt <- c(log_txt, paste0("--- ", y, " (n = ", nrow(dd), ") ---"))
  log_txt <- c(log_txt, capture.output(print(sm)))
  log_txt <- c(log_txt, paste("Partial eta-squared (condition):", round(partial_eta_sq, 4)))
  log_txt <- c(log_txt, "")

  tk <- stats::TukeyHSD(fit, "vignette_factor", ordered = TRUE)
  log_txt <- c(log_txt, "Tukey HSD:")
  log_txt <- c(log_txt, capture.output(print(tk)))
  log_txt <- c(log_txt, "")

  tkm <- as.data.frame(tk[[1]])
  tkm$contrast <- rownames(tkm)
  tkm$outcome <- y
  tukey_rows[[y]] <- tkm

  log_txt <- c(log_txt, "Pairwise t-tests (Bonferroni):")
  log_txt <- c(log_txt, capture.output(
    print(stats::pairwise.t.test(dd[[y]], dd$vignette_factor, p.adjust.method = "bonferroni"))
  ))
  log_txt <- c(log_txt, "")
}

anova_table <- bind_rows(anova_rows)
write.csv(anova_table, file.path(output_dir, "07_experimental_anova_table.csv"), row.names = FALSE)

tukey_table <- bind_rows(tukey_rows)
if (nrow(tukey_table) > 0) {
  write.csv(tukey_table, file.path(output_dir, "07_experimental_pairwise_tukey.csv"), row.names = FALSE)
}

message("\nWrote ", file.path(output_dir, "07_experimental_anova_table.csv"))
if (nrow(tukey_table) > 0) {
  message("Wrote ", file.path(output_dir, "07_experimental_pairwise_tukey.csv"))
}

# Planned contrasts (optional): see emmeans example in prior version.

writeLines(log_txt, file.path(output_dir, "07_experimental_component_log.txt"))
message("Wrote ", file.path(output_dir, "07_experimental_component_log.txt"))

message("\n========== 07_experimental_component.R finished ==========")
