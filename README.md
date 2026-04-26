# Information and the Making of Learned Political Helplessness in Lebanon

This repository contains the replication materials for my honors research project at Northwestern University.

## Research Question
Why does Lebanon’s political system remain resilient despite repeated crises, economic collapse, and widespread public disillusionment?

## Approach
This project uses a mixed-methods design:
- National survey (N ≈ 130+)
- Survey experiment (three vignette conditions)
- Semi-structured interviews
- Quantitative analysis in R

## File Structure
- `00_config.R` → project setup
- `01_cleaning.R` → data cleaning
- `02_reliability_and_factor_structure.R` → scale validation
- `03_validity_analyses.R` → regression models
- `04_export_tables.R` → tables for thesis
- `05_mediation_example.R` → mediation analysis
- `06_regressions_investigation.R` → robustness checks
- `07_experimental_component.R` → vignette experiment analysis

## Code Repository

All replication files and scripts are available here: 
https://github.com/mariachebli/Information-and-The-Making-of-Learned-Helplessness-in-Lebanon

## Methodology

This project employs a multi-stage quantitative pipeline implemented in R to construct, validate, and analyze the concept of *Learned Political Helplessness (LPH)*. The workflow proceeds through data cleaning, scale construction, factor validation, and multiple regression-based analyses, followed by an experimental component.

### Data Processing and Cleaning
Raw survey data were exported from Qualtrics and processed using a structured pipeline. Initial preprocessing involved:
- Removing metadata rows and filtering for completed responses
- Excluding preview/test entries
- Recoding categorical responses (agreement, frequency, likelihood) into numeric scales
- Standardizing variable names across multiple survey formats

Listwise deletion was applied to respondents missing any of the 11 LPH items, producing the final analysis sample. Emotional items were reverse-coded (except the “hopeful” item) so that higher values consistently reflect greater helplessness. Composite indices were constructed using row means.

These procedures are implemented in the cleaning script :contentReference[oaicite:0]{index=0}.

---

### Scale Construction
Two primary subscales were constructed:

- **Cognitive Helplessness**: mean of five state-efficacy items  
- **Emotional Helplessness**: mean of six emotional-frequency items  

Both were standardized and combined into a **composite helplessness index** using z-scores.

Additional indices were created for:
- Information availability  
- Information credibility  
- Crisis-normalizing perceptions  
- Media consumption  
- Collective action intent  

---

### Reliability Analysis
Internal consistency of both subscales was assessed using **Cronbach’s alpha**. Items with zero variance were excluded to ensure valid estimation, and item-level diagnostics (“alpha if item deleted”) were examined to assess scale coherence.

Reliability procedures are implemented in :contentReference[oaicite:1]{index=1}.

---

### Factor Structure Validation

#### Exploratory Factor Analysis (EFA)
EFA was conducted using **maximum likelihood estimation with oblimin rotation**, allowing factors to correlate. The number of factors was determined using **parallel analysis**, comparing observed eigenvalues to simulated random data.

Both one-factor and two-factor solutions were estimated to assess whether cognitive and emotional helplessness form distinct constructs.

#### Confirmatory Factor Analysis (CFA)
CFA models were estimated using structural equation modeling to formally test:
- A single latent helplessness factor
- A two-factor model (cognitive and emotional, allowed to correlate)

Model fit was evaluated using standard indices (CFI, TLI, RMSEA, SRMR).

#### Principal Component Analysis (PCA)
As a robustness check, PCA was performed on all 11 items to generate a first principal component score (PC1), used as an alternative composite measure.

These analyses are implemented in :contentReference[oaicite:2]{index=2}.

---

### Validity and Regression Analyses

#### Incremental Validity
Linear regression models assessed whether cognitive and emotional helplessness independently predict collective action intent:

- Cognitive-only model  
- Emotional-only model  
- Combined model  

Model fit was compared using R² to evaluate incremental explanatory power.

#### Distinct Prediction Models
Separate OLS regressions estimated the effect of information environment variables on:
- Cognitive helplessness  
- Emotional helplessness  

This tested whether different informational dynamics predict distinct dimensions of helplessness.

#### Uprising Participation
Logistic regression models examined whether information environment variables predict participation in the 2019 Lebanese uprising.

These analyses are implemented in :contentReference[oaicite:3]{index=3}.

---

### Mediation Analysis
A structural equation model tested whether learned helplessness mediates the relationship between information environment variables and collective action intent.

The model estimated:
- Direct effect (IV → DV)  
- Indirect effect (IV → LPH → DV)  
- Total effect  

Indirect effects were computed as the product of path coefficients (a × b).

Implemented in :contentReference[oaicite:4]{index=4}.

---

### Descriptive and Diagnostic Analyses
Additional analyses included:
- Summary statistics (mean, SD, range)  
- Pearson correlation matrices  
- Bivariate regressions (IV → DV, IV → mediator, mediator → DV)  

These were used to validate variable construction and explore baseline relationships before model specification.

Implemented in :contentReference[oaicite:5]{index=5}.

---

### Experimental Component
A randomized survey experiment exposed respondents to one of three vignette conditions:
- High-transparency (optimistic)
- Low-transparency / conflicting information (pessimistic)
- Neutral baseline

Condition assignment was reconstructed from response patterns where necessary. Group differences were evaluated using:
- **One-way ANOVA**  
- **Tukey post-hoc pairwise comparisons**

Outcomes included cognitive helplessness, emotional helplessness, PCA composite scores, and collective action intent.

Implemented in :contentReference[oaicite:6]{index=6}.

---

### Output and Reproducibility
All results—including cleaned datasets, regression outputs, and summary tables—are automatically generated and saved through the pipeline, ensuring full reproducibility of the analysis.

Final outputs are exported via :contentReference[oaicite:7]{index=7}.

## Replication Instructions
Run the scripts in order:

1. `00_config.R`
2. `01_cleaning.R`
3. `02_reliability_and_factor_structure.R`
4. `03_validity_analyses.R`
5. `04_export_tables.R`
6. `07_experimental_component.R`

## How to Interpret the Results

This section provides guidance on how to interpret the outputs generated by each stage of the analysis pipeline. The goal is to ensure that results are not only reproducible, but also analytically meaningful.

---

### 1. Reliability Analysis (Cronbach’s Alpha)

**What it tests:**
Internal consistency of the scale (whether items measure the same underlying construct).

**How to interpret:**

* α ≥ 0.80 → strong reliability
* 0.70 ≤ α < 0.80 → acceptable
* α < 0.70 → potential issues with scale coherence

**Additional diagnostics:**

* *“Alpha if item deleted”*:

  * If removing an item increases α substantially → that item may not belong in the scale
* *Item-total correlations*:

  * Low values indicate weak alignment with the overall construct

**In this project:**
Interpret reliability separately for:

* Cognitive helplessness
* Emotional helplessness

---

### 2. Exploratory Factor Analysis (EFA)

**What it tests:**
Whether items empirically group into distinct latent dimensions.

**How to interpret:**

* Factor loadings ≥ 0.40 → meaningful association with a factor
* Items should load strongly on one factor and weakly on others
* Cross-loadings (high on multiple factors) indicate ambiguity

**Factor structure:**

* 1-factor solution → unidimensional construct
* 2-factor solution → distinct cognitive and emotional dimensions

**Parallel analysis:**

* Retain factors whose eigenvalues exceed those from random data

---

### 3. Confirmatory Factor Analysis (CFA)

**What it tests:**
Whether the hypothesized factor structure fits the observed data.

**Key fit indices:**

* CFI / TLI:

  * > 0.95 → good fit
  * > 0.90 → acceptable
* RMSEA:

  * < 0.06 → good
  * < 0.08 → acceptable
* SRMR:

  * < 0.08 → acceptable

**Model comparison:**

* If the two-factor model fits better than the one-factor model → supports construct differentiation

---

### 4. Principal Component Analysis (PCA)

**What it provides:**
A data-driven composite index of helplessness.

**How to interpret:**

* PC1 represents the dimension explaining the most variance
* Higher PC1 scores → higher overall helplessness
* Used as a robustness check, not a theoretical measurement model

---

### 5. OLS Regression Models

#### a. Distinct Prediction Models

**What they test:**
How information-environment variables predict:

* Cognitive helplessness
* Emotional helplessness

**How to interpret coefficients (β):**

* Positive β → higher predictor associated with higher helplessness
* Negative β → higher predictor associated with lower helplessness

**Statistical significance:**

* p < .05 → statistically significant
* p < .01 → strong evidence
* p < .10 → marginal

**Model fit (R²):**

* Indicates proportion of variance explained
* Example: R² = 0.14 → model explains 14% of variation

---

#### b. Incremental Validity Models

**What they test:**
Whether cognitive and emotional subscales independently predict collective action intent.

**How to interpret:**

* Compare R² across models:

  * Cognitive-only
  * Emotional-only
  * Combined

* If combined model does not substantially increase R² → limited incremental contribution

---

### 6. Logistic Regression (Binary Outcomes)

**What it tests:**
Probability of participation in the 2019 uprising.

**How to interpret:**

* Coefficients are in log-odds → convert using exponentiation:

  * exp(β) = Odds Ratio (OR)

**Odds Ratio (OR):**

* OR > 1 → increases likelihood of outcome
* OR < 1 → decreases likelihood
* OR = 1 → no effect

**Statistical significance:**

* Same thresholds as OLS (p-values)

---

### 7. Mediation Analysis (SEM)

**What it tests:**
Whether learned helplessness mediates the relationship between:

* Information environment → collective action

**Key components:**

* Path a: IV → mediator
* Path b: mediator → DV
* Path c: direct effect (IV → DV)
* Indirect effect: a × b

**How to interpret:**

* Significant indirect effect → evidence of mediation
* Non-significant indirect effect → no mediation

---

### 8. Experimental Analysis (ANOVA)

**What it tests:**
Differences across randomized vignette conditions.

**ANOVA output:**

* F-statistic:

  * Significant F → at least one group differs
* p-value:

  * p < .05 → reject null hypothesis of equal means

**Post-hoc (Tukey tests):**

* Identify which specific groups differ
* Adjusts for multiple comparisons

**Interpretation:**

* Compare mean scores across conditions
* Direction of differences indicates effect of informational treatments

---

### 9. Descriptive Statistics and Correlations

**Descriptive statistics:**

* Mean → central tendency
* SD → dispersion
* Range → scale validity

**Correlations:**

* r ranges from -1 to +1
* |r| ≈ 0.10 → small
* |r| ≈ 0.30 → moderate
* |r| ≥ 0.50 → strong

**Important:**

* Correlation ≠ causation
* Used for preliminary insight, not inference

---

### General Interpretation Guidelines

* Focus on **effect size + statistical significance**, not p-values alone
* Distinguish between:

  * Statistical significance
  * Substantive significance
* Interpret results in light of:

  * Measurement validity
  * Model assumptions
  * Sample size

---

### Reproducibility Note

All results can be reproduced by running the scripts in sequence. Output files include regression tables, ANOVA results, and summary statistics to facilitate verification and interpretation.


## Key Finding
Crisis-normalizing information environments significantly predict cognitive political helplessness, helping explain weak collective action despite widespread dissatisfaction.
