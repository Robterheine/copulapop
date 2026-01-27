# copulapop v2.0.0 - Complete Reference Guide

Fit and generate realistic virtual patient populations using vine copulas for pharmacometric modeling, PBPK simulations, and clinical trial simulations.

---

## Installation

```r
# Install from GitHub
devtools::install_github("robterheine/copulapop")

# Additional packages for validation reports
install.packages(c("rmarkdown", "knitr", "kableExtra", "ggplot2", 
                   "dplyr", "tidyr", "patchwork", "scales", "viridis"))
```

---

## Functions Overview

### Copula Fitting (NEW in v2.0.0)

| Function | Purpose |
|----------|---------|
| `fit_copula()` | Fit a vine copula from your own data |
| `save_copula()` | Save fitted copula to JSON file |
| `load_copula()` | Load copula from JSON file |
| `generate_population_from_copula()` | Generate population from copula object |

### Pre-fitted Datasets

| Function | Purpose |
|----------|---------|
| `set_data_path()` | Set location of JSON data files |
| `get_data_path()` | Get current data path |
| `list_datasets()` | List available datasets |
| `get_dataset_info()` | Get dataset metadata |
| `generate_population()` | Generate population from built-in dataset |
| `generate_population_from_json()` | Generate population from custom JSON file |

### Validation

| Function | Purpose |
|----------|---------|
| `validate_copula()` | Generate PDF validation report |

---

## Function Reference

### 1. `fit_copula()` *(NEW)*

Fits an R-Vine copula model to a dataset, creating a copula object that can be used to generate virtual populations.

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `data` | data.frame | *required* | Source data containing the variables |
| `name` | character | "Custom Dataset" | Name for the copula |
| `description` | character | NULL | Optional description |
| `variables` | character vector | c("AGE", "HEIGHT", "WEIGHT") | **Continuous** variables to include (AGE required). Do NOT include sex. |
| `sex_column` | character | NULL | Column name for sex (auto-detected) |
| `height_in_cm` | logical | TRUE | Convert HEIGHT from cm to meters |
| `country` | character | NULL | Optional country name |
| `source` | character | NULL | Optional data source |
| `source_url` | character | NULL | Optional URL to data source |
| `adult_only` | logical | FALSE | Only fit adult (≥18) strata |
| `n_quantiles` | integer | 1001 | Number of quantiles for marginals |
| `min_stratum_n` | integer | 50 | Minimum N per stratum |
| `verbose` | logical | TRUE | Print progress messages |

#### Example

```r
# Load your data
my_data <- read.csv("hospital_data.csv")

# Fit copula with default variables
copula <- fit_copula(my_data, name = "My Hospital")

# Fit with additional variables
copula <- fit_copula(
  my_data,
  name = "My Hospital",
  variables = c("AGE", "HEIGHT", "WEIGHT", "CREAT"),
  description = "Hospitalized patients 2020-2024",
  country = "Netherlands"
)

# View summary
print(copula)
summary(copula)
```

#### Return

A `copulapop_copula` object (list) containing:

| Element | Description |
|---------|-------------|
| `info` | Metadata (name, description, country, etc.) |
| `variables` | Variables included in the copula |
| `overall` | Overall statistics (n_total, age_min, age_max, prop_male) |
| `strata` | Fitted copula parameters per stratum |
| `quantile_probs` | Quantile probabilities used |

#### Strata

The function fits separate copulas for each age-sex stratum:

| Stratum | Description |
|---------|-------------|
| `pediatric_0` | Children (<18) female |
| `pediatric_1` | Children (<18) male |
| `pediatric_all` | All children |
| `adult_0` | Adults (≥18) female |
| `adult_1` | Adults (≥18) male |
| `adult_all` | All adults |
| `all` | Combined dataset |

If vine copula fitting fails (insufficient data or numerical issues), a Gaussian copula fallback is used.

#### Input Data Requirements

##### Required Data Format

Your CSV or data.frame should be structured as follows:

| Column | Required | Type | Unit | Description |
|--------|----------|------|------|-------------|
| AGE | Yes | Numeric | years | Age of subject |
| HEIGHT | Recommended | Numeric | cm or m | Height (auto-converted if in cm) |
| WEIGHT | Recommended | Numeric | kg | Weight |
| SEX | Recommended | Numeric or character | - | Sex indicator (see below) |
| CREAT | Optional | Numeric | µmol/L | Serum creatinine |
| ALBUMIN | Optional | Numeric | g/L | Serum albumin |
| *other* | Optional | Numeric | - | Any continuous variable |

##### Example Dataset

```
AGE,HEIGHT,WEIGHT,SEX,CREAT
45,175,82,1,88
32,162,65,0,62
8,128,26,1,35
67,168,78,0,95
```

##### Sex Column Encoding

The sex column is auto-detected (looks for: SEXISMALE, SEX, GENDER, MALE) and accepts:

| Male | Female |
|------|--------|
| 1 | 0 |
| "M" | "F" |
| "male" | "female" |
| TRUE | FALSE |

##### ⚠️ IMPORTANT: Do NOT include sex in `variables`

The `variables` parameter should contain **only continuous variables**. The sex column is used automatically for **stratification** (fitting separate copulas for males and females), not as a copula variable itself.

```r
# ❌ WRONG - will cause an error
copula <- fit_copula(
  my_data,
  variables = c("AGE", "HEIGHT", "WEIGHT", "SEXISMALE")  # ERROR!
)

# ✓ CORRECT
copula <- fit_copula(
  my_data,
  variables = c("AGE", "HEIGHT", "WEIGHT")
)
```

**Why?** Within each stratum (e.g., adult females), SEXISMALE has zero variance (all values are 0), making correlation calculation impossible.

##### Column Name Handling

- Column names are **case-insensitive** (age, AGE, Age all work)
- HEIGHT is auto-converted from cm to m if values > 3
- Missing values (NA) are handled: rows with NA in any variable are excluded

---

### 2. `save_copula()` *(NEW)*

Saves a fitted copula object to a JSON file for later use or sharing.

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `copula` | copulapop_copula | *required* | Copula object from `fit_copula()` |
| `file` | character | *required* | Output JSON file path |
| `pretty` | logical | FALSE | Use indented JSON formatting |

#### Example

```r
copula <- fit_copula(my_data, name = "My Hospital")
save_copula(copula, "my_hospital.json")
```

#### Return

Invisibly returns the file path.

---

### 3. `load_copula()` *(NEW)*

Loads a previously saved copula from a JSON file.

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `file` | character | *required* | Path to JSON file |

#### Example

```r
copula <- load_copula("my_hospital.json")
print(copula)
```

#### Return

A `copulapop_copula` object.

---

### 4. `generate_population_from_copula()` *(NEW)*

Generates a virtual population directly from a copulapop_copula object.

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `copula` | copulapop_copula | *required* | Copula object from `fit_copula()` or `load_copula()` |
| `n` | integer | 1000 | Number of virtual patients to generate |
| `age_range` | numeric(2) | NULL | Age range [min, max]. NULL uses full range |
| `sex` | character | "both" | Sex filter: "both", "male", or "female" |
| `seed` | integer | NULL | Random seed for reproducibility |

#### Example

```r
# Fit and generate in one workflow
copula <- fit_copula(my_data, name = "My Hospital")
pop <- generate_population_from_copula(copula, n = 1000)

# With filters
adults <- generate_population_from_copula(
  copula,
  n = 500,
  age_range = c(18, 65),
  sex = "male",
  seed = 42
)
```

#### Return

A data.frame with the same structure as `generate_population()`.

---

### 5. `set_data_path()`

Sets the directory path where the copulapop JSON data files are stored.

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `path` | character | *required* | Path to directory containing JSON files |

#### Example

```r
set_data_path("/Users/rob/copulapop/data/")
```

#### Return

Invisibly returns the previous path setting.

---

### 6. `get_data_path()`

Returns the currently configured path to copulapop data files.

#### Parameters

None.

#### Example

```r
get_data_path()
# [1] "/Users/rob/copulapop/data/"
```

#### Return

Character string with the data path, or NULL if not set.

---

### 7. `list_datasets()`

Lists all available datasets in the current data path.

#### Parameters

None.

#### Example

```r
list_datasets()
# [1] "CHNS" "HSE" "KNHANES" "NHANES" "RUMC" "RUMC_IC" "TANZANIA"
```

#### Return

Character vector of dataset names (without .json extension).

---

### 8. `get_dataset_info()`

Retrieves metadata and summary information about a specific dataset.

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `dataset` | character | *required* | Dataset name (e.g., "RUMC", "NHANES") |

#### Example

```r
get_dataset_info("RUMC")
# Dataset: RUMC (Netherlands)
# --------------------------------------------------
# Description: Radboud University Medical Center
# Country: Netherlands
# Population: Hospital patients
# Sample size: 12,345
#   Pediatric (<18): 1,234
#   Adult (>=18): 11,111
# Age range: 0 - 95 years
# Male proportion: 52.3%
# Variables: AGE, HEIGHT, WEIGHT, CREAT
# Creatinine: Available
# Source: Radboudumc
```

#### Return

A list (class `copulapop_info`) containing:

| Element | Description |
|---------|-------------|
| `name` | Full dataset name |
| `description` | Dataset description |
| `country` | Country of origin |
| `population` | Population type |
| `source` | Data source |
| `source_url` | URL to data source |
| `n_total` | Total sample size |
| `n_pediatric` | Number of pediatric subjects (<18) |
| `n_adult` | Number of adult subjects (≥18) |
| `age_range` | Age range [min, max] in years |
| `prop_male` | Proportion male |
| `variables` | Available variables |
| `has_creatinine` | Whether creatinine is available |
| `has_albumin` | Whether albumin is available |
| `adult_only` | Whether dataset contains adults only |

---

### 9. `generate_population()`

Generates a virtual patient population using vine copula-based simulation from a built-in dataset.

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `dataset` | character | *required* | Dataset name (e.g., "RUMC", "NHANES", "HSE") |
| `n` | integer | 1000 | Number of virtual patients to generate |
| `age_range` | numeric(2) | NULL | Age range [min, max] in years. NULL uses full range of source data |
| `sex` | character | "both" | Sex filter: "both", "male", or "female" |
| `seed` | integer | NULL | Random seed for reproducibility |

#### Examples

```r
# Basic usage - generate 1000 patients
pop <- generate_population("RUMC", n = 1000)

# Generate only adults (18-65 years)
adults <- generate_population("NHANES", n = 500, age_range = c(18, 65))

# Generate only pediatric females
ped_f <- generate_population("CHNS", n = 200, age_range = c(2, 17), sex = "female")

# Generate elderly males with reproducibility
elderly_m <- generate_population("HSE", n = 300, age_range = c(65, 90), 
                                  sex = "male", seed = 42)
```

#### Return

A data.frame with the following columns:

| Column | Description | Unit | Always present |
|--------|-------------|------|----------------|
| `ID` | Patient identifier | - | ✓ |
| `SEXISMALE` | Sex indicator (1 = male, 0 = female) | - | ✓ |
| `AGE` | Age | years | ✓ |
| `HEIGHT` | Height | m | ✓ |
| `WEIGHT` | Weight | kg | ✓ |
| `BMI` | Body mass index | kg/m² | ✓ (calculated) |
| `BSA` | Body surface area | m² | ✓ (calculated) |
| `CREAT` | Serum creatinine | µmol/L | Only RUMC, RUMC_IC, NHANES, KNHANES |
| `EGFR` | Estimated GFR | mL/min/1.73m² | Only if CREAT available |
| `ALBUMIN` | Serum albumin | g/L | Only RUMC_IC |

#### Notes

- **eGFR calculation:**
  - Adults (≥18 years): CKD-EPI 2021 race-free equation
  - Children (<18 years): Bedside Schwartz formula
- **BSA calculation:** Du Bois formula: √(height_cm × weight_kg / 3600)
- The function uses age-sex stratified copulas to preserve appropriate covariate relationships

---

### 10. `generate_population_from_json()`

Generates a virtual patient population from a custom-fitted vine copula stored in a JSON file.

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `json_file` | character | *required* | Path to JSON file containing the fitted copula |
| `n` | integer | 1000 | Number of virtual patients to generate |
| `age_range` | numeric(2) | NULL | Age range [min, max] in years. NULL uses full range |
| `sex` | character | "both" | Sex filter: "both", "male", or "female" |
| `seed` | integer | NULL | Random seed for reproducibility |

#### Examples

```r
# Generate from custom fitted copula
pop <- generate_population_from_json("my_study_copula.json", n = 1000)

# With filters
adults <- generate_population_from_json(
  json_file = "custom.json", 
  n = 500, 
  age_range = c(18, 65),
  sex = "male",
  seed = 123
)
```

#### Return

A data.frame with the same structure as `generate_population()`.

---

### 11. `validate_copula()`

Generates a comprehensive PDF validation report comparing generated virtual populations against original source data.

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `source_data` | character or data.frame | *required* | Path to source data CSV file, or a data.frame |
| `dataset` | character | *required* | Dataset name (e.g., "RUMC") OR path to custom JSON file (must end in `.json`) |
| `output_file` | character | "validation_report.pdf" | Output PDF filename |
| `n_generate` | integer | 1000 | Number of virtual individuals to generate for validation |
| `title` | character | NULL | Report title. NULL auto-generates from dataset name |
| `author` | character | "copulapop" | Report author name |
| `exclude_vars` | character vector | NULL | Variables to exclude from validation |
| `exclude_derived` | logical | TRUE | Auto-exclude derived variables (BMI, eGFR, BSA, etc.) |

#### Examples

```r
# Validate built-in dataset
validate_copula(
  source_data = "RUMC.csv",
  dataset = "RUMC",
  output_file = "RUMC_validation.pdf"
)

# Validate custom fitted copula
validate_copula(
  source_data = "my_study_data.csv",
  dataset = "my_fitted_copula.json",
  output_file = "custom_validation.pdf",
  n_generate = 5000,
  title = "My Study Validation Report",
  author = "Rob ter Heine"
)
```

#### Return

Invisibly returns the path to the generated PDF.

#### Report Contents

The PDF validation report includes:

1. **Executive Summary** - Key metrics with interpretation
2. **Data Overview** - Sample characteristics, descriptive statistics
3. **Figure 1: Goodness-of-Fit** - Percentile agreement, P-P plots, conditional percentile bands, correlation preservation
4. **Figure 2: Distribution Comparisons** - Histogram overlays
5. **Figure 3: Conditional Density Heatmaps** - Bivariate density plots
6. **Summary Statistics Table** - Complete metrics with thresholds
7. **Conclusions** - Automated interpretation

#### Interpretation Thresholds

| Metric | Excellent | Good | Acceptable | Poor |
|--------|-----------|------|------------|------|
| Correlation RMSE | < 0.05 | < 0.10 | < 0.15 | ≥ 0.15 |
| KS statistic | < 0.05 | < 0.10 | < 0.15 | ≥ 0.15 |

#### Required Packages

```r
install.packages(c("rmarkdown", "knitr", "kableExtra", "ggplot2", 
                   "dplyr", "tidyr", "patchwork", "scales", "viridis"))
```

---

## Available Pre-fitted Datasets

| Dataset | Country | Population | N | Age Range | Variables |
|---------|---------|------------|---|-----------|-----------|
| RUMC | Netherlands | Hospital patients | ~12,000 | 0–100 | AGE, HEIGHT, WEIGHT, CREAT |
| RUMC_IC | Netherlands | ICU patients (adults only) | ~3,000 | 18–100 | AGE, HEIGHT, WEIGHT, CREAT, ALBUMIN |
| CHNS | China | General population | ~25,000 | 0–100 | AGE, HEIGHT, WEIGHT |
| KNHANES | South Korea | General population | ~50,000 | 1–80 | AGE, HEIGHT, WEIGHT, CREAT |
| TANZANIA | Tanzania | Women 15–49, children 0–5 | ~22,000 | 0–49 | AGE, HEIGHT, WEIGHT |
| NHANES | USA | General population | ~40,000 | 0–80 | AGE, HEIGHT, WEIGHT, CREAT |
| HSE | UK | General population | ~45,000 | 0–90 | AGE, HEIGHT, WEIGHT |

**Note:** The JSON files contain only statistical parameters (copula structure, quantiles), not individual-level data.

---

## Typical Workflows

### Workflow 1: Fit Your Own Copula

```r
library(copulapop)

# 1. Load your data
my_data <- read.csv("hospital_data.csv")

# 2. Fit copula
copula <- fit_copula(
  my_data,
  name = "My Hospital",
  variables = c("AGE", "HEIGHT", "WEIGHT", "CREAT"),
  description = "Hospitalized patients 2020-2024"
)

# 3. Inspect
print(copula)
summary(copula)

# 4. Generate virtual population
pop <- generate_population_from_copula(copula, n = 1000)
head(pop)

# 5. Save for later
save_copula(copula, "my_hospital.json")

# 6. Validate (optional)
validate_copula(
  source_data = my_data,
  dataset = "my_hospital.json",
  output_file = "validation.pdf"
)
```

### Workflow 2: Use Pre-fitted Datasets

```r
library(copulapop)

# 1. Set data path (location of JSON files)
set_data_path("/path/to/copulapop/data/")

# 2. Explore available datasets
list_datasets()
get_dataset_info("RUMC")

# 3. Generate virtual population
pop <- generate_population(
  dataset = "RUMC",
  n = 1000,
  age_range = c(18, 65),
  sex = "both",
  seed = 42
)

# 4. Inspect results
head(pop)
summary(pop)
```

---

## Dependencies

### Core (Imports)

Required for basic functionality:

- VineCopula
- data.table
- jsonlite
- Matrix
- MASS
- stats

### Validation (Suggests)

Required only for `validate_copula()`:

- rmarkdown
- knitr
- kableExtra
- ggplot2
- dplyr
- tidyr
- patchwork
- scales
- viridis

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 2.0.0 | 2026-01-26 | Added `fit_copula()`, `save_copula()`, `load_copula()`, `generate_population_from_copula()` |
| 1.0.0 | 2026-01-20 | Initial release with pre-fitted datasets and validation |

---

## Citation

```
ter Heine R (2026). copulapop: Fit and Generate Virtual Populations Using Vine Copulas.
R package version 2.0.0. https://github.com/robterheine/copulapop
```

---

## License

MIT License - Copyright (c) 2026 Rob ter Heine, Radboud University Medical Center
