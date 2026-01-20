# copulapop v1.0.0 - Complete Reference Guide

Generate realistic virtual patient populations using vine copulas for pharmacometric modeling, PBPK simulations, and clinical trial simulations.

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

| Function | Purpose |
|----------|---------|
| `set_data_path()` | Set location of JSON data files |
| `get_data_path()` | Get current data path |
| `list_datasets()` | List available datasets |
| `get_dataset_info()` | Get dataset metadata |
| `generate_population()` | Generate population from built-in dataset |
| `generate_population_from_json()` | Generate population from custom JSON file |
| `validate_copula()` | Generate PDF validation report |

---

## Function Reference

### 1. `set_data_path()`

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

### 2. `get_data_path()`

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

### 3. `list_datasets()`

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

### 4. `get_dataset_info()`

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

### 5. `generate_population()`

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
elderly_m <- generate_population("HSE", n = 300, age_range = c(65, 100), 
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
| `CREAT` | Serum creatinine | µmol/L | Only RUMC, RUMC_IC, NHANES |
| `EGFR` | Estimated GFR | mL/min/1.73m² | Only if CREAT available |
| `ALBUMIN` | Serum albumin | g/L | Only RUMC_IC |

#### Notes

- **eGFR calculation:**
  - Adults (≥18 years): CKD-EPI 2021 race-free equation
  - Children (<18 years): Bedside Schwartz formula
- **BSA calculation:** Du Bois formula: √(height_cm × weight_kg / 3600)
- The function uses age-sex stratified copulas to preserve appropriate covariate relationships

---

### 6. `generate_population_from_json()`

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

#### JSON File Format

The JSON file must follow the copulapop format with:

```json
{
  "info": {
    "name": "Dataset name",
    "description": "Description",
    "country": "Country",
    "population": "Population type",
    "has_creatinine": true/false
  },
  "variables": ["AGE", "HEIGHT", "WEIGHT", ...],
  "quantile_probs": [0.001, 0.005, 0.01, ...],
  "overall": {
    "n_total": 1000,
    "n_pediatric": 200,
    "n_adult": 800,
    "age_min": 0,
    "age_max": 95,
    "prop_male": 0.52
  },
  "strata": {
    "pediatric_1": { ... },
    "pediatric_0": { ... },
    "adult_1": { ... },
    "adult_0": { ... }
  }
}
```

This is the format produced by `generate_vine_copulas.R`.

---

### 7. `validate_copula()`

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

# With specific exclusions
validate_copula(
  source_data = my_data,
  dataset = "HSE",
  output_file = "HSE_validation.pdf",
  exclude_vars = c("REGION", "ETHNICITY"),
  exclude_derived = TRUE
)
```

#### Return

Invisibly returns the path to the generated PDF.

#### Report Contents

The PDF validation report includes:

1. **Executive Summary**
   - Key metrics with interpretation
   - Correlation RMSE
   - KS statistics (median and maximum)

2. **Data Overview**
   - Sample characteristics table
   - Descriptive statistics comparison (mean, SD, median)

3. **Figure 1: Goodness-of-Fit Assessment**
   - Panel A: Percentile agreement across variables
   - Panel B: P-P plots for variables with largest distributional differences
   - Panel C: Conditional percentile bands (outcome vs age)
   - Panel D: Correlation structure preservation

4. **Figure 2: Distribution Comparisons**
   - Histogram overlays for all continuous variables

5. **Figure 3: Conditional Density Heatmaps**
   - Bivariate density plots showing relationship preservation

6. **Summary Statistics Table**
   - Complete metrics with interpretation thresholds

7. **Conclusions**
   - Automated conclusions based on validation metrics

#### Interpretation Thresholds

| Metric | Excellent | Good | Acceptable | Poor |
|--------|-----------|------|------------|------|
| Correlation RMSE | < 0.05 | < 0.10 | < 0.15 | ≥ 0.15 |
| KS statistic | < 0.05 | < 0.10 | < 0.15 | ≥ 0.15 |

#### Required Packages

The validation function requires additional packages:

```r
install.packages(c("rmarkdown", "knitr", "kableExtra", "ggplot2", 
                   "dplyr", "tidyr", "patchwork", "scales", "viridis"))
```

---

## Available Datasets

| Dataset | Country | Population | Variables | Creatinine | Albumin | Age Range |
|---------|---------|------------|-----------|------------|---------|-----------|
| RUMC | Netherlands | Hospital patients | AGE, HEIGHT, WEIGHT, CREAT | ✓ | ✗ | 0-100 |
| RUMC_IC | Netherlands | ICU patients | AGE, HEIGHT, WEIGHT, CREAT, ALBUMIN | ✓ | ✓ | 18+ only |
| CHNS | China | General population | AGE, HEIGHT, WEIGHT | ✗ | ✗ | 0-100 |
| KNHANES | South Korea | General population | AGE, HEIGHT, WEIGHT | ✗ | ✗ | 0-100 |
| TANZANIA | Tanzania | DHS Survey | AGE, HEIGHT, WEIGHT | ✗ | ✗ | 0-100 |
| NHANES | USA | General population | AGE, HEIGHT, WEIGHT, CREAT | ✓ | ✗ | 0-100 |
| HSE | UK | General population | AGE, HEIGHT, WEIGHT | ✗ | ✗ | 0-100 |

---

## Typical Workflow

```r
library(copulapop)

# 1. Set data path (location of JSON files)
set_data_path("/path/to/copulapop/data/")

# 2. Explore available datasets
list_datasets()

# 3. Get information about a specific dataset
get_dataset_info("RUMC")

# 4. Generate virtual population
pop <- generate_population(
  dataset = "RUMC",
  n = 1000,
  age_range = c(18, 65),
  sex = "both",
  seed = 42
)

# 5. Inspect results
head(pop)
summary(pop)

# 6. Generate validation report (optional)
validate_copula(
  source_data = "RUMC.csv",
  dataset = "RUMC",
  output_file = "RUMC_validation.pdf",
  n_generate = 1000
)
```

---

## Custom Copula Workflow

```r
library(copulapop)

# 1. Fit your own copula using generate_vine_copulas.R
#    This produces a JSON file (e.g., "my_copula.json")

# 2. Generate population from your custom copula
pop <- generate_population_from_json(
  json_file = "my_copula.json",
  n = 1000,
  age_range = c(18, 80)
)

# 3. Validate against original data
validate_copula(
  source_data = "my_original_data.csv",
  dataset = "my_copula.json",
  output_file = "my_validation.pdf",
  n_generate = 1000,
  title = "Custom Copula Validation",
  author = "Your Name"
)
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
| 1.0.0 | 2026-20-01 | Initial release |

---

## Citation

```
ter Heine R (2026). copulapop: Generate Virtual Populations Using Vine Copulas.
R package version 1.0.0 https://github.com/robterheine/copulapop
```

---

## License

MIT License - Copyright (c) 2026 Rob ter Heine, Radboud University Medical Center
