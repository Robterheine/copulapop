# copulapop

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

**Fit and Generate Realistic Virtual Patient Populations Using Vine Copulas**

Copulas have been shown to be able to reproduce realistic datasets for use in pharmacometrics, as outlined by Zwep et al in Clin Pharmacol Ther 2024 Apr;115(4):795-804 (https://doi.org/10.1002/cpt.3099). The package copulapop creates virtual patient populations for example for pharmacometric modeling, PBPK simulations, and clinical trial simulations. It uses the vinecopula package for creation of copulas (https://cran.r-project.org/web/packages/VineCopula/index.html) also hosted at github https://github.com/tnagler/VineCopula by Thomas Nagler. More information about vinae copulas can be found at Department of Mathematics, TUM School of Computation, Information and Technology, Technische Universität München https://www.math.cit.tum.de/math/forschung/gruppen/statistics/vine-copula-models/ and the book by Claudia Czado published by Springer https://link.springer.com/book/10.1007/978-3-030-13785-4 .

## Features

- **Fit your own copulas** - Create vine copulas from any dataset
- **Vine copula-based generation** - Captures asymmetric dependencies and tail behaviors
- **Multiple international datasets** - Pre-fitted models for 7 populations
- **Age-sex stratified models** - Separate copulas for demographic subgroups
- **Derived variable calculation** - BMI, BSA, eGFR (CKD-EPI 2021, Schwartz)
- **Validation reports** - Automated PDF reports to assess copula fit quality

## Installation

```r
# Install from GitHub
devtools::install_github("robterheine/copulapop")

# Or install dependencies first
install.packages(c("VineCopula", "data.table", "jsonlite", "Matrix", "MASS"))
devtools::install_github("robterheine/copulapop")
```

## Quick Start: Fit Your Own Copula

```r
library(copulapop)

# Load your data
my_data <- read.csv("hospital_data.csv")

# Fit a vine copula
copula <- fit_copula(
  my_data,
  name = "My Hospital",
  variables = c("AGE", "HEIGHT", "WEIGHT", "CREAT"),
  description = "Hospitalized patients 2020-2024"
)

# Generate virtual population
pop <- generate_population_from_copula(copula, n = 1000)
head(pop)

# Save for later use or sharing
save_copula(copula, "my_hospital.json")

# Load saved copula
copula <- load_copula("my_hospital.json")
```

## Quick Start: Use Pre-fitted Datasets

```r
library(copulapop)

# Set path to JSON data files
set_data_path("/path/to/copulapop/data/")

# List available datasets
list_datasets()

# Generate 1000 virtual patients
pop <- generate_population("RUMC", n = 1000)
head(pop)

# Generate specific subpopulations
adults <- generate_population("NHANES", n = 500, age_range = c(18, 65))
pediatric <- generate_population("CHNS", n = 200, age_range = c(2, 17))
females <- generate_population("HSE", n = 300, sex = "female")
```

## Functions Overview

### Copula Fitting
| Function | Description |
|----------|-------------|
| `fit_copula()` | Fit a vine copula from your own data |
| `save_copula()` | Save fitted copula to JSON file |
| `load_copula()` | Load copula from JSON file |
| `generate_population_from_copula()` | Generate population from copula object |

### Pre-fitted Datasets
| Function | Description |
|----------|-------------|
| `set_data_path()` | Set path to JSON data files |
| `list_datasets()` | List available pre-fitted datasets |
| `get_dataset_info()` | Get information about a dataset |
| `generate_population()` | Generate population from pre-fitted dataset |
| `generate_population_from_json()` | Generate population from JSON file path |

### Validation
| Function | Description |
|----------|-------------|
| `validate_copula()` | Generate PDF validation report |

## fit_copula() Details

```r
copula <- fit_copula(
  data,                          # Your data.frame
  name = "My Dataset",           # Name for the copula
  description = NULL,            # Optional description
  variables = c("AGE", "HEIGHT", "WEIGHT"),  # Variables to include
  sex_column = NULL,             # Column name for sex (auto-detected)
  height_in_cm = TRUE,           # Convert HEIGHT from cm to m
  country = NULL,                # Optional country
  source = NULL,                 # Optional data source
  adult_only = FALSE,            # Only fit adult strata
  n_quantiles = 1001,            # Quantiles for marginal distributions
  min_stratum_n = 50,            # Minimum N per stratum
  verbose = TRUE                 # Print progress
)
```

### Input Data Requirements

Your data should have:
- **AGE** column (required)
- **HEIGHT** column (in cm or m)
- **WEIGHT** column (in kg)
- **SEX** column (optional, 1/M/male = male, 0/F/female = female)
- Additional continuous variables (e.g., CREAT, ALBUMIN)

Column names are case-insensitive. The function automatically:
- Converts HEIGHT from cm to meters if needed
- Detects sex column (SEXISMALE, SEX, GENDER, MALE)
- Creates age-sex stratified copulas

## Output Variables

| Variable | Description | Unit |
|----------|-------------|------|
| ID | Patient identifier | - |
| SEXISMALE | Sex (1=male, 0=female) | - |
| AGE | Age | years |
| HEIGHT | Height | m |
| WEIGHT | Weight | kg |
| BMI | Body mass index | kg/m² |
| BSA | Body surface area | m² |
| CREAT | Serum creatinine* | µmol/L |
| EGFR | Estimated GFR* | mL/min/1.73m² |
| ALBUMIN | Serum albumin* | g/L |

\* If included in variables

## Pre-fitted Datasets

| Dataset | Country | Population | N | Variables |
|---------|---------|------------|---|-----------|
| RUMC | Netherlands | Hospital patients | ~12k | AGE, HEIGHT, WEIGHT, CREAT |
| RUMC_IC | Netherlands | ICU patients | ~3k | AGE, HEIGHT, WEIGHT, CREAT, ALBUMIN |
| CHNS | China | General population | ~25k | AGE, HEIGHT, WEIGHT |
| KNHANES | South Korea | General population | ~50k | AGE, HEIGHT, WEIGHT, CREAT |
| TANZANIA | Tanzania | DHS survey | ~22k | AGE, HEIGHT, WEIGHT |
| NHANES | USA | General population | ~40k | AGE, HEIGHT, WEIGHT, CREAT |
| HSE | UK | General population | ~45k | AGE, HEIGHT, WEIGHT |

## Validation Reports

Generate comprehensive PDF validation reports:

```r
# Validate against source data
validate_copula(
  source_data = "my_data.csv",
  dataset = "my_copula.json",
  output_file = "validation.pdf"
)
```

The report includes:
- Executive summary with key metrics
- Descriptive statistics comparison
- Goodness-of-fit figures (percentiles, P-P plots)
- Correlation preservation assessment
- Distribution comparisons
- Conditional density heatmaps

### Validation Dependencies

```r
install.packages(c("rmarkdown", "knitr", "kableExtra", "ggplot2", 
                   "dplyr", "tidyr", "patchwork", "scales", "tinytex", viridis"))
```

## eGFR Calculation

- **Adults (≥18 years)**: CKD-EPI 2021 race-free equation
- **Children (<18 years)**: Bedside Schwartz formula



## Data Sources

| Dataset | Source | Reference |
|---------|--------|-----------|
| RUMC | Radboud University Medical Center | Internal |
| RUMC_IC | Radboud University Medical Center ICU | Internal |
| CHNS | China Health and Nutrition Survey | [cpc.unc.edu](https://www.cpc.unc.edu/projects/china) |
| KNHANES | Korea National Health and Nutrition Examination Survey | [knhanes.kdca.go.kr](https://knhanes.kdca.go.kr) |
| TANZANIA | Tanzania Demographic and Health Survey | [nbs.go.tz](https://www.nbs.go.tz) |
| NHANES | National Health and Nutrition Examination Survey | [cdc.gov/nchs/nhanes](https://www.cdc.gov/nchs/nhanes) |
| HSE | Health Survey for England 2015-2022 | [UK Data Service](https://digital.nhs.uk) |

## License

MIT License - see [LICENSE](LICENSE) for details.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 2.0.1 | 2026-01-26 | Changed heatmaps from absolute to relative counts
| 2.0.0 | 2026-01-26 | Added `fit_copula()`, `save_copula()`, `load_copula()`, `generate_population_from_copula()` |
| 1.0.0 | 2026-01-20 | Initial release with pre-fitted datasets and validation |
