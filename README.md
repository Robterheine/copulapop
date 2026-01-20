# copulapop

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

**Generate Realistic Virtual Patient Populations Using Vine Copulas**

copulapop creates virtual patient populations for pharmacometric modeling, PBPK simulations, and clinical trial simulations. It uses R-Vine copulas to preserve complex correlation structures and tail dependencies between covariates.

## Features

- **Vine copula-based generation** - Captures asymmetric dependencies and tail behaviors
- **Multiple international datasets** - Pre-fitted models for 7 populations:
  - RUMC (Netherlands) - Hospital patients with creatinine
  - RUMC_IC (Netherlands) - ICU patients with creatinine & albumin
  - CHNS (China) - General population
  - KNHANES (South Korea) - General population
  - TANZANIA - Demographic Health Survey
  - NHANES (USA) - General population with creatinine
  - HSE (UK) - Health Survey for England 2015-2022
- **Age-sex stratified models** - Separate copulas for demographic subgroups
- **Derived variable calculation** - BMI, BSA, eGFR (CKD-EPI 2021, Schwartz)
- **Validation reports** - Automated PDF reports to assess copula fit quality

## Repository Structure

```
copulapop/
├── R/                      # R source code
│   ├── data_path.R         # Path management functions
│   ├── generate.R          # Population generation functions
│   └── validate.R          # Validation report function
│
├── data/                   # Pre-fitted vine copula models (JSON)
│   ├── CHNS.json           # China Health and Nutrition Survey
│   ├── HSE.json            # Health Survey for England 2015-2022
│   ├── KNHANES.json        # Korea National Health and Nutrition Examination Survey
│   ├── NHANES.json         # US National Health and Nutrition Examination Survey
│   ├── RUMC.json           # Radboud UMC hospital patients
│   ├── RUMC_IC.json        # Radboud UMC ICU patients
│   └── TANZANIA.json       # Tanzania Demographic Health Survey
│
├── docs/                   # Documentation
│   └── reference_guide.md  # Complete function reference
│
├── DESCRIPTION             # Package metadata
├── NAMESPACE               # Exported functions
├── LICENSE                 # MIT License
└── README.md               # This file
```

## Installation

```r
# Install from GitHub
devtools::install_github("robterheine/copulapop")

# Or install dependencies first
install.packages(c("VineCopula", "data.table", "jsonlite", "Matrix", "MASS"))
devtools::install_github("robterheine/copulapop")
```

## Quick Start

```r
library(copulapop)

# Set path to JSON data files (the 'data' folder from this repository)
# Option 1: If you cloned the repo
set_data_path("~/git/copulapop/data/")

# Option 2: Download just the data folder and point to it
set_data_path("/path/to/downloaded/data/")

# List available datasets
list_datasets()
# [1] "CHNS" "HSE" "KNHANES" "NHANES" "RUMC" "RUMC_IC" "TANZANIA"

# Get dataset information
get_dataset_info("RUMC")

# Generate 1000 virtual patients
pop <- generate_population("RUMC", n = 1000)
head(pop)

# Generate specific subpopulations
adults <- generate_population("NHANES", n = 500, age_range = c(18, 65))
pediatric <- generate_population("CHNS", n = 200, age_range = c(2, 17))
females <- generate_population("HSE", n = 300, sex = "female")
```

## Available Datasets (data/ folder)

The `data/` folder contains pre-fitted R-Vine copula models for 7 international populations:

| JSON File | Country | Population | N | Age Range | Variables |
|-----------|---------|------------|---|-----------|-----------|
| `RUMC.json` | Netherlands | Hospital patients | ~12,000 | 0-100 | AGE, HEIGHT, WEIGHT, CREAT |
| `RUMC_IC.json` | Netherlands | ICU patients | ~3,000 | 18+ | AGE, HEIGHT, WEIGHT, CREAT, ALBUMIN |
| `CHNS.json` | China | General population | ~25,000 | 0-100 | AGE, HEIGHT, WEIGHT |
| `KNHANES.json` | South Korea | General population | ~50,000 | 0-100 | AGE, HEIGHT, WEIGHT |
| `TANZANIA.json` | Tanzania | DHS Survey | ~10,000 | 0-100 | AGE, HEIGHT, WEIGHT |
| `NHANES.json` | USA | General population | ~40,000 | 0-100 | AGE, HEIGHT, WEIGHT, CREAT |
| `HSE.json` | UK | General population | ~45,000 | 0-100 | AGE, HEIGHT, WEIGHT |

Each JSON file contains:
- Vine copula parameters (structure matrix, copula families, parameters)
- Marginal distributions (quantiles for inverse transform sampling)
- Age-sex stratified models (pediatric/adult × male/female)
- Dataset metadata (source, sample sizes, variable ranges)

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
| ALBUMIN | Serum albumin** | g/L |

\* Available in RUMC, RUMC_IC, NHANES  
\** Available in RUMC_IC only

## Validation Reports

Generate comprehensive PDF validation reports:

```r
# Validate built-in dataset
validate_copula(
  source_data = "RUMC.csv",       # Original source data
  dataset = "RUMC",               # Dataset name
  output_file = "RUMC_validation.pdf"
)

# Validate custom fitted copula (provide path to JSON file)
validate_copula(
  source_data = "my_study_data.csv",
  dataset = "my_fitted_copula.json",   # Path to your own JSON file
  output_file = "custom_validation.pdf"
)

# With custom options
validate_copula(
  source_data = my_data,
  dataset = "HSE",
  n_generate = 5000,
  title = "HSE Validation Report",
  author = "Your Name"
)
```

You can also generate directly from a custom JSON file:

```r
# Generate from your own fitted copula
pop <- generate_population_from_json("my_copula.json", n = 1000)
```

The validation report includes:
- Executive summary with key metrics
- Descriptive statistics comparison
- Goodness-of-fit figures (percentiles, P-P plots, correlation preservation)
- Distribution comparisons (histograms)
- Conditional density heatmaps
- Automated conclusions

### Validation Dependencies

```r
install.packages(c("rmarkdown", "knitr", "kableExtra", "ggplot2", 
                   "dplyr", "tidyr", "patchwork", "scales", "viridis"))
```

## eGFR Calculation

- **Adults (≥18 years)**: CKD-EPI 2021 race-free equation
- **Children (<18 years)**: Bedside Schwartz formula

## Citation

If you use copulapop in your research, please cite:

```
ter Heine R (2026). copulapop: Generate Virtual Populations Using Vine Copulas.
R package version 1.0.0. https://github.com/robterheine/copulapop
```

## Data Sources

The copula models in `data/` were fitted on the following source datasets:

| Dataset | Source | Reference |
|---------|--------|-----------|
| RUMC | Radboud University Medical Center | Internal |
| RUMC_IC | Radboud University Medical Center ICU | Internal |
| CHNS | China Health and Nutrition Survey | [cpc.unc.edu](https://www.cpc.unc.edu/projects/china) |
| KNHANES | Korea National Health and Nutrition Examination Survey | [knhanes.kdca.go.kr](https://knhanes.kdca.go.kr) |
| TANZANIA | Tanzania Demographic and Health Survey | [nbs.go.tz](https://www.nbs.go.tz) |
| NHANES | National Health and Nutrition Examination Survey | [cdc.gov/nchs/nhanes](https://www.cdc.gov/nchs/nhanes) |
| HSE | Health Survey for England 2015-2022 | [UK Data Service](https://digital.nhs.uk/data-and-information/publications/statistical/health-survey-for-england) |

**Note:** The JSON files contain only fitted statistical parameters (copula structures, quantiles), not the original individual-level data.

## License

MIT License - see [LICENSE](LICENSE) for details.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-01-20 | Initial release |
