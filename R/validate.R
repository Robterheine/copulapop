# ============================================================================
# COPULAPOP VALIDATION REPORT GENERATOR
# ============================================================================

#' Generate a Validation Report for Copula-Generated Populations
#'
#' Creates a comprehensive PDF report comparing generated virtual populations
#' against the original source data. The report includes goodness-of-fit
#' assessments, distribution comparisons, conditional density analyses,
#' and summary statistics.
#'
#' @param source_data Path to the original source data CSV file, or a data.frame
#' @param dataset Either:
#'   \itemize{
#'     \item Name of a copulapop dataset (e.g., "RUMC", "NHANES", "HSE")
#'     \item Path to a custom JSON copula file (must end in .json)
#'   }
#' @param output_file Output PDF filename (default: "validation_report.pdf")
#' @param n_generate Number of virtual individuals to generate (default: 1000)
#' @param title Report title (default: auto-generated from dataset name)
#' @param author Report author (default: "copulapop")
#' @param exclude_vars Variables to exclude from validation (default: auto-detect IDs)
#' @param exclude_derived Auto-exclude derived variables like BMI, eGFR (default: TRUE)
#'
#' @return Invisibly returns the path to the generated PDF
#'
#' @details
#' The validation report includes:
#' \itemize{
#'   \item Executive summary with key metrics and interpretation
#'   \item Data overview with sample characteristics
#'   \item Figure 1: Goodness-of-fit (percentiles, P-P plots, conditional bands, correlations)
#'   \item Figure 2: Histogram comparisons for all continuous variables
#'   \item Figure 3: Conditional density heatmaps
#'   \item Summary statistics table
#'   \item Automated conclusions based on validation metrics
#' }
#'
#' Interpretation thresholds:
#' \itemize{
#'   \item RMSE/KS < 0.05: Excellent fit
#'   \item RMSE/KS < 0.10: Good fit
#'   \item RMSE/KS < 0.15: Acceptable fit
#' }
#'
#' @examples
#' \dontrun{
#' # Validate built-in dataset
#' validate_copula(
#'   source_data = "RUMC.csv",
#'   dataset = "RUMC",
#'   output_file = "RUMC_validation.pdf"
#' )
#'
#' # Validate custom fitted copula
#' validate_copula(
#'   source_data = "my_study_data.csv",
#'   dataset = "my_fitted_copula.json",
#'   output_file = "custom_validation.pdf"
#' )
#'
#' # Validate with custom settings
#' validate_copula(
#'   source_data = my_data,
#'   dataset = "HSE",
#'   n_generate = 5000,
#'   title = "HSE 2015-2022 Validation Report"
#' )
#' }
#'
#' @export
validate_copula <- function(source_data,
                            dataset,
                            output_file = "validation_report.pdf",
                            n_generate = 1000,
                            title = NULL,
                            author = "copulapop",
                            exclude_vars = NULL,
                            exclude_derived = TRUE) {
  
  # Check required packages
  required_pkgs <- c("rmarkdown", "knitr", "ggplot2", "dplyr", "tidyr",
                     "patchwork", "scales", "viridis", "kableExtra")
  
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("Required packages missing: ", paste(missing_pkgs, collapse = ", "),
         "\nInstall with: install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))")
  }
  
  # Load source data
  if (is.character(source_data)) {
    if (!file.exists(source_data)) {
      stop("Source data file not found: ", source_data)
    }
    original_data <- data.table::fread(source_data, na.strings = c("NULL", "NA", ""))
    original_data <- as.data.frame(original_data)
  } else if (is.data.frame(source_data)) {
    original_data <- as.data.frame(source_data)
  } else {
    stop("source_data must be a file path or data.frame")
  }
  
  # Determine if dataset is a JSON file path or a dataset name
  is_json_file <- grepl("\\.json$", dataset, ignore.case = TRUE)
  
  if (is_json_file) {
    # Custom JSON file provided
    if (!file.exists(dataset)) {
      stop("JSON copula file not found: ", dataset)
    }
    json_file <- dataset
    dataset_name <- gsub("\\.json$", "", basename(dataset))
    message("Using custom copula file: ", dataset)
  } else {
    # Built-in dataset name
    json_file <- NULL
    dataset_name <- dataset
  }
  
  # Generate virtual population
  message("Generating ", n_generate, " virtual individuals from '", dataset_name, "'...")
  generated_data <- tryCatch({
    if (is_json_file) {
      generate_population_from_json(json_file, n = n_generate)
    } else {
      generate_population(dataset, n = n_generate)
    }
  }, error = function(e) {
    stop("Failed to generate population: ", e$message)
  })
  
  # Auto-generate title if not provided
  if (is.null(title)) {
    title <- paste0("Copula Validation Report: ", dataset_name)
  }
  
  # Create temporary Rmd file
  rmd_template <- .create_validation_rmd_template()
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(rmd_template, temp_rmd)
  
  # Ensure output directory exists
  output_dir <- dirname(output_file)
  if (output_dir == ".") output_dir <- getwd()
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Render report
  message("Generating validation report...")
  output_path <- rmarkdown::render(
    input = temp_rmd,
    output_file = basename(output_file),
    output_dir = output_dir,
    params = list(
      original_data = original_data,
      generated_data = generated_data,
      dataset_name = dataset,
      title = title,
      author = author,
      n_generate = n_generate,
      exclude_vars = exclude_vars,
      exclude_derived = exclude_derived
    ),
    quiet = TRUE
  )
  
  # Clean up temp file
  unlink(temp_rmd)
  
  message("Validation report saved: ", output_path)
  invisible(output_path)
}


# Internal function to create R Markdown template
.create_validation_rmd_template <- function() {
'---
title: "`r params$title`"
author: "`r params$author`"
date: "`r Sys.Date()`"
output:
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
    fig_caption: true
    keep_tex: false
    latex_engine: pdflatex
header-includes:
  - \\usepackage{float}
  - \\usepackage{booktabs}
  - \\floatplacement{figure}{H}
params:
  original_data: NULL
  generated_data: NULL
  dataset_name: "Unknown"
  title: "Copula Validation Report"
  author: "copulapop"

  n_generate: 1000
  exclude_vars: NULL
  exclude_derived: TRUE
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.width = 10,
  fig.height = 7,
  fig.align = "center",
  out.width = "100%"
)

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)
library(viridis)
library(kableExtra)

# Load data from params
original_data <- params$original_data
generated_data <- params$generated_data
dataset_name <- params$dataset_name
n_generate <- params$n_generate
EXCLUDE_VARS <- params$exclude_vars
EXCLUDE_DERIVED <- params$exclude_derived
```

```{r data-prep}
# Clean column names
names(original_data)[1] <- gsub("^\\\\uFEFF", "", names(original_data)[1])
names(original_data) <- gsub(" ", "_", names(original_data))

# Detect ID columns
detect_id_columns <- function(df) {
  id_cols <- c()
  for (col in names(df)) {
    if (grepl("^id$|^ID$|patient.*id|record.*id|subject.*id|_id$", col, ignore.case = TRUE)) {
      id_cols <- c(id_cols, col)
    }
  }
  id_cols
}

# Detect derived variables
detect_derived_vars <- function(df) {
  derived <- c()
  patterns <- c("bmi", "egfr", "gfr", "crcl", "bsa", "ibw", "lbm", "ffm")
  for (p in patterns) {
    matches <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
    derived <- c(derived, matches)
  }
  unique(derived)
}

# Build exclusion list
exclude_list <- c()
exclude_list <- c(exclude_list, detect_id_columns(original_data))
if (EXCLUDE_DERIVED) {
  exclude_list <- c(exclude_list, detect_derived_vars(original_data))
}
if (!is.null(EXCLUDE_VARS)) {
  exclude_list <- c(exclude_list, EXCLUDE_VARS)
}
exclude_list <- unique(exclude_list)

# Get common columns
common_cols <- intersect(names(original_data), names(generated_data))
common_cols <- setdiff(common_cols, exclude_list)

# Filter to common columns
original_data <- original_data[, common_cols, drop = FALSE]
generated_data <- generated_data[, common_cols, drop = FALSE]

# Harmonize HEIGHT units (convert cm to m if needed)
height_col <- grep("^height$", names(original_data), ignore.case = TRUE, value = TRUE)[1]
if (!is.na(height_col) && height_col %in% names(original_data) && height_col %in% names(generated_data)) {
  orig_height_median <- median(original_data[[height_col]], na.rm = TRUE)
  gen_height_median <- median(generated_data[[height_col]], na.rm = TRUE)
  
  # If original is in cm (>3) and generated is in m (<3), convert original to m
  if (!is.na(orig_height_median) && !is.na(gen_height_median)) {
    if (orig_height_median > 3 && gen_height_median < 3) {
      original_data[[height_col]] <- original_data[[height_col]] / 100
    }
    # If original is in m (<3) and generated is in cm (>3), convert generated to m
    if (orig_height_median < 3 && gen_height_median > 3) {
      generated_data[[height_col]] <- generated_data[[height_col]] / 100
    }
  }
}

# Detect variable types
continuous_vars <- names(original_data)[sapply(original_data, function(x) {
  is.numeric(x) && length(unique(na.omit(x))) > 10
})]

categorical_vars <- setdiff(names(original_data), continuous_vars)

# Age and sex detection
age_col <- grep("^age$", names(original_data), ignore.case = TRUE, value = TRUE)[1]
sex_col <- grep("sex|gender|male", names(original_data), ignore.case = TRUE, value = TRUE)[1]
```

# Executive Summary

This report validates the vine copula model for the **`r dataset_name`** dataset by comparing `r format(n_generate, big.mark = ",")` generated virtual individuals against `r format(nrow(original_data), big.mark = ",")` observations from the original source data.

**Key findings:**

```{r exec-summary}
# Calculate key metrics
metrics <- list()

# Correlation RMSE
if (length(continuous_vars) >= 2) {
  cor_orig <- cor(original_data[, continuous_vars], method = "spearman", use = "pairwise.complete.obs")
  cor_gen <- cor(generated_data[, continuous_vars], method = "spearman", use = "pairwise.complete.obs")
  upper_idx <- upper.tri(cor_orig)
  cor_diff <- cor_orig[upper_idx] - cor_gen[upper_idx]
  cor_diff <- cor_diff[!is.na(cor_diff)]
  if (length(cor_diff) > 0) {
    metrics$cor_rmse <- sqrt(mean(cor_diff^2))
    metrics$cor_within_01 <- mean(abs(cor_diff) <= 0.1) * 100
  }
}

# KS statistics
ks_stats <- sapply(continuous_vars, function(v) {
  orig <- na.omit(original_data[[v]])
  gen <- na.omit(generated_data[[v]])
  if (length(orig) >= 10 && length(gen) >= 10) {
    ks.test(gen, orig)$statistic
  } else NA
})
ks_stats <- ks_stats[!is.na(ks_stats)]
if (length(ks_stats) > 0) {
  metrics$median_ks <- median(ks_stats)
  metrics$max_ks <- max(ks_stats)
}

# Interpretation
interpret_rmse <- function(x) {
  if (is.null(x) || is.na(x)) return("N/A")
  if (x < 0.05) return("Excellent")
  if (x < 0.10) return("Good")
  if (x < 0.15) return("Acceptable")
  return("Poor")
}

interpret_ks <- function(x) {
  if (is.null(x) || is.na(x)) return("N/A")
  if (x < 0.05) return("Excellent")
  if (x < 0.10) return("Good")
  if (x < 0.15) return("Acceptable")
  return("Poor")
}
```

| Metric | Value | Interpretation |
|--------|-------|----------------|
| Correlation RMSE | `r if(!is.null(metrics$cor_rmse)) sprintf("%.3f", metrics$cor_rmse) else "N/A"` | `r interpret_rmse(metrics$cor_rmse)` |
| Correlations within ±0.1 | `r if(!is.null(metrics$cor_within_01)) sprintf("%.0f%%", metrics$cor_within_01) else "N/A"` | |
| Median KS statistic | `r if(!is.null(metrics$median_ks)) sprintf("%.3f", metrics$median_ks) else "N/A"` | `r interpret_ks(metrics$median_ks)` |
| Max KS statistic | `r if(!is.null(metrics$max_ks)) sprintf("%.3f", metrics$max_ks) else "N/A"` | `r interpret_ks(metrics$max_ks)` |

**Interpretation thresholds:** RMSE/KS < 0.05 = Excellent, < 0.10 = Good, < 0.15 = Acceptable

\\newpage

# Data Overview

## Sample Characteristics

```{r sample-table}
sample_info <- data.frame(
  Characteristic = c(
    "Sample size",
    "Continuous variables",
    "Categorical variables",
    "Variables excluded"
  ),
  Original = c(
    format(nrow(original_data), big.mark = ","),
    length(continuous_vars),
    length(categorical_vars),
    ifelse(length(exclude_list) > 0, paste(exclude_list, collapse = ", "), "None")
  ),
  Generated = c(
    format(nrow(generated_data), big.mark = ","),
    length(continuous_vars),
    length(categorical_vars),
    "-"
  )
)

kable(sample_info, booktabs = TRUE, caption = "Sample characteristics") %>%
  kable_styling(latex_options = c("hold_position"))
```

## Descriptive Statistics

```{r desc-stats}
desc_table <- lapply(continuous_vars, function(v) {
  orig <- na.omit(original_data[[v]])
  gen <- na.omit(generated_data[[v]])
  data.frame(
    Variable = v,
    `Orig Mean` = sprintf("%.2f", mean(orig)),
    `Orig SD` = sprintf("%.2f", sd(orig)),
    `Orig Median` = sprintf("%.2f", median(orig)),
    `Gen Mean` = sprintf("%.2f", mean(gen)),
    `Gen SD` = sprintf("%.2f", sd(gen)),
    `Gen Median` = sprintf("%.2f", median(gen)),
    check.names = FALSE
  )
})
desc_table <- do.call(rbind, desc_table)

kable(desc_table, booktabs = TRUE, caption = "Descriptive statistics comparison",
      col.names = c("Variable", "Mean", "SD", "Median", "Mean", "SD", "Median")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  add_header_above(c(" " = 1, "Original" = 3, "Generated" = 3))
```

\\newpage

# Figure 1: Goodness-of-Fit Assessment

```{r figure1, fig.height=10, fig.width=12, fig.cap="Goodness-of-fit assessment of copula-generated virtual population. Panel A: Percentile comparison across variables. Panel B: P-P plots for variables with largest distributional differences. Panel C: Conditional percentile bands showing outcome-age relationships. Panel D: Correlation structure preservation."}

# Panel A: Percentile comparison
calc_percentiles <- function(orig, gen, var) {
  pcts <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  data.frame(
    variable = var,
    percentile = pcts * 100,
    original = quantile(na.omit(orig), pcts),
    generated = quantile(na.omit(gen), pcts)
  )
}

pct_data <- do.call(rbind, lapply(head(continuous_vars, 9), function(v) {
  calc_percentiles(original_data[[v]], generated_data[[v]], v)
}))

pct_long <- pivot_longer(pct_data, cols = c(original, generated),
                         names_to = "source", values_to = "value")

panel_a <- ggplot(pct_long, aes(x = percentile, y = value, color = source, shape = source)) +
  geom_line(aes(group = interaction(variable, source)), linewidth = 0.5, alpha = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("original" = "#1B9E77", "generated" = "#D95F02"),
                     labels = c("Original", "Generated")) +
  scale_shape_manual(values = c("original" = 16, "generated" = 17),
                     labels = c("Original", "Generated")) +
  labs(title = "A. Percentile Agreement", x = "Percentile", y = "Value", color = NULL, shape = NULL) +
  theme_bw(base_size = 9) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "grey90"))

# Panel B: P-P plots
calc_pp <- function(orig, gen, var) {
  orig <- na.omit(orig)
  gen <- na.omit(gen)
  if (length(orig) < 10 || length(gen) < 10) return(NULL)
  orig_ecdf <- ecdf(orig)
  probs <- seq(0.01, 0.99, length.out = 100)
  gen_q <- quantile(gen, probs)
  data.frame(variable = var, theoretical = probs, empirical = orig_ecdf(gen_q))
}

# Select top 6 by KS stat
top_ks <- names(sort(ks_stats, decreasing = TRUE))[1:min(6, length(ks_stats))]
pp_data <- do.call(rbind, lapply(top_ks, function(v) {
  calc_pp(original_data[[v]], generated_data[[v]], v)
}))

if (!is.null(pp_data) && nrow(pp_data) > 0) {
  pp_data$max_dev <- ave(abs(pp_data$empirical - pp_data$theoretical),
                         pp_data$variable, FUN = max)
  pp_data$label <- sprintf("%s (D=%.3f)", pp_data$variable, pp_data$max_dev)

  panel_b <- ggplot(pp_data, aes(x = theoretical, y = empirical)) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    geom_ribbon(aes(ymin = theoretical - 0.05, ymax = theoretical + 0.05),
                fill = "grey80", alpha = 0.5) +
    geom_line(color = "#2166AC", linewidth = 0.8) +
    facet_wrap(~label, ncol = 3) +
    coord_fixed(xlim = c(0,1), ylim = c(0,1)) +
    labs(title = "B. Probability-Probability Plots", x = "Theoretical CDF", y = "Empirical CDF") +
    theme_bw(base_size = 9) +
    theme(strip.background = element_rect(fill = "grey90"))
} else {
  panel_b <- ggplot() + theme_void() + labs(title = "B. P-P Plots: Insufficient data")
}

# Panel C: Conditional percentile bands
panel_c <- ggplot() + theme_void() + labs(title = "C. Conditional Bands: See Figure 3")

if (!is.na(age_col) && age_col %in% names(original_data)) {
  outcome_vars <- setdiff(continuous_vars, age_col)[1:min(4, length(continuous_vars)-1)]

  cond_data <- do.call(rbind, lapply(outcome_vars, function(v) {
    orig_df <- data.frame(age = original_data[[age_col]], outcome = original_data[[v]])
    gen_df <- data.frame(age = generated_data[[age_col]], outcome = generated_data[[v]])

    breaks <- c(0, 18, 40, 65, Inf)
    labels <- c("0-18", "18-40", "40-65", ">65")

    orig_df$bin <- cut(orig_df$age, breaks, labels = labels, include.lowest = TRUE)
    gen_df$bin <- cut(gen_df$age, breaks, labels = labels, include.lowest = TRUE)

    orig_stats <- orig_df %>%
      group_by(bin) %>%
      summarise(p05 = quantile(outcome, 0.05, na.rm=T),
                p50 = quantile(outcome, 0.50, na.rm=T),
                p95 = quantile(outcome, 0.95, na.rm=T),
                mid = median(age, na.rm=T), .groups = "drop") %>%
      mutate(source = "Original")

    gen_stats <- gen_df %>%
      group_by(bin) %>%
      summarise(p05 = quantile(outcome, 0.05, na.rm=T),
                p50 = quantile(outcome, 0.50, na.rm=T),
                p95 = quantile(outcome, 0.95, na.rm=T),
                mid = median(age, na.rm=T), .groups = "drop") %>%
      mutate(source = "Generated")

    rbind(orig_stats, gen_stats) %>%
      mutate(variable = v)
  }))

  if (nrow(cond_data) > 0) {
    panel_c <- ggplot(cond_data, aes(x = mid)) +
      geom_ribbon(aes(ymin = p05, ymax = p95, fill = source), alpha = 0.3) +
      geom_line(aes(y = p50, color = source), linewidth = 0.8) +
      facet_wrap(~variable, scales = "free_y", ncol = 2) +
      scale_fill_manual(values = c("Original" = "#1B9E77", "Generated" = "#D95F02")) +
      scale_color_manual(values = c("Original" = "#1B9E77", "Generated" = "#D95F02")) +
      labs(title = "C. Conditional Percentile Bands", x = "Age", y = "Value",
           fill = NULL, color = NULL) +
      theme_bw(base_size = 9) +
      theme(legend.position = "bottom", strip.background = element_rect(fill = "grey90"))
  }
}

# Panel D: Correlation preservation
if (length(continuous_vars) >= 2) {
  cor_orig <- cor(original_data[, continuous_vars], method = "spearman", use = "pairwise.complete.obs")
  cor_gen <- cor(generated_data[, continuous_vars], method = "spearman", use = "pairwise.complete.obs")

  upper_idx <- which(upper.tri(cor_orig), arr.ind = TRUE)
  cor_df <- data.frame(
    original = cor_orig[upper_idx],
    generated = cor_gen[upper_idx]
  ) %>%
    filter(!is.na(original) & !is.na(generated)) %>%
    mutate(within_01 = abs(original - generated) <= 0.1)

  rmse <- sqrt(mean((cor_df$original - cor_df$generated)^2))

  panel_d <- ggplot(cor_df, aes(x = original, y = generated, color = within_01)) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    geom_abline(slope = 1, intercept = c(-0.1, 0.1), color = "grey60", linetype = "dotted") +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_manual(values = c("TRUE" = "#1B9E77", "FALSE" = "#D95F02"),
                       labels = c("Within +/-0.1", "Outside +/-0.1")) +
    annotate("text", x = -0.8, y = 0.9, label = sprintf("RMSE = %.3f", rmse),
             hjust = 0, fontface = "bold", size = 3) +
    coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
    labs(title = "D. Correlation Preservation", x = "Original rho", y = "Generated rho",
         color = NULL) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom")
} else {
  panel_d <- ggplot() + theme_void() + labs(title = "D. Correlations: <2 variables")
}

# Combine
(panel_a + panel_b) / (panel_c + panel_d)
```

**Interpretation:** Panel A shows percentile agreement across variables - overlapping lines indicate good marginal distribution fit. Panel B shows P-P plots for variables with largest distributional differences; deviations from the diagonal beyond the grey band (+/-5%) indicate poor fit. Panel C shows conditional percentile bands for age-dependent relationships. Panel D shows correlation structure preservation; points near the diagonal with RMSE < 0.10 indicate excellent preservation.

\\newpage

# Figure 2: Distribution Comparisons

```{r figure2, fig.height=10, fig.width=12, fig.cap="Histogram comparisons of marginal distributions for all continuous variables. Green: original data, Orange: generated data."}

# Create combined data for histograms
hist_data <- do.call(rbind, lapply(continuous_vars, function(v) {
  data.frame(
    variable = v,
    value = c(original_data[[v]], generated_data[[v]]),
    source = rep(c("Original", "Generated"), c(nrow(original_data), nrow(generated_data)))
  )
}))

ggplot(hist_data, aes(x = value, fill = source)) +
  geom_histogram(aes(y = after_stat(density)), position = "identity",
                 alpha = 0.5, bins = 30) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("Original" = "#1B9E77", "Generated" = "#D95F02")) +
  labs(x = "Value", y = "Density", fill = NULL) +
  theme_bw(base_size = 9) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "grey90"))
```

**Interpretation:** Overlapping histograms indicate good marginal distribution preservation. The copula model captures the shape, location, and spread of each variable distribution. Systematic differences (e.g., shifted modes, different skewness) would indicate marginal distribution problems.

\\newpage

# Figure 3: Conditional Density Heatmaps

```{r figure3, fig.height=8, fig.width=12, fig.cap="Conditional density heatmaps showing bivariate relationships between key variables."}

# Select key relationships
if (!is.na(age_col) && length(continuous_vars) >= 2) {
  outcome_vars <- setdiff(continuous_vars, age_col)[1:min(2, length(continuous_vars)-1)]

  heat_plots <- lapply(outcome_vars, function(v) {
    # Combine data
    combined <- rbind(
      data.frame(x = original_data[[age_col]], y = original_data[[v]], source = "Original"),
      data.frame(x = generated_data[[age_col]], y = generated_data[[v]], source = "Generated")
    ) %>% filter(!is.na(x) & !is.na(y) & is.finite(x) & is.finite(y))
    
    # Skip if insufficient data
    if (nrow(combined) < 50) {
      return(ggplot() + theme_void() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Insufficient data for", v)))
    }
    
    # Use geom_bin2d as more reliable alternative to geom_hex
    ggplot(combined, aes(x = x, y = y)) +
      geom_bin2d(bins = 30) +
      facet_wrap(~source) +
      scale_fill_viridis_c(option = "plasma", trans = "log1p") +
      labs(title = paste(v, "|", age_col), x = age_col, y = v) +
      theme_bw(base_size = 9) +
      theme(strip.background = element_rect(fill = "grey90"),
            legend.position = "right")
  })

  # Filter out NULL plots
  heat_plots <- heat_plots[!sapply(heat_plots, is.null)]
  
  if (length(heat_plots) >= 2) {
    heat_plots[[1]] / heat_plots[[2]]
  } else if (length(heat_plots) == 1) {
    heat_plots[[1]]
  } else {
    ggplot() + theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = "Could not generate heatmaps")
  }
} else {
  ggplot() + theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = "Insufficient variables for heatmaps")
}
```

**Interpretation:** Conditional density heatmaps reveal the bivariate structure between variables. Good copula fit is indicated by similar density patterns between original (left) and generated (right) panels. High-density regions (bright colors) should occur in the same locations. These plots are particularly sensitive to tail dependence - the copula ability to capture extreme value combinations.

\\newpage

# Summary Statistics

```{r summary-table}
# Build comprehensive summary table
summary_df <- data.frame(
  Metric = c(
    "Original sample size",
    "Generated sample size",
    "Continuous variables analyzed",
    "Correlation RMSE",
    "Correlations within +/-0.1",
    "Median KS statistic",
    "Maximum KS statistic",
    "--- Thresholds ---",
    "Excellent fit",
    "Good fit",
    "Acceptable fit"
  ),
  Value = c(
    format(nrow(original_data), big.mark = ","),
    format(nrow(generated_data), big.mark = ","),
    as.character(length(continuous_vars)),
    if(!is.null(metrics$cor_rmse)) sprintf("%.4f", metrics$cor_rmse) else "N/A",
    if(!is.null(metrics$cor_within_01)) sprintf("%.1f%%", metrics$cor_within_01) else "N/A",
    if(!is.null(metrics$median_ks)) sprintf("%.4f", metrics$median_ks) else "N/A",
    if(!is.null(metrics$max_ks)) sprintf("%.4f", metrics$max_ks) else "N/A",
    "",
    "RMSE/KS < 0.05",
    "RMSE/KS < 0.10",
    "RMSE/KS < 0.15"
  )
)

kable(summary_df, booktabs = TRUE, caption = "Validation summary statistics") %>%
  kable_styling(latex_options = c("hold_position"))
```

# Conclusions

```{r conclusions, results="asis"}
# Auto-generate conclusions based on metrics
conclusions <- c()

if (!is.null(metrics$cor_rmse)) {
  if (metrics$cor_rmse < 0.05) {
    conclusions <- c(conclusions, "- **Correlation structure:** Excellent preservation (RMSE < 0.05)")
  } else if (metrics$cor_rmse < 0.10) {
    conclusions <- c(conclusions, "- **Correlation structure:** Good preservation (RMSE < 0.10)")
  } else if (metrics$cor_rmse < 0.15) {
    conclusions <- c(conclusions, "- **Correlation structure:** Acceptable preservation (RMSE < 0.15)")
  } else {
    conclusions <- c(conclusions, "- **Correlation structure:** Poor preservation (RMSE >= 0.15) - consider model refinement")
  }
}

if (!is.null(metrics$median_ks)) {
  if (metrics$median_ks < 0.05) {
    conclusions <- c(conclusions, "- **Marginal distributions:** Excellent fit (median KS < 0.05)")
  } else if (metrics$median_ks < 0.10) {
    conclusions <- c(conclusions, "- **Marginal distributions:** Good fit (median KS < 0.10)")
  } else if (metrics$median_ks < 0.15) {
    conclusions <- c(conclusions, "- **Marginal distributions:** Acceptable fit (median KS < 0.15)")
  } else {
    conclusions <- c(conclusions, "- **Marginal distributions:** Poor fit (median KS >= 0.15) - consider model refinement")
  }
}

if (length(conclusions) == 0) {
  conclusions <- "Insufficient data for automated conclusions."
}

cat(paste(conclusions, collapse = "\\n\\n"))
```

---

*Report generated by copulapop on `r Sys.time()`*
'
}
