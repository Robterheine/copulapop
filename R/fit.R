# ============================================================================
# Vine Copula Fitting Functions
# ============================================================================

#' Fit a Vine Copula from Data
#'
#' Fits an R-Vine copula model to a dataset, creating a copula object that can
#' be used to generate virtual populations. The function automatically handles
#' age-sex stratification and extracts marginal quantiles.
#'
#' @param data A data.frame containing the source data
#' @param name Character string naming this copula (e.g., "My Hospital")
#' @param description Optional description of the dataset
#' @param variables Character vector of variable names to include in the copula.
#'   Must include "AGE". Default: c("AGE", "HEIGHT", "WEIGHT")
#' @param sex_column Name of the sex column. Should contain 1/TRUE/"male"/"M" for

#'   male and 0/FALSE/"female"/"F" for female. Default: "SEX" or "SEXISMALE"
#' @param height_in_cm Logical. If TRUE, HEIGHT is converted from cm to meters.
#'   Default: TRUE
#' @param country Optional country name for metadata
#' @param source Optional source description
#' @param source_url Optional URL to data source
#' @param adult_only Logical. If TRUE, only fit adult (>=18) strata. Default: FALSE
#' @param n_quantiles Number of quantiles to store for marginal distributions.
#'   Default: 1001
#' @param min_stratum_n Minimum observations required per stratum. Default: 50
#' @param verbose Logical. Print progress messages. Default: TRUE
#'
#' @return A list of class "copulapop_copula" containing:
#'   \itemize{
#'     \item \code{info}: Metadata (name, description, country, etc.)
#'     \item \code{variables}: Variables included in the copula
#'     \item \code{overall}: Overall statistics
#'     \item \code{strata}: Fitted copula parameters per stratum
#'     \item \code{quantile_probs}: Quantile probabilities used
#'   }
#'
#' @details
#' The function fits separate vine copulas for each age-sex stratum:
#' \itemize{
#'   \item pediatric_0: Children (<18) female
#'   \item pediatric_1: Children (<18) male
#'   \item pediatric_all: All children
#'   \item adult_0: Adults (>=18) female
#'   \item adult_1: Adults (>=18) male
#'   \item adult_all: All adults
#'   \item all: Combined dataset
#' }
#'
#' If vine copula fitting fails for a stratum (insufficient data or numerical
#' issues), a Gaussian copula fallback is used based on the Spearman correlation
#' matrix.
#'
#' @examples
#' \dontrun{
#' # Load your data
#' my_data <- read.csv("hospital_data.csv")
#'
#' # Fit copula with default variables
#' copula <- fit_copula(my_data, name = "My Hospital")
#'
#' # Fit with additional variables
#' copula <- fit_copula(
#'   my_data,
#'   name = "My Hospital with Creatinine",
#'   variables = c("AGE", "HEIGHT", "WEIGHT", "CREAT"),
#'   description = "Hospitalized patients 2020-2024"
#' )
#'
#' # Generate population from fitted copula
#' pop <- generate_population_from_copula(copula, n = 1000)
#'
#' # Save for later use
#' save_copula(copula, "my_hospital.json")
#' }
#'
#' @seealso \code{\link{save_copula}}, \code{\link{load_copula}},
#'   \code{\link{generate_population_from_copula}}
#'
#' @export
fit_copula <- function(data,
                       name = "Custom Dataset",
                       description = NULL,
                       variables = c("AGE", "HEIGHT", "WEIGHT"),
                       sex_column = NULL,
                       height_in_cm = TRUE,
                       country = NULL,
                       source = NULL,
                       source_url = NULL,
                       adult_only = FALSE,
                       n_quantiles = 1001,
                       min_stratum_n = 50,
                       verbose = TRUE) {
  
  # Convert to data.table for efficiency
  dt <- data.table::as.data.table(data)
  
  # Standardize column names to uppercase
  names(dt) <- toupper(names(dt))
  variables <- toupper(variables)
  
  # Validate AGE is present
  if (!"AGE" %in% variables) {
    stop("'AGE' must be included in variables")
  }
  
  # Check all variables exist
  missing_vars <- setdiff(variables, names(dt))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }
  
  # Find and process sex column
  if (is.null(sex_column)) {
    sex_column <- intersect(c("SEXISMALE", "SEX", "GENDER", "MALE"), names(dt))[1]
  } else {
    sex_column <- toupper(sex_column)
  }
  
  if (is.na(sex_column) || !sex_column %in% names(dt)) {
    if (verbose) message("No sex column found. Assuming 50/50 male/female split.")
    dt[, SEXISMALE := sample(0:1, .N, replace = TRUE)]
  } else {
    # Convert sex to SEXISMALE (1 = male, 0 = female)
    sex_vals <- dt[[sex_column]]
    if (is.numeric(sex_vals)) {
      dt[, SEXISMALE := as.integer(sex_vals == 1)]
    } else {
      dt[, SEXISMALE := as.integer(toupper(as.character(sex_vals)) %in% c("M", "MALE", "1", "TRUE"))]
    }
  }
  
  # Convert HEIGHT from cm to m if needed
  if ("HEIGHT" %in% variables && height_in_cm) {
    if (max(dt$HEIGHT, na.rm = TRUE) > 3) {  # Likely in cm
      dt[, HEIGHT := HEIGHT / 100]
      if (verbose) message("Converted HEIGHT from cm to meters")
    }
  }
  
  # Ensure numeric variables
  for (v in variables) {
    dt[[v]] <- as.numeric(dt[[v]])
  }
  
  # Create age group
  dt[, AGE_GROUP := ifelse(AGE < 18, "pediatric", "adult")]
  
  # Remove rows with missing values in key variables
  complete_vars <- c(variables, "SEXISMALE")
  dt_complete <- dt[stats::complete.cases(dt[, ..complete_vars])]
  
  if (verbose) {
    message("Data preparation:")
    message("  Total rows: ", nrow(dt))
    message("  Complete cases: ", nrow(dt_complete))
    message("  Variables: ", paste(variables, collapse = ", "))
  }
  
  # Calculate overall statistics
  n_pediatric <- sum(dt_complete$AGE_GROUP == "pediatric")
  n_adult <- sum(dt_complete$AGE_GROUP == "adult")
  
  if (adult_only) n_pediatric <- 0
  
  overall <- list(
    n_total = nrow(dt_complete),
    n_pediatric = n_pediatric,
    n_adult = n_adult,
    prop_male = mean(dt_complete$SEXISMALE),
    age_min = min(dt_complete$AGE),
    age_max = max(dt_complete$AGE)
  )
  
  if (verbose) {
    message("  Pediatric: ", n_pediatric)
    message("  Adult: ", n_adult)
    message("  Prop male: ", round(overall$prop_male, 2))
  }
  
  # Quantile probabilities
  quantile_probs <- seq(0, 1, length.out = n_quantiles)
  
  # Define strata to fit
  strata_defs <- list(
    list(name = "pediatric_0", filter = quote(AGE_GROUP == "pediatric" & SEXISMALE == 0)),
    list(name = "pediatric_1", filter = quote(AGE_GROUP == "pediatric" & SEXISMALE == 1)),
    list(name = "pediatric_all", filter = quote(AGE_GROUP == "pediatric")),
    list(name = "adult_0", filter = quote(AGE_GROUP == "adult" & SEXISMALE == 0)),
    list(name = "adult_1", filter = quote(AGE_GROUP == "adult" & SEXISMALE == 1)),
    list(name = "adult_all", filter = quote(AGE_GROUP == "adult")),
    list(name = "all", filter = quote(TRUE))
  )
  
  # Fit copulas per stratum
  strata <- list()
  
  if (verbose) message("\nFitting vine copulas:")
  
  for (stratum_def in strata_defs) {
    stratum_name <- stratum_def$name
    
    # Skip pediatric strata if adult_only
    if (adult_only && grepl("pediatric", stratum_name)) next
    
    # Filter data for this stratum
    stratum_dt <- dt_complete[eval(stratum_def$filter)]
    
    if (nrow(stratum_dt) < min_stratum_n) {
      if (verbose) message("  ", stratum_name, ": ", nrow(stratum_dt), " obs - skipping (< ", min_stratum_n, ")")
      next
    }
    
    if (verbose) message("  ", stratum_name, ": ", nrow(stratum_dt), " obs... ", appendLF = FALSE)
    
    # Fit vine copula
    vine_result <- fit_vine_stratum(stratum_dt, variables, min_stratum_n)
    
    if (is.null(vine_result)) {
      if (verbose) message("FAILED")
      next
    }
    
    if (verbose) message("done (", vine_result$type, ")")
    
    # Extract quantiles for marginal distributions
    quantiles <- list()
    for (v in variables) {
      quantiles[[v]] <- as.numeric(stats::quantile(stratum_dt[[v]], probs = quantile_probs, na.rm = TRUE))
    }
    
    # Combine into stratum result
    strata[[stratum_name]] <- c(
      vine_result,
      list(
        n = nrow(stratum_dt),
        prop_of_total = nrow(stratum_dt) / nrow(dt_complete),
        quantiles = quantiles
      )
    )
  }
  
  if (length(strata) == 0) {
    stop("Failed to fit any strata. Check your data.")
  }
  
  # Build result object
  result <- list(
    info = list(
      name = name,
      description = description,
      population = NULL,
      country = country,
      has_creatinine = "CREAT" %in% variables,
      has_albumin = "ALBUMIN" %in% variables,
      source = source,
      source_url = source_url,
      warning = if (adult_only) "Adult population only" else NULL,
      adult_only = adult_only,
      fitted_date = as.character(Sys.Date()),
      copulapop_version = as.character(utils::packageVersion("copulapop"))
    ),
    variables = variables,
    overall = overall,
    strata = strata,
    quantile_probs = quantile_probs
  )
  
  class(result) <- c("copulapop_copula", "list")
  
  if (verbose) {
    message("\nCopula fitting complete!")
    message("  Strata fitted: ", length(strata))
    message("  Variables: ", paste(variables, collapse = ", "))
  }
  
  return(result)
}


#' Internal function to fit vine copula for a single stratum
#' @keywords internal
fit_vine_stratum <- function(dt, variables, min_n = 50) {
  # Extract data matrix
  dt_vars <- dt[, ..variables]
  dt_complete <- stats::na.omit(dt_vars)
  
  if (nrow(dt_complete) < min_n) return(NULL)
  
  n_vars <- length(variables)
  
  # Convert to pseudo-observations (ranks scaled to [0,1])
  u_data <- VineCopula::pobs(as.matrix(dt_complete))
  
  # Try to fit R-Vine copula
  vine <- tryCatch({
    VineCopula::RVineStructureSelect(
      u_data,
      familyset = c(1, 2, 3, 4, 5, 6),  # Gaussian, t, Clayton, Gumbel, Frank, Joe
      type = "RVine",
      selectioncrit = "AIC",
      indeptest = TRUE
    )
  }, error = function(e) {
    NULL
  })
  
  if (is.null(vine)) {
    # Fallback to Gaussian copula
    cor_mat <- stats::cor(dt_complete, method = "spearman")
    cor_mat <- as.matrix(Matrix::nearPD(cor_mat, corr = TRUE)$mat)
    return(list(
      type = "gaussian",
      cor_matrix = as.list(as.data.frame(cor_mat))
    ))
  }
  
  # Extract vine copula parameters
  list(
    type = "vine",
    matrix = as.list(as.data.frame(vine$Matrix)),
    family = as.list(as.data.frame(vine$family)),
    par = as.list(as.data.frame(vine$par)),
    par2 = as.list(as.data.frame(vine$par2)),
    cor_matrix = as.list(as.data.frame(stats::cor(dt_complete, method = "spearman")))
  )
}


#' Save Copula to JSON File
#'
#' Saves a fitted copula object to a JSON file for later use or sharing.
#'
#' @param copula A copulapop_copula object from \code{fit_copula()}
#' @param file Path to the output JSON file
#' @param pretty Logical. Use pretty (indented) JSON formatting. Default: FALSE
#'   (compact format for smaller file size)
#'
#' @return Invisibly returns the file path
#'
#' @examples
#' \dontrun{
#' copula <- fit_copula(my_data, name = "My Hospital")
#' save_copula(copula, "my_hospital.json")
#' }
#'
#' @seealso \code{\link{fit_copula}}, \code{\link{load_copula}}
#'
#' @export
save_copula <- function(copula, file, pretty = FALSE) {
  if (!inherits(copula, "copulapop_copula")) {
    stop("copula must be a copulapop_copula object from fit_copula()")
  }
  
  json_output <- jsonlite::toJSON(copula, auto_unbox = TRUE, pretty = pretty, digits = 8)
  writeLines(json_output, file)
  
  message("Saved copula to: ", file)
  invisible(file)
}


#' Load Copula from JSON File
#'
#' Loads a previously saved copula from a JSON file.
#'
#' @param file Path to the JSON file
#'
#' @return A copulapop_copula object
#'
#' @examples
#' \dontrun{
#' copula <- load_copula("my_hospital.json")
#' pop <- generate_population_from_copula(copula, n = 1000)
#' }
#'
#' @seealso \code{\link{fit_copula}}, \code{\link{save_copula}}
#'
#' @export
load_copula <- function(file) {
  if (!file.exists(file)) {
    stop("File not found: ", file)
  }
  
  copula <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  class(copula) <- c("copulapop_copula", "list")
  
  return(copula)
}


#' Generate Population from Copula Object
#'
#' Generates a virtual population directly from a copulapop_copula object,
#' without needing to save/load from JSON.
#'
#' @param copula A copulapop_copula object from \code{fit_copula()} or
#'   \code{load_copula()}
#' @param n Integer specifying the number of virtual patients to generate
#' @param age_range Numeric vector of length 2 specifying the age range
#'   (default: full range of source data)
#' @param sex Character string: "both" (default), "male", or "female"
#' @param seed Optional integer for reproducibility
#'
#' @return A data.frame containing the generated virtual population
#'
#' @examples
#' \dontrun{
#' # Fit and generate in one workflow
#' copula <- fit_copula(my_data, name = "My Hospital")
#' pop <- generate_population_from_copula(copula, n = 1000)
#'
#' # Or load existing copula
#' copula <- load_copula("my_hospital.json")
#' pop <- generate_population_from_copula(copula, n = 500, age_range = c(18, 65))
#' }
#'
#' @seealso \code{\link{fit_copula}}, \code{\link{generate_population}}
#'
#' @export
generate_population_from_copula <- function(copula,
                                             n = 1000,
                                             age_range = NULL,
                                             sex = "both",
                                             seed = NULL) {
  
  if (!inherits(copula, "copulapop_copula") && !is.list(copula)) {
    stop("copula must be a copulapop_copula object or list")
  }
  
  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)
  
  # Use the copula object directly (same structure as JSON)
  ds <- copula
  
  # Set default age range
  if (is.null(age_range)) {
    age_range <- c(ds$overall$age_min, ds$overall$age_max)
  }
  
  # Validate age range
  if (age_range[1] > ds$overall$age_max || age_range[2] < ds$overall$age_min) {
    stop("Selected age range outside source data range (",
         ds$overall$age_min, "-", ds$overall$age_max, " years)")
  }
  
  # Validate sex option
  sex <- tolower(sex)
  if (!sex %in% c("both", "male", "female")) {
    stop("sex must be 'both', 'male', or 'female'")
  }
  
  # Get variables and quantile probabilities
  variables <- unlist(ds$variables)
  quantile_probs <- unlist(ds$quantile_probs)
  
  # Determine generation strategy
  has_pediatric <- age_range[1] < 18 && ds$overall$n_pediatric > 0
  has_adult <- age_range[2] >= 18 && ds$overall$n_adult > 0
  
  # Calculate proportions
  if (!has_pediatric) {
    prop_ped <- 0
  } else if (!has_adult) {
    prop_ped <- 1
  } else {
    prop_ped <- ds$overall$n_pediatric / ds$overall$n_total
  }
  
  # Sex proportions
  if (sex == "male") {
    prop_male <- 1
  } else if (sex == "female") {
    prop_male <- 0
  } else {
    prop_male <- ds$overall$prop_male
  }
  
  # Calculate numbers per stratum (oversample for age filtering)
  oversample <- 1.5
  n_gen <- ceiling(n * oversample)
  
  n_ped <- round(n_gen * prop_ped)
  n_adult <- n_gen - n_ped
  
  n_ped_m <- round(n_ped * prop_male)
  n_ped_f <- n_ped - n_ped_m
  n_adult_m <- round(n_adult * prop_male)
  n_adult_f <- n_adult - n_adult_m
  
  # Generate from each stratum
  generated_list <- list()
  
  strata <- list(
    list(key = "pediatric_1", sex = 1, n = n_ped_m),
    list(key = "pediatric_0", sex = 0, n = n_ped_f),
    list(key = "adult_1", sex = 1, n = n_adult_m),
    list(key = "adult_0", sex = 0, n = n_adult_f)
  )
  
  for (s in strata) {
    if (s$n == 0) next
    
    # Get stratum stats with fallbacks
    stratum_stats <- ds$strata[[s$key]]
    if (is.null(stratum_stats) || stratum_stats$n < 20) {
      age_group <- if (grepl("pediatric", s$key)) "pediatric_all" else "adult_all"
      stratum_stats <- ds$strata[[age_group]]
    }
    if (is.null(stratum_stats) || stratum_stats$n < 20) {
      stratum_stats <- ds$strata[["all"]]
    }
    
    # Generate from this stratum
    sim <- generate_from_stratum_internal(stratum_stats, s$n, variables, quantile_probs)
    
    if (!is.null(sim) && nrow(sim) > 0) {
      sim$SEXISMALE <- s$sex
      generated_list[[s$key]] <- sim
    }
  }
  
  # Combine all strata
  if (length(generated_list) == 0) {
    stop("Failed to generate any data")
  }
  
  generated <- data.table::rbindlist(generated_list, fill = TRUE)
  
  # Filter by age range
  generated <- generated[AGE >= age_range[1] & AGE <= age_range[2], ]
  
  if (nrow(generated) < n) {
    warning("Could only generate ", nrow(generated), " of ", n, 
            " requested individuals in specified age range")
  } else {
    # Trim to requested size
    generated <- generated[sample(.N, n), ]
  }
  
  # Calculate derived variables if HEIGHT and WEIGHT exist
  if ("HEIGHT" %in% names(generated) && "WEIGHT" %in% names(generated)) {
    generated[, BMI := WEIGHT / (HEIGHT^2)]
    generated[, BSA := sqrt((HEIGHT * 100 * WEIGHT) / 3600)]
  }
  
  # Calculate eGFR if creatinine available
  if ("CREAT" %in% names(generated) && "AGE" %in% names(generated)) {
    height_col <- if ("HEIGHT" %in% names(generated)) generated$HEIGHT else NULL
    generated[, EGFR := calculate_egfr(CREAT, AGE, SEXISMALE, height_col)]
  }
  
  # Add ID
  generated[, ID := .I]
  
  # Reorder columns
  first_cols <- c("ID", "SEXISMALE")
  other_cols <- setdiff(names(generated), first_cols)
  data.table::setcolorder(generated, c(first_cols, other_cols))
  
  # Return as data.frame
  return(as.data.frame(generated))
}


#' Internal function to generate from a single stratum
#' @keywords internal
generate_from_stratum_internal <- function(stratum_stats, n_generate, variables, quantile_probs) {
  if (is.null(stratum_stats) || n_generate == 0) return(NULL)
  
  n_vars <- length(variables)
  
  # Check copula type
  use_vine <- !is.null(stratum_stats$type) && stratum_stats$type == "vine"
  
  u_sim <- NULL
  
  if (use_vine) {
    # Reconstruct and simulate from vine copula
    u_sim <- tryCatch({
      vine_matrix <- matrix(unlist(stratum_stats$matrix), nrow = n_vars, byrow = FALSE)
      family_matrix <- matrix(unlist(stratum_stats$family), nrow = n_vars, byrow = FALSE)
      par_matrix <- matrix(unlist(stratum_stats$par), nrow = n_vars, byrow = FALSE)
      par2_matrix <- matrix(unlist(stratum_stats$par2), nrow = n_vars, byrow = FALSE)
      
      storage.mode(vine_matrix) <- "integer"
      storage.mode(family_matrix) <- "integer"
      
      RVM <- VineCopula::RVineMatrix(
        Matrix = vine_matrix,
        family = family_matrix,
        par = par_matrix,
        par2 = par2_matrix
      )
      
      VineCopula::RVineSim(n_generate, RVM)
    }, error = function(e) {
      NULL
    })
  }
  
  # Fallback to Gaussian copula
  if (is.null(u_sim)) {
    cor_mat <- stratum_stats$cor_matrix
    if (is.list(cor_mat)) {
      cor_mat <- matrix(unlist(cor_mat), nrow = n_vars, byrow = TRUE)
    }
    cor_mat <- as.matrix(cor_mat)
    cor_mat <- Matrix::nearPD(cor_mat, corr = TRUE)$mat
    cor_mat <- as.matrix(cor_mat)
    
    u_sim <- tryCatch({
      z <- MASS::mvrnorm(n_generate, mu = rep(0, n_vars), Sigma = cor_mat)
      stats::pnorm(z)
    }, error = function(e) {
      matrix(stats::runif(n_generate * n_vars), ncol = n_vars)
    })
  }
  
  # Transform to original scale using quantile interpolation
  sim_mat <- matrix(NA, nrow = n_generate, ncol = n_vars)
  colnames(sim_mat) <- variables
  
  for (j in seq_along(variables)) {
    var <- variables[j]
    stored_q <- unlist(stratum_stats$quantiles[[var]])
    stored_p <- quantile_probs
    sim_mat[, j] <- stats::approx(x = stored_p, y = stored_q, 
                                   xout = u_sim[, j], rule = 2)$y
  }
  
  return(data.table::as.data.table(sim_mat))
}


#' Print method for copulapop_copula objects
#' @export
print.copulapop_copula <- function(x, ...) {
  cat("Vine Copula: ", x$info$name, "\n", sep = "")
  cat("──────────────────────────────────────────\n")
  
  if (!is.null(x$info$description)) {
    cat("Description: ", x$info$description, "\n")
  }
  if (!is.null(x$info$country)) {
    cat("Country:     ", x$info$country, "\n")
  }
  
  cat("Variables:   ", paste(x$variables, collapse = ", "), "\n")
  cat("Total N:     ", x$overall$n_total, "\n")
  cat("Age range:   ", x$overall$age_min, " - ", x$overall$age_max, " years\n", sep = "")
  cat("Pediatric:   ", x$overall$n_pediatric, "\n")
  cat("Adult:       ", x$overall$n_adult, "\n")
  cat("Prop male:   ", round(x$overall$prop_male, 2), "\n")
  cat("Strata:      ", paste(names(x$strata), collapse = ", "), "\n")
  
  if (!is.null(x$info$fitted_date)) {
    cat("Fitted:      ", x$info$fitted_date, "\n")
  }
  
  invisible(x)
}


#' Summary method for copulapop_copula objects
#' @export
summary.copulapop_copula <- function(object, ...) {
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("COPULAPOP COPULA SUMMARY: ", object$info$name, "\n", sep = "")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  cat("METADATA\n")
  cat("────────────────────────────────────────\n")
  if (!is.null(object$info$description)) cat("  Description: ", object$info$description, "\n")
  if (!is.null(object$info$country)) cat("  Country:     ", object$info$country, "\n")
  if (!is.null(object$info$source)) cat("  Source:      ", object$info$source, "\n")
  if (!is.null(object$info$source_url)) cat("  URL:         ", object$info$source_url, "\n")
  if (!is.null(object$info$fitted_date)) cat("  Fitted:      ", object$info$fitted_date, "\n")
  
  cat("\nDATA OVERVIEW\n")
  cat("────────────────────────────────────────\n")
  cat("  Variables:     ", paste(object$variables, collapse = ", "), "\n")
  cat("  Total N:       ", object$overall$n_total, "\n")
  cat("  Age range:     ", object$overall$age_min, " - ", object$overall$age_max, " years\n", sep = "")
  cat("  Pediatric N:   ", object$overall$n_pediatric, " (", 
      round(100 * object$overall$n_pediatric / object$overall$n_total, 1), "%)\n", sep = "")
  cat("  Adult N:       ", object$overall$n_adult, " (", 
      round(100 * object$overall$n_adult / object$overall$n_total, 1), "%)\n", sep = "")
  cat("  Male prop:     ", round(object$overall$prop_male, 3), "\n")
  
  cat("\nSTRATA DETAILS\n")
  cat("────────────────────────────────────────\n")
  for (stratum_name in names(object$strata)) {
    s <- object$strata[[stratum_name]]
    cat(sprintf("  %-15s N=%-6d (%s)\n", stratum_name, s$n, s$type))
  }
  
  cat("\n")
  invisible(object)
}
