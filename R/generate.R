# ============================================================================
# Virtual Population Generation
# ============================================================================

#' Generate a Virtual Population
#'
#' Generates a virtual patient population using vine copula-based simulation.
#' The function preserves correlation structures and tail dependencies between
#' covariates while maintaining realistic marginal distributions.
#'
#' @param dataset Character string specifying which dataset to use. 
#'   Available datasets can be listed with \code{list_datasets()}.
#' @param n Integer specifying the number of virtual patients to generate.
#' @param age_range Numeric vector of length 2 specifying the age range 
#'   (default: full range of source data).
#' @param sex Character string: "both" (default), "male", or "female".
#' @param seed Optional integer for reproducibility.
#'
#' @return A data.frame containing the generated virtual population with columns:
#'   \itemize{
#'     \item ID: Patient identifier
#'     \item SEXISMALE: Sex indicator (1 = male, 0 = female)
#'     \item AGE: Age in years
#'     \item HEIGHT: Height in meters
#'     \item WEIGHT: Weight in kilograms
#'     \item BMI: Body mass index (kg/m²)
#'     \item BSA: Body surface area (m²)
#'     \item CREAT: Serum creatinine in µmol/L (if available)
#'     \item EGFR: Estimated GFR in mL/min/1.73m² (if creatinine available)
#'   }
#'
#' @details
#' The function uses pre-fitted R-Vine copula models to generate correlated
#' covariates. Age-sex stratification ensures appropriate covariate 
#' relationships are preserved for different demographic groups.
#'
#' eGFR is calculated using:
#' \itemize{
#'   \item Adults (≥18 years): CKD-EPI 2021 race-free equation
#'   \item Children (<18 years): Bedside Schwartz formula
#' }
#'
#' @examples
#' \dontrun{
#' # Generate 1000 patients from RUMC dataset
#' pop <- generate_population("RUMC", n = 1000)
#'
#' # Generate adult patients only
#' adults <- generate_population("NHANES", n = 500, age_range = c(18, 80))
#'
#' # Generate pediatric females
#' ped_f <- generate_population("CHNS", n = 200, 
#'                               age_range = c(2, 17), sex = "female")
#' }
#'
#' @export
generate_population <- function(dataset, 
                                 n = 1000,
                                 age_range = NULL,
                                 sex = "both",
                                 seed = NULL) {
  
  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)
  
  # Load dataset
  path <- get_data_path()
  if (is.null(path)) {
    stop("Data path not set. Use set_data_path() first.")
  }
  
  json_file <- paste0(path, dataset, ".json")
  if (!file.exists(json_file)) {
    stop("Dataset not found: ", dataset,
         "\nAvailable: ", paste(list_datasets(), collapse = ", "))
  }
  
  ds <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)
  
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
    sim <- generate_from_stratum(stratum_stats, s$n, variables, quantile_probs)
    
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
  
  # Calculate derived variables
  generated[, BMI := WEIGHT / (HEIGHT^2)]
  generated[, BSA := sqrt((HEIGHT * 100 * WEIGHT) / 3600)]
  
  # Calculate eGFR if creatinine available
  if ("CREAT" %in% names(generated)) {
    generated[, EGFR := calculate_egfr(CREAT, AGE, SEXISMALE, HEIGHT)]
  }
  
  # Add ID and reorder columns
  generated[, ID := .I]
  
  base_cols <- c("ID", "SEXISMALE", "AGE", "HEIGHT", "WEIGHT", "BMI", "BSA")
  if ("CREAT" %in% names(generated)) base_cols <- c(base_cols, "CREAT")
  if ("EGFR" %in% names(generated)) base_cols <- c(base_cols, "EGFR")
  if ("ALBUMIN" %in% names(generated)) base_cols <- c(base_cols, "ALBUMIN")
  
  data.table::setcolorder(generated, intersect(base_cols, names(generated)))
  
  # Return as data.frame
  return(as.data.frame(generated))
}


# Internal function to generate from a single stratum
generate_from_stratum <- function(stratum_stats, n_generate, variables, quantile_probs) {
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
      message("Vine simulation failed, using Gaussian fallback: ", e$message)
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


# Internal function to calculate eGFR
calculate_egfr <- function(creat_umol, age, sex_is_male, height_m = NULL) {
  creat_mgdl <- creat_umol / 88.4
  egfr <- rep(NA_real_, length(creat_umol))
  
  # Adults: CKD-EPI 2021 (race-free)
  adult_idx <- which(age >= 18)
  if (length(adult_idx) > 0) {
    kappa <- ifelse(sex_is_male[adult_idx] == 1, 0.9, 0.7)
    alpha <- ifelse(sex_is_male[adult_idx] == 1, -0.302, -0.241)
    sex_factor <- ifelse(sex_is_male[adult_idx] == 1, 1, 1.012)
    cr_kappa <- creat_mgdl[adult_idx] / kappa
    egfr[adult_idx] <- 142 * pmin(cr_kappa, 1)^alpha * 
                        pmax(cr_kappa, 1)^(-1.2) * 
                        0.9938^age[adult_idx] * sex_factor
  }
  
  # Children: Bedside Schwartz
  child_idx <- which(age < 18)
  if (length(child_idx) > 0 && !is.null(height_m)) {
    height_cm <- height_m[child_idx] * 100
    egfr[child_idx] <- 0.413 * height_cm / creat_mgdl[child_idx]
  }
  
  return(egfr)
}


#' Generate Population from Custom JSON File
#'
#' Generates a virtual patient population from a custom-fitted vine copula
#' stored in a JSON file. This allows validation of user-fitted copulas.
#'
#' @param json_file Path to the JSON file containing the fitted copula
#' @param n Integer specifying the number of virtual patients to generate
#' @param age_range Numeric vector of length 2 specifying the age range 
#'   (default: full range of source data)
#' @param sex Character string: "both" (default), "male", or "female"
#' @param seed Optional integer for reproducibility
#'
#' @return A data.frame containing the generated virtual population
#'
#' @details
#' The JSON file must follow the copulapop format with:
#' \itemize{
#'   \item \code{variables}: Vector of variable names
#'   \item \code{quantile_probs}: Vector of quantile probabilities
#'   \item \code{overall}: Overall statistics (n_total, age_min, age_max, prop_male, etc.)
#'   \item \code{strata}: Stratified copula parameters (with vine matrix, family, par, par2)
#' }
#'
#' @examples
#' \dontrun{
#' # Generate from custom fitted copula
#' pop <- generate_population_from_json("my_copula.json", n = 1000)
#' }
#'
#' @export
generate_population_from_json <- function(json_file, 
                                           n = 1000,
                                           age_range = NULL,
                                           sex = "both",
                                           seed = NULL) {
  
  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)
  
  # Check file exists
  if (!file.exists(json_file)) {
    stop("JSON file not found: ", json_file)
  }
  
  # Load JSON
  ds <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)
  
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
    sim <- generate_from_stratum(stratum_stats, s$n, variables, quantile_probs)
    
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
  
  # Reorder columns - put ID and SEXISMALE first, then original variables
  first_cols <- c("ID", "SEXISMALE")
  other_cols <- setdiff(names(generated), first_cols)
  data.table::setcolorder(generated, c(first_cols, other_cols))
  
  # Return as data.frame
  return(as.data.frame(generated))
}
