# ============================================================================
# Data Path Management
# ============================================================================

# Package environment for storing data path
.copulapop_env <- new.env(parent = emptyenv())
.copulapop_env$data_path <- NULL

#' Set the Path to Copula Data Files
#'
#' Sets the directory path where the copulapop JSON data files are stored.
#' This should be called before using \code{generate_population()} if data
#' files are not in the package's default location.
#'
#' @param path Character string specifying the path to the data directory
#'   containing the JSON files (e.g., "RUMC.json", "NHANES.json")
#'
#' @return Invisibly returns the previous path setting
#'
#' @examples
#' \dontrun{
#' set_data_path("/path/to/copulapop_data/")
#' pop <- generate_population("RUMC", n = 1000)
#' }
#'
#' @export
set_data_path <- function(path) {
  old_path <- .copulapop_env$data_path
  
  if (!dir.exists(path)) {
    warning("Directory does not exist: ", path)
  }
  
  # Ensure trailing slash
  if (!grepl("/$", path)) {
    path <- paste0(path, "/")
  }
  
  .copulapop_env$data_path <- path
  message("Data path set to: ", path)
  
  invisible(old_path)
}

#' Get the Current Data Path
#'
#' Returns the currently configured path to copulapop data files.
#'
#' @return Character string with the data path, or NULL if not set
#'
#' @examples
#' get_data_path()
#'
#' @export
get_data_path <- function() {
  path <- .copulapop_env$data_path
  
  if (is.null(path)) {
    # Try package default location
    pkg_path <- system.file("extdata", package = "copulapop")
    if (nzchar(pkg_path) && dir.exists(pkg_path)) {
      path <- paste0(pkg_path, "/")
    }
  }
  
  return(path)
}

#' List Available Datasets
#'
#' Lists all available datasets in the current data path.
#'
#' @return Character vector of dataset names (without .json extension)
#'
#' @examples
#' \dontrun{
#' list_datasets()
#' }
#'
#' @export
list_datasets <- function() {
  path <- get_data_path()
  
  if (is.null(path) || !dir.exists(path)) {
    stop("Data path not set or does not exist. Use set_data_path() first.")
  }
  
  json_files <- list.files(path, pattern = "\\.json$", full.names = FALSE)
  
  # Exclude summary_statistics.json
  json_files <- json_files[json_files != "summary_statistics.json"]
  
  # Remove .json extension
  datasets <- gsub("\\.json$", "", json_files)
  
  return(datasets)
}

#' Get Dataset Information
#'
#' Retrieves metadata and summary information about a specific dataset.
#'
#' @param dataset Character string specifying the dataset name
#'
#' @return A list containing dataset metadata including:
#'   \itemize{
#'     \item name: Full dataset name
#'     \item description: Dataset description
#'     \item country: Country of origin
#'     \item population: Population type
#'     \item n_total: Total sample size
#'     \item age_range: Age range in years
#'     \item variables: Available variables
#'     \item has_creatinine: Whether creatinine is available
#'   }
#'
#' @examples
#' \dontrun{
#' get_dataset_info("RUMC")
#' }
#'
#' @export
get_dataset_info <- function(dataset) {
  path <- get_data_path()
  
  if (is.null(path)) {
    stop("Data path not set. Use set_data_path() first.")
  }
  
  json_file <- paste0(path, dataset, ".json")
  
  if (!file.exists(json_file)) {
    stop("Dataset not found: ", dataset, 
         "\nAvailable datasets: ", paste(list_datasets(), collapse = ", "))
  }
  
  data <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)
  
  info <- list(
    name = data$info$name,
    description = data$info$description,
    country = data$info$country,
    population = data$info$population,
    source = data$info$source,
    source_url = data$info$source_url,
    n_total = data$overall$n_total,
    n_pediatric = data$overall$n_pediatric,
    n_adult = data$overall$n_adult,
    age_range = c(data$overall$age_min, data$overall$age_max),
    prop_male = data$overall$prop_male,
    variables = unlist(data$variables),
    has_creatinine = isTRUE(data$info$has_creatinine),
    has_albumin = isTRUE(data$info$has_albumin),
    adult_only = isTRUE(data$info$adult_only)
  )
  
  # Add source_urls if present (for HSE)
  if (!is.null(data$info$source_urls)) {
    info$source_urls <- data$info$source_urls
  }
  
  class(info) <- c("copulapop_info", "list")
  return(info)
}

#' @export
print.copulapop_info <- function(x, ...) {
  cat("Dataset:", x$name, "\n")
  cat(strrep("-", 50), "\n")
  cat("Description:", x$description, "\n")
  cat("Country:", x$country, "\n")
  cat("Population:", x$population, "\n")
  cat("Sample size:", format(x$n_total, big.mark = ","), "\n")
  cat("  Pediatric (<18):", format(x$n_pediatric, big.mark = ","), "\n")
  cat("  Adult (>=18):", format(x$n_adult, big.mark = ","), "\n")
  cat("Age range:", x$age_range[1], "-", x$age_range[2], "years\n")
  cat("Male proportion:", round(x$prop_male * 100, 1), "%\n")
  cat("Variables:", paste(x$variables, collapse = ", "), "\n")
  cat("Creatinine:", if(x$has_creatinine) "Available" else "Not available", "\n")
  if (x$has_albumin) cat("Albumin: Available\n")
  if (x$adult_only) cat("Note: Adult population only\n")
  cat("Source:", x$source, "\n")
  invisible(x)
}
