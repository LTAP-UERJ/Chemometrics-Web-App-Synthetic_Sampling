# ==============================================================================
# Chemometrics Web App: Synthetic Sampling (CWA: SS)
# Package & Environment Dependency Installation Script
# Laboratory of Process Analytical Technology (LTAP/UERJ)
# Repository: https://github.com/LTAP-UERJ/Chemometrics-Web-App-Synthetic_Sampling
# ==============================================================================

#' Install and verify all required R and Python dependencies for CWA: SS
#'
#' @param install_python Logical; if TRUE, configures reticulate and installs required Python packages.
#' @param update_packages Logical; if TRUE, updates existing packages to latest CRAN versions.
#' @param verbose Logical; if TRUE, prints detailed diagnostic messages.
#' @return A named logical vector indicating installation status of each package.
#' @export
install_cwa_synthetic_sampling_packages <- function(install_python = TRUE, 
                                                   update_packages = FALSE, 
                                                   verbose = TRUE) {
  
  cat("\n=======================================================================\n")
  cat("   Chemometrics Web App: Synthetic Sampling (CWA: SS) Dependency Setup  \n")
  cat("=======================================================================\n\n")
  
  # Ensure CRAN mirror is set
  if (is.null(getOption("repos")) || getOption("repos")["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  
  # --------------------------------------------------------------------------
  # 1. Comprehensive list of required CRAN packages for CWA: SS runtime
  # --------------------------------------------------------------------------
  cran_packages <- c(
    # Core Shiny & Dashboard Infrastructure
    "shiny", "shinydashboard", "shinyWidgets", "shinyBS", "shinyjs", 
    "shinycssloaders", "DT", "plotly",
    
    # Data Wrangling & Visualization
    "ggplot2", "RColorBrewer", "reshape2", "tibble", "tidyr", "dplyr", 
    "purrr", "readxl", "writexl", "openxlsx", "psych", "skimr",
    
    # Chemometrics, Statistics & Multivariate Analysis
    "mdatools", "vegan", "Hotelling", "MASS", "car", "caret", "rgl", 
    "robustbase", "rrcov", "philentropy", "MVN", "ellipse", "Rtsne",
    
    # Synthetic Sampling & Resampling Algorithms
    "smotefamily", "imbalance", "UBL", "scutr",
    
    # Interoperability & Deployment
    "reticulate", "remotes", "RInno"
  )
  
  cat("[1/3] Checking CRAN R package dependencies...\n")
  installed <- rownames(installed.packages())
  missing_pkgs <- setdiff(cran_packages, installed)
  
  if (length(missing_pkgs) > 0) {
    cat(sprintf(" -> Installing %d missing CRAN packages: %s\n", 
                length(missing_pkgs), paste(missing_pkgs, collapse = ", ")))
    install.packages(missing_pkgs, dependencies = TRUE)
  } else {
    cat(" -> All core CRAN packages are already installed.\n")
  }
  
  if (update_packages) {
    cat(" -> Updating existing packages...\n")
    update.packages(ask = FALSE, checkBuilt = TRUE)
  }
  
  # --------------------------------------------------------------------------
  # 2. Package Verification
  # --------------------------------------------------------------------------
  cat("\n[2/3] Verifying package loading...\n")
  verification_status <- sapply(cran_packages, function(pkg) {
    available <- requireNamespace(pkg, quietly = TRUE)
    status_sym <- if (available) "[OK]" else "[FAILED]"
    if (verbose) cat(sprintf("   %-20s %s\n", pkg, status_sym))
    return(available)
  })
  
  # --------------------------------------------------------------------------
  # 3. Python Environment & Machine Learning Libraries (via reticulate)
  # --------------------------------------------------------------------------
  if (install_python && verification_status["reticulate"]) {
    cat("\n[3/3] Configuring Python environment for hybrid ML backend...\n")
    tryCatch({
      suppressPackageStartupMessages(library(reticulate))
      
      # Check if a Python environment is accessible
      py_avail <- py_available(initialize = TRUE)
      if (!py_avail) {
        cat(" -> Initializing default Python virtual environment (r-reticulate)...\n")
        virtualenv_create("r-reticulate")
        use_virtualenv("r-reticulate", required = FALSE)
      }
      
      # Python module import names mapped to their respective pip package names
      python_modules <- list(
        numpy            = "numpy",
        pandas           = "pandas",
        scipy            = "scipy",
        sklearn          = "scikit-learn",
        imblearn         = "imbalanced-learn"
      )
      
      cat(sprintf(" -> Verifying Python modules (%s)...\n", 
                  paste(unname(unlist(python_modules)), collapse = ", ")))
      
      for (mod_import in names(python_modules)) {
        pip_pkg <- python_modules[[mod_import]]
        if (!py_module_available(mod_import)) {
          cat(sprintf("    Installing Python package '%s' (module: %s)...\n", pip_pkg, mod_import))
          py_install(pip_pkg, pip = TRUE)
        } else {
          cat(sprintf("    Python package '%s' (module: %s) is ready [OK]\n", pip_pkg, mod_import))
        }
      }
    }, error = function(e) {
      warning(paste("Python environment configuration note:", e$message, 
                    "\nNote: Native R resampling algorithms (e.g. smotefamily) will remain fully functional."))
    })
  }
  
  cat("\n=======================================================================\n")
  all_ok <- all(verification_status)
  if (all_ok) {
    cat(" SUCCESS: All CWA: SS dependencies are successfully installed and ready! \n")
    cat(" You can launch the application by running: shiny::runApp()\n")
  } else {
    failed_pkgs <- names(verification_status)[!verification_status]
    cat(sprintf(" WARNING: The following packages could not be loaded: %s\n", 
                paste(failed_pkgs, collapse = ", ")))
  }
  cat("=======================================================================\n\n")
  
  invisible(verification_status)
}

# Execute automatic dependency installation immediately upon running/sourcing
install_cwa_synthetic_sampling_packages()
