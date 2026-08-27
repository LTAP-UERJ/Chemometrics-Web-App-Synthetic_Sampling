# ==============================================================================
# LTAP Chemometrics Web App (CWA) — Dependency Installer
# Module: Synthetic Sampling & Imbalance Balancing
# Laboratorio de Tecnologia e Analise Quimiometrica (LTAP/UERJ)
# ==============================================================================

install_synthetic_sampling_dependencies <- function(force = FALSE, check_only = FALSE, quiet = FALSE) {
  required_packages <- c(
    "shiny", "shinydashboard", "shinycssloaders", "shinyWidgets", "shinyBS", "shinyjs",
    "bslib", "DT", "plotly", "ggplot2", "dplyr", "tidyr", "tibble", "mdatools",
    "rrcov", "psych", "Rtsne", "vegan", "Hotelling", "MVN", "car", "caret",
    "ellipse", "imbalance", "MASS", "openxlsx", "philentropy", "purrr",
    "RColorBrewer", "readxl", "remotes", "reshape2", "reticulate", "rgl",
    "robustbase", "scutr", "skimr", "SmartEDA", "smotefamily", "UBL", "writexl",
    "data.table", "digest", "httr", "jsonlite"
  )
  
  if (!quiet) {
    cat("\n==============================================================================\n")
    cat("  LTAP Synthetic Sampling — Package Dependency Installer\n")
    cat(sprintf("  Checking %d required packages...\n", length(required_packages)))
    cat("==============================================================================\n\n")
  }
  
  # Ensure CRAN mirror is set
  if (is.null(getOption("repos")) || getOption("repos")["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org/"))
  }
  
  installed_already <- character(0)
  to_install <- character(0)
  
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE) && !force) {
      installed_already <- c(installed_already, pkg)
    } else {
      to_install <- c(to_install, pkg)
    }
  }
  
  if (check_only) {
    if (!quiet) {
      cat(sprintf("Status: %d installed, %d missing.\n", length(installed_already), length(to_install)))
      if (length(to_install) > 0) cat("Missing packages:", paste(to_install, collapse = ", "), "\n")
    }
    return(invisible(list(status = if (length(to_install) == 0) "OK" else "MISSING",
                          installed = installed_already, missing = to_install)))
  }
  
  if (length(to_install) > 0) {
    if (!quiet) cat("Installing missing packages:", paste(to_install, collapse = ", "), "\n\n")
    install.packages(to_install, dependencies = TRUE)
  } else {
    if (!quiet) cat("All required packages are already installed!\n")
  }
  
  # Verification
  failed <- character(0)
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      failed <- c(failed, pkg)
    }
  }
  
  if (!quiet) {
    cat("\n------------------------------------------------------------------------------\n")
    if (length(failed) == 0) {
      cat("[SUCCESS] All packages for Synthetic Sampling are successfully installed and functional!\n")
    } else {
      cat("[WARNING] The following packages could not be installed/loaded:", paste(failed, collapse = ", "), "\n")
    }
    cat("------------------------------------------------------------------------------\n\n")
  }
  
  invisible(list(success = length(failed) == 0, failed = failed))
}

# Generic alias
install_dependencies <- install_synthetic_sampling_dependencies

# Auto-run if executed non-interactively
if (!interactive()) {
  install_synthetic_sampling_dependencies()
}
