# Chemometrics Web App — Synthetic Sampling (CWA: SS)

![Status](https://img.shields.io/badge/Status-Active-green)
![Release](https://img.shields.io/badge/Release-V1%20%26%20V2-orange)
![License](https://img.shields.io/badge/License-Proprietary%20%2F%20INPI%20Registered-red.svg)
![R](https://img.shields.io/badge/R%20Version-4.3.0%2B-blue.svg)
![Python](https://img.shields.io/badge/Python-v3.10%2B-yellow.svg)
![Platform](https://img.shields.io/badge/Deployment-ShinyApps%20%7C%20RStudio%20%7C%20Desktop%20EXE-blueviolet)

Developed by the **[Process Analytical Technology Laboratory (LTAP-UERJ)](https://www.ltapuerj.com.br/)**, the **Chemometrics Web App: Synthetic Sampling (CWA: SS)** is an advanced computational platform designed to resolve severe class-imbalance problems in high-dimensional analytical, spectroscopic, chromatographic, and machine learning datasets. The system integrates classical resampling, modern border-focused heuristics, clustering-based undersampling, hybrid algorithms, multivariate chemometric diagnostics, and automated reporting.

---

## 🔗 Quick Links

* 🌐 **Online Web Application (Shinyapps.io):**
  * **[Launch Version 02 (Full Production & QC Diagnostics)](https://ltap.shinyapps.io/Synthetic_Sampling/)**
  * **[Launch Version 01 (Lightweight Core Resampling)](https://ltap.shinyapps.io/Synthetic_Sampling_V1/)**
* 🖥️ **Desktop Executable Download (.exe):** [LTAP CWA Standalone Executables (Google Drive)](https://drive.google.com/drive/folders/1l8dB4BGKVjqPMrvA5ZCLvafRNtTOCFew?usp=drive_link)
* 📦 **Automated Dependency Setup Scripts:**
  * [`install_dependencies.R`](https://github.com/LTAP-UERJ/Chemometrics-Web-App-Synthetic_Sampling/blob/main/install_dependencies.R)
  * [`install_packages.R`](https://github.com/LTAP-UERJ/Chemometrics-Web-App-Synthetic_Sampling/blob/main/install_packages.R)
* 🏛️ **Official Portal & Software Registration:**
  * [LTAP-UERJ Official Website](https://www.ltapuerj.com.br/)
  * [LTAP-UERJ CWA — Registros de Software (INPI)](https://sites.google.com/view/ltap-uerj/cwa)
* 📬 **Support & Academic Inquiries:** [licarion@gmail.com](mailto:licarion@gmail.com) | [ltapuerj@gmail.com](mailto:ltapuerj@gmail.com)

---

## 👥 Developers & Authors

This module was conceived and engineered by the chemometrics and data science team at **LTAP-UERJ**:

| Author | Affiliation | Profile / Contact |
| :--- | :--- | :--- |
| **[Julio Cesar Siqueira](http://lattes.cnpq.br/1968914053746315)** | LTAP — UERJ | [Lattes Profile](http://lattes.cnpq.br/1968914053746315) |
| **[José Licarion Pinto Segundo Neto](http://lattes.cnpq.br/5267552018296169)** | LTAP — UERJ | [Lattes Profile](http://lattes.cnpq.br/5267552018296169) |
| **[Aderval Severino Luna](http://lattes.cnpq.br/0294676847895948)** | UERJ | [Lattes Profile](http://lattes.cnpq.br/0294676847895948) |
| **[Paulo Henrique Couto Simões](http://lattes.cnpq.br/5098915046998337)** | LTAP — UERJ | [Lattes Profile](http://lattes.cnpq.br/5098915046998337) |

---

## 🆕 Version Comparison & Change Log

The CWA Synthetic Sampling ecosystem is maintained in two distinct releases tailored to different research and operational requirements:

| Dimension | Version 01 (V1 — Lightweight Baseline) | Version 02 (V2 — Full Production Platform) |
| :--- | :--- | :--- |
| **Primary Focus** | Fast, lightweight resampling & core data exploration | Advanced hybrid resampling, QC diagnostics & reporting |
| **Execution Architecture** | **100% Self-contained single-file (`app.R`)** or multi-file | Modular multi-layer platform with automated report engine |
| **Oversampling** | SMOTE, SMOTE-NC, Borderline-SMOTE, SVM-SMOTE, ADASYN, Random Over | Full V1 suite + SMOTE-IPF + SPIDER |
| **Undersampling** | Tomek Links, NearMiss (v1-v3), ENN, OSS, Random Under, SBC (scutr) | Full V1 suite + multi-metric distance clustering |
| **Hybrid Resampling** | SMOTE-Tomek Links, SMOTE-ENN | SMOTE-TL, SMOTE-ENN, SMOTE-IPF, SPIDER |
| **Quality Control & Diagnostics** | Standard PCA, Robust PCA, 2D/3D Score/Loading plots, t-SNE | Full V1 QC + PERMANOVA (`adonis2`), Hotelling's $T^2$, KS tests, Jensen-Shannon |
| **Sample Datasets** | In-line QSAR Biodegradation (779 samples) & Prestige (102 samples) | In-line demo datasets + external session import (`.RData`) |
| **Report Generation** | Export tables & charts directly (CSV, Excel, PNG) | Multi-language automated report builder (PDF, HTML, Word in 7 languages) |
| **Target Users** | Individual researchers, students, quick RStudio single-file runs | Research labs, validation pipelines, full regulatory compliance |

---

## 💻 Access & Execution Guide

You can access and run the Synthetic Sampling platform through **three different modalities**:

```
                              ┌──────────────────────────────────────────────┐
                              │    CWA Synthetic Sampling Access Modes       │
                              └──────────────────────┬───────────────────────┘
                                                     │
         ┌───────────────────────────────────────────┼──────────────────────────────────────────┐
         │                                           │                                          │
         ▼                                           ▼                                          ▼
┌─────────────────────────┐             ┌─────────────────────────┐            ┌─────────────────────────┐
│  1. RStudio / Local R   │             │   2. Desktop EXE Bundle │            │  3. ShinyApps.io Cloud  │
│  • Full code control    │             │   • 100% Offline (.exe) │            │  • Instant Web Access   │
│  • V1 Single-File App   │             │   • Embedded R & Python │            │  • Zero Installation    │
│  • V2 Full Production   │             │   • One-click launch    │            │  • Any Browser/Device   │
└─────────────────────────┘             └─────────────────────────┘            └─────────────────────────┘
```

---

### Option 1: Running in RStudio (Source Code — V1 & V2)

Running directly in RStudio provides full flexibility for researchers who wish to inspect data reactively, customize parameters, or run the application locally on their workstations.

#### Step 1: Clone or Download the Repository
Clone the repository using Git or download the ZIP archive:
```bash
git clone https://github.com/LTAP-UERJ/Chemometrics-Web-App-Synthetic_Sampling.git
cd Chemometrics-Web-App-Synthetic_Sampling
```

#### Step 2: Automated Environment & Dependency Installation
We provide two equivalent convenience scripts—[`install_dependencies.R`](https://github.com/LTAP-UERJ/Chemometrics-Web-App-Synthetic_Sampling/blob/main/install_dependencies.R) and [`install_packages.R`](https://github.com/LTAP-UERJ/Chemometrics-Web-App-Synthetic_Sampling/blob/main/install_packages.R)—that automate the entire setup.

Open RStudio and run **either** command in the R Console:

```r
# Execute local installation script
source("install_dependencies.R")

# Alternatively, the alias script can be used:
source("install_packages.R")
```

> [!TIP]
> **One-Liner Remote Installation (No Download Required):**
> You can also run the installer directly from GitHub without cloning the repo first:
> ```r
> source("https://raw.githubusercontent.com/LTAP-UERJ/Chemometrics-Web-App-Synthetic_Sampling/main/install_dependencies.R")
> ```

**What this script configures automatically:**
1. **CRAN Packages:** Identifies and installs all missing R dependencies (`shiny`, `shinydashboard`, `shinyWidgets`, `shinyBS`, `shinyjs`, `shinycssloaders`, `DT`, `plotly`, `ggplot2`, `readxl`, `writexl`, `openxlsx`, `mdatools`, `vegan`, `Hotelling`, `MASS`, `car`, `caret`, `rgl`, `robustbase`, `rrcov`, `philentropy`, `MVN`, `ellipse`, `Rtsne`, `smotefamily`, `imbalance`, `UBL`, `scutr`, `reticulate`, etc.).
2. **Verification Diagnostics:** Validates that each required package loads cleanly and reports a verification table (`[OK]` / `[FAILED]`).
3. **Python & Hybrid ML Engine:** Automatically detects or provisions an isolated `r-reticulate` virtual environment and installs the required Python packages (`scikit-learn`, `imbalanced-learn`, `numpy`, `pandas`, `scipy`).
4. **Fallback Safety:** If Python is not installed on the system, the installer logs a diagnostic note; **all native R resampling algorithms (e.g., SMOTE, SMOTE-NC, ADASYN, Random Upsampling, SBC) will remain 100% operational**.

#### Step 3: Launching the Application
Once dependencies are verified, you can launch the app using either of the following approaches:

* **Method A (Interactive GUI — Recommended):**
  1. Open `app.R` in RStudio.
  2. Notice the green **"Run App"** button at the top-right of the source editor pane.
  3. Click **"Run App"** (or press `Ctrl + Shift + Enter`).
  
* **Method B (Console Command):**
  ```r
  # From within the project directory:
  shiny::runApp()
  
  # Or pointing directly to the file path:
  shiny::runApp("path/to/app.R")
  ```

---

#### 🌟 Special Standalone Mode: Version 01 (`app.R` Single-File)
For users who require zero directory overhead, **Version 01** can run as a **completely self-contained single file**:
* **Zero Folder Dependencies:** No external `www/`, `report_template.Rmd`, or database files are required.
* **Inline Datasets:** Both the QSAR Biodegradation (779 samples $\times$ 11 features) and Prestige (102 samples $\times$ 5 features) demo datasets are compiled directly into the code.
* **Built-in Auto-Installer:** The top of the V1 `app.R` script contains an automated startup check that installs missing CRAN packages on first execution.
* **How to use:** Simply download or copy `app.R` into any folder (e.g., your `Downloads` directory), open it in RStudio, and hit **Run App**!

---

### Option 2: Desktop Standalone Executable (.exe — Offline Windows)

For production laboratories, industrial quality control units, or computers restricted from internet access or external package compilation, we provide pre-compiled Windows installers.

* **Key Advantages:**
  * **100% Offline:** Operates entirely without an internet connection.
  * **Embedded Runtime:** Ships with its own isolated, hardened R-Portable and Python-Portable environments—no installation of R, RStudio, or Python is needed on the client computer.
  * **System Integrity:** Does not interfere with existing system PATH variables or local Python/R installations.

* **Installation Steps:**
  1. Navigate to the **[LTAP CWA Executables Folder on Google Drive](https://drive.google.com/drive/folders/1l8dB4BGKVjqPMrvA5ZCLvafRNtTOCFew?usp=drive_link)**.
  2. Download the installer:
     * `setup_Synthetic_Sampling.exe` (for Version 02)
     * `setup_Synthetic_Sampling_V1.exe` (for Version 01)
  3. Run the installer wizard and choose the installation destination (default: `C:\LTAP_Modules\`).
  4. Launch the application directly from the **Desktop Shortcut** or **Windows Start Menu**.

---

### Option 3: Online Web Application (Shinyapps.io — Cloud Deployment)

For fast evaluations, client demonstrations, or cross-platform use (including macOS, Linux, ChromeOS, iPad, and Android tablets), the application is hosted on high-availability cloud servers.

* **Version 02 (Production Platform):**
  🔗 **[https://ltap.shinyapps.io/Synthetic_Sampling/](https://ltap.shinyapps.io/Synthetic_Sampling/)**
* **Version 01 (Core Resampling Suite):**
  🔗 **[https://ltap.shinyapps.io/Synthetic_Sampling_V1/](https://ltap.shinyapps.io/Synthetic_Sampling_V1/)**

* **Highlights:**
  * Zero local installation or hardware requirements.
  * Instant access from any modern browser (Chrome, Firefox, Safari, Edge).
  * Data confidentiality: Uploaded sessions reside solely in volatile container memory and are erased upon session close.

---

## 🚀 Key Methodological Features

### 📥 1. Data Import & Interactive Preprocessing
* Accepts `.xlsx`, `.xls`, `.csv`, `.txt` formats with customizable delimiters, decimal separators, and header rows.
* Supports transposed matrix inputs (samples structured in columns).
* Normalization and scaling: Auto-scaling (UV), Mean Centering, Median Centering, Range Scaling (0–1), and Power transformations.
* Dynamic sample/variable removal and zero-variance feature filtering.

### ⬆️ 2. Oversampling (Upsampling) Algorithms
| Algorithm | Mechanism | Practical Chemometrics Application |
| :--- | :--- | :--- |
| **SMOTE** | Generates synthetic instances along the line segments connecting $k$-nearest minority neighbors. | Standard spectral / compositional imbalance balancing. |
| **SMOTE-NC** | Extends SMOTE to handle mixed continuous and nominal/categorical attributes. | Combined chemical parameters (spectral bands + batch / origin categories). |
| **Borderline-SMOTE** | Identifies minority instances near the decision boundary (DANGER zone) and restricts synthesis to them. | Differentiating closely overlapping chemical classes or adulterant boundaries. |
| **SVM-SMOTE** | Uses Support Vector Machine support vectors to model class boundaries and generate border samples. | Non-linear spectroscopic boundaries and sparse high-dimensional datasets. |
| **ADASYN** | Adaptively calculates density distribution and synthesizes more instances for harder-to-learn samples. | Highly heterogeneous sample clusters with non-uniform dispersion. |
| **Random Over** | Duplicates randomly sampled minority instances with optional jittering. | Baseline comparison and ultra-small sample subsets. |

### ⬇️ 3. Undersampling (Downsampling) Algorithms
| Algorithm | Mechanism | Practical Chemometrics Application |
| :--- | :--- | :--- |
| **Tomek Links** | Detects and removes majority instances that form mutually closest opposite-class pairs. | Boundary sharpening and elimination of mislabeled or borderline samples. |
| **NearMiss (v1–v3)** | Selects majority samples based on average or minimum distance to closest minority instances. | Controlled reduction of dominant background classes. |
| **ENN** | Removes instances whose classification disagrees with the majority vote of their $k$-nearest neighbors. | Noise reduction and outlier cleaning in raw spectral libraries. |
| **OSS** | Applies Tomek Links followed by Condensed Nearest Neighbor (CNN) filtering. | Aggressive dataset condensation while preserving critical decision edges. |
| **SBC (scutr)** | Clusters the majority class via $k$-means and samples cluster representatives. | Preserving multivariate cluster topology while reducing sample volume. |
| **Random Under** | Randomly eliminates majority instances to achieve target class balance. | Rapid prototype balancing and computational workload reduction. |

### 🔀 4. Hybrid Resampling Pipelines
* **SMOTE-Tomek Links (SMOTE-TL):** Overcomes oversampling blur by synthesizing with SMOTE and subsequently stripping ambiguous border points via Tomek Links.
* **SMOTE-ENN:** Pairs SMOTE oversampling with Edited Nearest Neighbours to aggressively prune noisy synthetic instances.
* **SMOTE-IPF (V2):** Iterative Partitioning Filter that trains an ensemble to identify and eliminate mislabeled synthetic samples.
* **SPIDER (V2):** Classifies instances into safe, borderline, and noisy subsets before executing combined resampling.

### 📊 5. Diagnostic & Quality Control (QC) Suite
* **Comparative PCA:** Interactive Score plots, Loading plots, BiPlots, and Explained Variance curves comparing Original vs. Resampled spaces.
* **Robust PCA (ROBPCA):** Score Distance (SD) vs. Orthogonal Distance (OD) diagnostic plots using Minimum Covariance Determinant (MCD) to verify outlier integrity.
* **t-SNE Projections:** 2D and 3D non-linear manifold embeddings to detect artificial sub-clustering or manifold collapse.
* **Multivariate Hypothesis Tests:**
  * **PERMANOVA (`adonis2`):** Non-parametric multivariate analysis of variance evaluating whether synthetic generation altered class centroids ($p > 0.05$ desired).
  * **Hotelling's $T^2$ Centroid Preservation:** Verifies that synthetic centroids do not diverge from empirical sample means.
  * **Kolmogorov-Smirnov & Jensen-Shannon Tests:** Univariate and distribution-wide divergence metrics.

---

## 🛠️ Technical Stack & Dependencies

```plaintext
┌────────────────────────────────────────────────────────────────────────────┐
│                    CWA Synthetic Sampling Architecture                     │
├────────────────────────────────────────────────────────────────────────────┤
│ User Interface      │ shiny, shinydashboard, shinyWidgets, shinyBS, DT     │
│ Visualizations      │ plotly, ggplot2, RColorBrewer, rgl                   │
│ R Chemometrics / QC │ mdatools, rrcov, vegan, Hotelling, MVN, car, Rtsne   │
│ R Resampling        │ smotefamily, imbalance, UBL, scutr, caret            │
│ Python Hybrid ML    │ reticulate ──► scikit-learn, imbalanced-learn, scipy │
│ Data I/O & Reports  │ readxl, writexl, openxlsx, tibble, tidyr, dplyr      │
└────────────────────────────────────────────────────────────────────────────┘
```

---

## ⚠️ Methodological Guidelines

> [!IMPORTANT]
> **Critical Methodological Principles for Synthetic Resampling in Chemometrics:**
> 1. **Strict Test Set Isolation:** Synthetic samples must **ONLY** be generated within the **training set** ($X_{\text{cal}}$). The test or external validation set ($X_{\text{val}}$) must remain 100% genuine, experimentally measured samples. Never resample before splitting data into calibration and test sets.
> 2. **Avoid Hyper-Oversampling:** Increasing the sample count of a minority class beyond $3\times$ its original size can artificially compress covariance matrices and lead to overfitted models with inflated validation scores.
> 3. **Quality Gate Verification:** Always review the **Diagnostic & QC** tab after resampling. Ensure that the generated samples populate the convex hull of the original minority class and do not introduce severe leverage points in Robust PCA.

---

## 📜 License & Intellectual Property Protection

> [!CAUTION]
> **Proprietary Software — All Rights Reserved (INPI Registered)**
> 
> This software system, its graphical interface, algorithmic workflows, compiled binaries, and associated documentation are proprietary assets protected under Brazilian Software Law (Law No. 9.609/98) and Industrial Property Law (Law No. 9.279/96), officially registered at the **National Institute of Industrial Property (INPI)**.

### **Terms of Use:**
1. **Academic & Research Attribution:** Any academic paper, dissertation, presentation, or technical report utilizing results, resampled data, or visualizations produced by this platform **must cite**:
   > *Siqueira, J. C.; Pinto Segundo Neto, J. L.; Luna, A. S.; Simões, P. H. C. Chemometrics Web App: Synthetic Sampling (CWA: SS). Process Analytical Technology Laboratory (LTAP), Rio de Janeiro State University (UERJ), 2026.*
2. **Prohibition of Commercial Exploitation & Redistribution:** Redistribution of compiled binaries, mirroring of source repositories, reverse engineering, decompilation, and commercial exploitation are strictly prohibited without prior formal authorization from LTAP-UERJ.
3. **Warranty Disclaimer:** The software is provided "as is" in good faith for academic and research advancement. The authors and LTAP-UERJ assume no liability for experimental or analytical outcomes derived from its use.

---

## 🏛️ Institutional Support & Acknowledgments

The development of the CWA platform is supported by:
* **[UERJ (Universidade do Estado do Rio de Janeiro)](https://www.uerj.br/)**
* **[FAPERJ (Fundação de Amparo à Pesquisa do Estado do Rio de Janeiro)](https://www.faperj.br/)** — JCNE and CNE Research Grants
* **[CNPq (Conselho Nacional de Desenvolvimento Científico e Tecnológico)](https://www.gov.br/cnpq/pt-br)** — Universal Grant
* **[CAPES (Coordenação de Aperfeiçoamento de Pessoal de Nível Superior)](https://www.gov.br/capes/pt-br)** — Finance Code 001

---

<p align="center">
  <b>Process Analytical Technology Laboratory (LTAP-UERJ)</b><br>
  Pavilhão Haroldo Lisboa da Cunha, Maracanã, Rio de Janeiro - RJ, Brazil<br>
  <a href="https://www.ltapuerj.com.br/">www.ltapuerj.com.br</a>
</p>
