# Chemometrics Web App — Synthetic Sampling (CWA: SS)

![Status](https://img.shields.io/badge/Status-Active-green)
![Version](https://img.shields.io/badge/Release-v1.0-orange)
![License](https://img.shields.io/badge/License-Proprietary%20%2F%20INPI%20Registered-red.svg)
![R](https://img.shields.io/badge/R%20Version-4.3.0%2B-blue.svg)
![Python](https://img.shields.io/badge/Python-v3.10%2B-yellow.svg)
![Platform](https://img.shields.io/badge/Deployment-ShinyApps%20%7C%20RStudio%20%7C%20Desktop%20EXE-blueviolet)

Developed by the **[Process Analytical Technology Laboratory (LTAP-UERJ)](https://www.ltapuerj.com.br/)**, the **Chemometrics Web App: Synthetic Sampling (CWA: SS)** is a specialized computational platform designed to address and solve severe class-imbalance problems in chemometrics, spectroscopy, chromatography, and analytical machine learning. The system provides an end-to-end pipeline covering data import, spectral/numerical preprocessing, multiple oversampling, undersampling, and hybrid algorithms, followed by chemometric multivariate diagnostic verification and export.

---

## 🔗 Quick Links

* 🌐 **Online Web Application (Shinyapps.io):** [Access CWA: Synthetic Sampling](https://ltap.shinyapps.io/Synthetic_Sampling/)
* 🖥️ **Desktop Executable Download (.exe):** [Download Windows Standalone Installer (Google Drive)](https://drive.google.com/drive/folders/1oeVCeKzjskS-HWGx7GWG4lyT3QvViLgF?usp=drive_link)
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

## ✨ Features Available for Use in this Version

The platform integrates a complete, standalone chemometrics resampling and diagnostic workflow:

```
┌──────────────────────────────────────────────────────────────────────────────────────────────────┐
│                            CWA: SYNTHETIC SAMPLING PIPELINE                                      │
├───────────────────┬───────────────────┬───────────────────┬───────────────────┬──────────────────┤
│  1. Data Import   │ 2. Pre-processing │ 3. Resampling     │ 4. Multivariate   │ 5. Export &      │
│     & Demo Data   │    & Scaling      │    Algorithms     │    Diagnostics    │    Reporting     │
├───────────────────┼───────────────────┼───────────────────┼───────────────────┼──────────────────┤
│ • Excel/CSV/TXT   │ • Auto-scaling    │ • Oversampling    │ • PCA (2D & 3D)   │ • Balanced Data  │
│ • Transposition   │ • Mean/Median Ctr │ • Undersampling   │ • Robust PCA MCD  │   (CSV, Excel)   │
│ • In-line Demos:  │ • Range Scaling   │ • Hybrid Methods  │ • t-SNE Manifold  │ • Synthetic Only │
│   QSAR & Prestige │ • Box-Cox / Y-J   │ • SBC Clustering  │ • Hotelling's T²  │ • Models & Plots │
└───────────────────┴───────────────────┴───────────────────┴───────────────────┴──────────────────┘
```

### 📥 1. Flexible Data Import & Built-in Demo Datasets
* **Broad File Support:** Reads Microsoft Excel (`.xlsx`, `.xls`), CSV, TXT, and R workspace (`.RData`) files.
* **Transposition Flexibility:** Automatically transposes matrices when samples are arranged in columns rather than rows.
* **Interactive Class Definition:** Automatically detects or allows user selection of categorical target class columns.
* **Zero-File In-App Demos:** Instantly loadable without external files:
  * **QSAR Biodegradation Demo:** 779 chemical samples $\times$ 11 molecular descriptors across 2 classes (Ready vs. Not ready biodegradable).
  * **Prestige Demo:** 102 samples $\times$ 5 socioeconomic variables across 4 classes (`bc`, `NB`, `prof`, `wc`).

### ⚙️ 2. Spectral & Numerical Pre-processing
* **Centering & Scaling:** Mean Centering, Median Centering, Auto-scaling / Unit Variance (UV), Range Scaling (0 to 1).
* **Power Transformations:** Box-Cox and Yeo-Johnson transformations to induce normality and stabilize variance.
* **Missing Value Exploration:** Interactive diagnostic position mapping for missing data.

### ⬆️ 3. Oversampling (Upsampling) Methods
| Algorithm | Mechanism | Practical Chemometrics Application |
| :--- | :--- | :--- |
| **SMOTE** | Generates synthetic instances by linear interpolation along minority $k$-nearest neighbor segments. | Fundamental spectral / chromatographic sample balancing. |
| **SMOTE-NC** | Extends SMOTE to handle mixed continuous numerical and categorical attributes. | Combined chemical datasets (continuous spectra + discrete origin/batch labels). |
| **Borderline-SMOTE** | Restricts synthetic sample generation strictly to borderline minority samples (DANGER zone). | Sharp classification boundaries with overlapping chemical classes. |
| **SVM-SMOTE** | Uses Support Vector Machine boundary support vectors to guide synthesis. | High-dimensional, sparse spectroscopic feature spaces. |
| **ADASYN** | Adaptively synthesizes more instances for harder-to-learn minority samples according to local density. | Heterogeneous analytical clusters with non-uniform dispersion. |
| **Random Over** | Replicates existing minority samples with replacement. | Baseline comparison and ultra-small sample subsets. |

### ⬇️ 4. Undersampling (Downsampling) Methods
| Algorithm | Mechanism | Practical Chemometrics Application |
| :--- | :--- | :--- |
| **Tomek Links** | Identifies and purges majority instances that form mutually closest opposite-class pairs. | Cleaning boundary ambiguity and removing mislabeled borderline samples. |
| **NearMiss (v1–v3)** | Selects majority samples based on distance to closest minority instances (average or minimum). | Controlled, distance-aware reduction of dominant background classes. |
| **ENN** | Removes instances whose label disagrees with the majority vote of their $k$-nearest neighbors. | Noise filtering and outlier trimming in large spectral libraries. |
| **OSS** | Couples Tomek Links boundary cleaning with Condensed Nearest Neighbor (CNN) reduction. | Aggressive dataset condensation while preserving critical decision edges. |
| **SBC (scutr)** | Partitions majority class via $k$-means clustering and replaces dense regions with representatives. | Preserving multivariate cluster topology while balancing sample sizes. |
| **Random Under** | Randomly removes majority instances down to the desired balance ratio. | Quick computational reduction for preliminary exploratory screening. |

### 🔀 5. Hybrid Resampling Pipelines
* **SMOTE-Tomek Links (SMOTE-TL):** Overcomes oversampling blur by synthesizing with SMOTE and subsequently stripping ambiguous border points via Tomek Links.
* **SMOTE-ENN:** Pairs SMOTE oversampling with Edited Nearest Neighbours to aggressively prune noisy synthetic instances.

### 📊 6. Chemometric Quality Control & Diagnostic Visualizations
* **Comparative PCA:** Interactive Score plots (2D and 3D with 95% and 99% confidence ellipses), Loading plots, BiPlots, and Explained Variance scree plots.
* **Robust PCA (ROBPCA via `rrcov`):** Score Distance (SD) vs. Orthogonal Distance (OD) diagnostic plots using Minimum Covariance Determinant (MCD) to ensure synthetic samples do not introduce artificial leverage points.
* **t-SNE Manifold Projections:** Non-linear dimensionality reduction verifying that synthetic samples populate genuine manifold structures rather than artificial disconnected clusters.
* **Multivariate Statistical Tests:** Hotelling's $T^2$ centroid preservation test, Generalized Procrustes Analysis (`vegan`), Henze-Zirkler multivariate normality (`MVN`), and distance divergence metrics (`philentropy`).

### 💾 7. Flexible Data Export
* Direct export of the complete balanced dataset or synthetic-only instances to Microsoft Excel (`.xlsx`), CSV, and CSV2 (European semicolon/comma format).
* One-click download of PCA/t-SNE scores, loadings, and statistical summary matrices.

---

## 💻 Access & Execution Guide

You can access and run the platform using **any of the three modalities** below:

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
│  • Full source control  │             │   • 100% Offline (.exe) │            │  • Instant Web Access   │
│  • Single-file app.R    │             │   • Embedded R & Python │            │  • Zero Installation    │
│  • Auto-dependency setup│             │   • One-click desktop   │            │  • Any Browser / Device │
└─────────────────────────┘             └─────────────────────────┘            └─────────────────────────┘
```

---

### Option 1: Running in RStudio (Source Code)

Running the source code locally in RStudio provides full transparency, customization, and local computation speed.

#### Step 1: Clone or Download the Repository
Clone the repository using Git or download the repository ZIP file:
```bash
git clone https://github.com/LTAP-UERJ/Chemometrics-Web-App-Synthetic_Sampling.git
cd Chemometrics-Web-App-Synthetic_Sampling
```

#### Step 2: Automated Environment & Dependency Installation
We provide two convenience scripts—[`install_dependencies.R`](https://github.com/LTAP-UERJ/Chemometrics-Web-App-Synthetic_Sampling/blob/main/install_dependencies.R) and [`install_packages.R`](https://github.com/LTAP-UERJ/Chemometrics-Web-App-Synthetic_Sampling/blob/main/install_packages.R)—that automatically install and configure everything.

Open RStudio and run **either** command in the R Console:

```r
# Execute local installation script:
source("install_dependencies.R")

# Alternatively, the alias script can be used:
source("install_packages.R")
```

> [!TIP]
> **Direct One-Liner (No Prior Download Required):**
> You can also run the installer directly from GitHub in your RStudio Console:
> ```r
> source("https://raw.githubusercontent.com/LTAP-UERJ/Chemometrics-Web-App-Synthetic_Sampling/main/install_dependencies.R")
> ```

**What the installer does automatically:**
1. **CRAN Packages:** Installs and checks all required R packages (`shiny`, `shinydashboard`, `shinyWidgets`, `shinyBS`, `shinyjs`, `shinycssloaders`, `DT`, `plotly`, `ggplot2`, `readxl`, `writexl`, `openxlsx`, `mdatools`, `vegan`, `Hotelling`, `MASS`, `car`, `caret`, `rgl`, `robustbase`, `rrcov`, `philentropy`, `MVN`, `ellipse`, `Rtsne`, `smotefamily`, `imbalance`, `UBL`, `scutr`, `reticulate`, etc.).
2. **Loading Verification:** Verifies that every package loads correctly and outputs an informative checklist with `[OK]` status.
3. **Python & Hybrid ML Configuration:** Detects or provisions an isolated `r-reticulate` virtual environment and installs required Python packages (`scikit-learn`, `imbalanced-learn`, `numpy`, `pandas`, `scipy`).
4. **Fallback Safety:** If Python is not installed on the user machine, the installer logs a diagnostic note; **all native R resampling algorithms (SMOTE, SMOTE-NC, ADASYN, Random Upsampling, SBC) remain 100% operational**.

#### Step 3: Launching the Application in RStudio
* **Method A (Interactive GUI — Recommended):**
  1. Open `app.R` in RStudio.
  2. Notice the green **"Run App"** button at the top-right corner of the script editor.
  3. Click **"Run App"** (or press `Ctrl + Shift + Enter`).
* **Method B (Console Command):**
  ```r
  # In the project working directory:
  shiny::runApp()
  
  # Or specifying the file path directly:
  shiny::runApp("app.R")
  ```

> [!NOTE]
> **Single-File Self-Contained Execution:**
> The `app.R` file is engineered to be **100% standalone**. If someone has only this single file, opening it in RStudio and clicking **"Run App"** automatically detects missing CRAN packages, loads inline demo datasets, and runs seamlessly without requiring any external folders or assets.

---

### Option 2: Desktop Standalone Executable (.exe — Offline Windows)

For analytical laboratories, production environments, or institutional computers without administrative package compilation access or internet connectivity:

* **Key Benefits:**
  * **100% Offline:** Runs completely disconnected from the internet.
  * **Zero Setup:** Ships with pre-configured R-Portable and Python-Portable runtimes embedded inside the installer. No local R, RStudio, or Python installation is required.
  * **Safe & Isolated:** Will not modify or conflict with existing Python/R versions on the machine.

* **Download & Installation Instructions:**
  1. Open the Google Drive download directory:
     🔗 **[Download CWA: Synthetic Sampling Executable (Google Drive)](https://drive.google.com/drive/folders/1oeVCeKzjskS-HWGx7GWG4lyT3QvViLgF?usp=drive_link)**
  2. Download the installer: `setup_Synthetic_Sampling.exe`.
  3. Run the installer wizard (installs by default to `C:\LTAP_Modules\`).
  4. Launch the application anytime using the **Desktop Shortcut** or **Windows Start Menu**.

---

### Option 3: Online Web Application (Shinyapps.io — Cloud Deployment)

For immediate access, student classes, quick file evaluations, or running on non-Windows operating systems (macOS, Linux, Chromebooks, iPad, Android tablets):

* **Cloud Web App URL:**
  🔗 **[https://ltap.shinyapps.io/Synthetic_Sampling/](https://ltap.shinyapps.io/Synthetic_Sampling/)**

* **Highlights:**
  * Zero local installation or technical setup required.
  * Runs directly inside any modern web browser (Google Chrome, Mozilla Firefox, Microsoft Edge, Apple Safari).
  * Data privacy: Uploaded data resides strictly in volatile container RAM and is automatically purged upon closing the session.

---

## 🛠️ Technical Stack & Architecture

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
