# Chemometrics Web App — Synthetic Sampling

![Status](https://img.shields.io/badge/Status-Active-green)
![Version](https://img.shields.io/badge/Release-V2-orange)
![License](https://img.shields.io/badge/License-Proprietary%20%2F%20INPI%20Registered-red.svg)
![R](https://img.shields.io/badge/R%20Version-4.3.0%2B-blue.svg)
![Python](https://img.shields.io/badge/Python-v3.10%2B-yellow.svg)

Developed by the **[Process Analytical Technology Laboratory (LTAP-UERJ)](https://www.ltapuerj.com.br/)**, this application is a comprehensive platform for handling class imbalance in chemometric and machine learning datasets through advanced synthetic data generation, undersampling algorithms, hybrid resampling strategies, and multivariate quality control diagnostics.

---

## 🔗 Quick Links

* **Online Web App:** [Access on Shinyapps.io](https://ltap.shinyapps.io/Synthetic_Sampling/)
* **Desktop Executable Download:** [Download Executable (Google Drive)](https://drive.google.com/drive/folders/1l8dB4BGKVjqPMrvA5ZCLvafRNtTOCFew?usp=drive_link)
* **Official Website:** [LTAP-UERJ Portal](https://www.ltapuerj.com.br/)
* **Software Registration (INPI):** [LTAP-UERJ CWA — Registros de Software](https://sites.google.com/view/ltap-uerj/cwa)
* **Support & Licensing:** [licarion@gmail.com](mailto:licarion@gmail.com) | [ltapuerj@gmail.com](mailto:ltapuerj@gmail.com)

---

## 👥 Developers & Authors

This module was developed by the chemometrics research team at **LTAP-UERJ**:

| Author | Affiliation | Profile / Contact |
| :--- | :--- | :--- |
| **[Julio Cesar Siqueira](http://lattes.cnpq.br/1968914053746315)** | LTAP — UERJ | [Lattes Profile](http://lattes.cnpq.br/1968914053746315) |
| **[José Licarion Pinto Segundo Neto](http://lattes.cnpq.br/5267552018296169)** | LTAP — UERJ | [Lattes Profile](http://lattes.cnpq.br/5267552018296169) |
| **[Aderval Severino Luna](http://lattes.cnpq.br/0294676847895948)** | UERJ | [Lattes Profile](http://lattes.cnpq.br/0294676847895948) |
| **[Paulo Henrique Couto Simões](http://lattes.cnpq.br/5098915046998337)** | LTAP — UERJ | [Lattes Profile](http://lattes.cnpq.br/5098915046998337) |

---

## 🆕 Version History (Change Log)

### **V2 — Current Release**
* **Extended Resampling Library:** Added Clustering-Based Undersampling (SBC), SMOTE-IPF, and SPIDER hybrid methods.
* **Python Reticulate Integration:** Native execution of Python's `imbalanced-learn` and `smote-variants` libraries via `reticulate`, unlocking advanced oversampling/undersampling algorithms.
* **Diagnostic & Quality Control Engine:** Dedicated QC tab featuring comparative PCA, Robust PCA (Score Distance vs. Orthogonal Distance), t-SNE manifold projections, Hotelling's $T^2$, PERMANOVA, and Kolmogorov-Smirnov distribution tests.
* **Automated Report Builder:** Multi-language HTML/PDF report generator summarizing resampling distribution statistics and diagnostic metrics.
* **CWA Workspace Serialization:** Save and restore workspace states in `.RData` format for seamless inter-module data transfer across the CWA platform.

### **V1 — Initial Release**
* Core implementation of SMOTE, ADASYN, and basic random resampling methods.
* Initial data import and variable scaling pipeline.
* Exploratory PCA visualization for class distribution assessment.

---

## 🚀 Key Features

### 📥 Data Import & Preprocessing
* Flexible import of `.xlsx`, `.csv`, `.txt` files with support for transposed matrix formats (samples in columns).
* Variable preprocessing: Auto-scaling, Mean Centering, Median Centering, and normality-inducing transformations (Box-Cox, Yeo-Johnson).
* Automated detection of target class columns and zero-variance feature filtering.

### ⬆️ Oversampling (Upsampling) Methods
| Method | Description |
| :--- | :--- |
| **SMOTE** | Synthetic Minority Over-sampling Technique — linear interpolation between nearest neighbors. |
| **SMOTE-NC** | SMOTE adaptation for datasets containing mixed numerical and categorical features. |
| **Borderline-SMOTE** | Concentrates synthetic generation on minority samples located along class decision boundaries. |
| **SVM-SMOTE** | Uses Support Vector Machine (SVM) support vectors as seeds for synthetic sample generation. |
| **ADASYN** | Adaptive Synthetic Sampling — density-based method generating more samples in harder-to-learn regions. |
| **Random Upsampling** | Duplication of minority class instances for ultra-small sample contexts. |

### ⬇️ Undersampling (Downsampling) Methods
| Method | Description |
| :--- | :--- |
| **TOMEK Links** | Removes ambiguous majority-class instances forming Tomek links near class borders. |
| **NearMiss** | Distance-based majority reduction preserving samples closest to minority cluster centroids. |
| **Edited Nearest Neighbours (ENN)** | Removes samples whose class label differs from the majority of their $k$-nearest neighbors. |
| **One-Sided Selection (OSS)** | Combines Tomek Links with Condensed Nearest Neighbor (CNN) undersampling. |
| **Cluster-Based (SBC)** | Partitions majority class via $k$-means clustering and replaces dense clusters with centroids. |
| **Random Downsampling** | Random removal of majority class instances to achieve target imbalance ratios. |

### 🔀 Hybrid Methods
| Method | Description |
| :--- | :--- |
| **SMOTE-TL** | SMOTE oversampling followed by Tomek Links boundary cleaning. |
| **SMOTE-ENN** | SMOTE oversampling followed by ENN noise cleaning for sharper decision boundaries. |
| **SMOTE-IPF** | SMOTE combined with Iterative Partitioning Filter for ensemble-based noise removal. |
| **SPIDER** | Selective Preprocessing of Imbalanced Data — classifies samples into safe, borderline, and noisy. |

### 📊 Diagnostic & Quality Control (QC)
* **Comparative PCA & Robust PCA:** Before/after resampling diagnostic plots — Variance, Scores, Loadings, BiPlot, Leverage vs. Orthogonal Distance.
* **t-SNE Projection:** Non-linear manifold learning to verify spatial overlap and prevent synthetic sub-clustering.
* **Statistical Verification:** PERMANOVA, Hotelling's $T^2$ centroid test, Kolmogorov-Smirnov univariate distribution test, and Jensen-Shannon divergence.

---

## 🛠️ Technical Stack & Environment

### **Build & Compilation Environment**
* **Language R Runtime:** `R (>= 4.3.0)`
* **Language Python Engine:** `Python (v3.10+)` connected via R `reticulate`

### **Core R & Python Dependencies**
| Package / Library | Ecosystem | Purpose |
| :--- | :--- | :--- |
| **`shiny`** | R | Reactive application framework and web server architecture. |
| **`shinydashboard`** | R | Dashboard layout structure and sidebar navigation UI. |
| **`plotly`** | R | Interactive 2D/3D scatter plots, PCA projections, and t-SNE graphics. |
| **`DT`** | R | DataTables interface for dataset preview and resampled data inspection. |
| **`reticulate`** | R | Interoperability bridge connecting R data frames to Python objects. |
| **`imbalanced-learn`** | Python | Core algorithm engine for ADASYN, SVM-SMOTE, Tomek, NearMiss, ENN, OSS. |
| **`scikit-learn`** | Python | k-NN, SVM, and distance metrics for Python resampling algorithms. |
| **`smotefamily`** | R | Native R implementation of SMOTE and Borderline-SMOTE. |
| **`mdatools`** | R | Principal Component Analysis and outlier diagnostic metrics. |
| **`rrcov`** | R | Robust PCA via Minimum Covariance Determinant (ROBPCA). |
| **`Rtsne`** | R | Non-linear t-SNE manifold projections. |
| **`vegan`** | R | PERMANOVA (`adonis2`) multivariate variance testing. |

---

## 💻 Access & Execution

This application is distributed under proprietary closed-source terms (the underlying `app.R` source code is not publicly distributed). Access is available through two distribution models:

1. **🌐 Online Web Version (Shinyapps.io):**
   * Access directly via web browser without installing R or Python dependencies:
   * 🔗 **[https://ltap.shinyapps.io/Synthetic_Sampling/](https://ltap.shinyapps.io/Synthetic_Sampling/)**

2. **🖥️ Desktop Executable Version:**
   * Standalone Windows executable bundle (`.exe`) with embedded R and Python runtimes. No prior environment setup required on the target machine.
   * Download the executable for Synthetic Sampling and other LTAP CWA modules here:
   * 🔗 **[LTAP CWA Executables Folder (Google Drive)](https://drive.google.com/drive/folders/1l8dB4BGKVjqPMrvA5ZCLvafRNtTOCFew?usp=drive_link)**

---

## ⚠️ Methodological Guidelines

> [!IMPORTANT]
> **Critical recommendations for correct use of synthetic sampling:**
> - Do **not** excessively increase the sample count of a single minority class (keep oversampling ratio $\le 3\times$ original size) to avoid overfitted predictive models.
> - **Mandatory Test Set Isolation:** Synthetic samples must **only** be generated within the **training set** and never included in the validation or test set. Test sets must consist exclusively of real, experimentally acquired samples.
> - Always verify quality using the **Diagnostic & QC** tab (confirming non-significant PERMANOVA $p > 0.05$ and Hotelling's $T^2$ centroid preservation).

---

## 📜 License & Intellectual Property Protection

> [!CAUTION]
> **All Rights Reserved — Intellectual Property Protection (INPI)**
> 
> This software, its interface designs, compiled binaries, and underlying algorithmic implementations are protected under Intellectual Property laws (Brazilian Software Law No. 9.609/98 and Industrial Property Law No. 9.279/96) and registered at the **National Institute of Industrial Property (INPI)**. 

### **Terms of Use & Protection Clause:**
1. **Mandatory Attribution:** Any academic work, study, publication, software integration, or presentation utilizing or referencing this application **must explicitly credit** the authors (**Julio Cesar Siqueira, José Licarion Pinto Segundo Neto, Aderval Severino Luna, Paulo Henrique Couto Simões**) and the **Process Analytical Technology Laboratory ([LTAP-UERJ](https://www.ltapuerj.com.br/))**.
2. **Prohibition of Unauthorized Reproduction & Redistribution:** Copying, modifying, decompiling, reverse engineering, re-licensing, sub-licensing, mirroring, or redistributing the binary executables or deployment packages without explicit prior written consent from LTAP-UERJ is strictly prohibited.
3. **Non-Commercial Use Only:** The application may only be used for personal, educational, or non-commercial academic research purposes unless a specific commercial license has been granted by LTAP-UERJ.
4. **Disclaimer of Liability:** LTAP-UERJ and the developers accept no responsibility or liability for damages, misinterpretation, or loss resulting from the use of this software or its generated datasets. The software is provided "as is", without warranties of any kind.

For licensing inquiries or commercial use permissions, please contact [licarion@gmail.com](mailto:licarion@gmail.com) or [ltapuerj@gmail.com](mailto:ltapuerj@gmail.com).

---

## 📧 Contact & Institutional Support

**[Process Analytical Technology Laboratory (LTAP/UERJ)](https://www.ltapuerj.com.br/)**

We acknowledge financial and institutional support from **UERJ**, **FAPERJ** (JCNE and CNE research scholarships), **CNPq** (Universal Grant), and **CAPES** (Finance Code 001).

---

<p align="center">
  <a href="https://www.ltapuerj.com.br/">LTAP-UERJ</a> •
  <a href="https://www.uerj.br/">UERJ</a> •
  <a href="https://www.faperj.br/">FAPERJ</a> •
  <a href="https://www.gov.br/cnpq/pt-br">CNPq</a> •
  <a href="https://www.gov.br/capes/pt-br">CAPES</a>
</p>
```
