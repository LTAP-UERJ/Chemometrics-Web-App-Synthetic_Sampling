# Chemometrics Web App — Synthetic Sampling

![Status](https://img.shields.io/badge/Status-Active-green)
![Version](https://img.shields.io/badge/Release-V2-orange)
![License](https://img.shields.io/badge/License-Proprietary%20%2F%20INPI%20Registered-red.svg)
![R](https://img.shields.io/badge/Language-R-blue.svg)
![Python](https://img.shields.io/badge/Language-Python-yellow.svg)

Developed by the **Process Analytical Technology Laboratory (LTAP-UERJ)**, this application is a comprehensive tool for handling class imbalance in chemometric and machine learning datasets through synthetic data generation and resampling strategies.

---

## 🔗 Quick Links

* **Online Version:** [Access the Web App](https://ltap.shinyapps.io/Synthetic_Sampling/)
* **Software Registration (INPI):** [LTAP-UERJ CWA — Registros de Software](https://sites.google.com/view/ltap-uerj/cwa)
* **Support/Feedback:** [ltapuerj@gmail.com](mailto:ltapuerj@gmail.com)

---

## 🆕 Version History (Change Log)

### **V2 — Current Release**
* **Extended Method Library:** Added Clustering-Based Undersampling, SMOTE-IPF, and SPIDER hybrid methods.
* **Python Integration:** Full integration with the `imbalanced-learn` and `smote-variants` Python libraries via `reticulate`, enabling access to advanced oversampling and undersampling algorithms.
* **Diagnostic Tools:** Dedicated diagnostic section with PCA and Robust PCA comparative plots (before vs. after resampling) and t-SNE projections for visual validation.
* **Report Generation:** Automated report builder with multi-language support, allowing export of full analysis summaries.
* **Session Save/Restore:** Workspace serialization in `.RData` format for cross-session continuity and cross-module data transfer within the CWA platform.

### **V1 — Initial Release**
* Core implementation of SMOTE, ADASYN, and basic random resampling methods.
* Initial data import and preprocessing pipeline.
* Exploratory PCA visualization for class distribution assessment.

---

## 🚀 Key Features

### 📥 Data Import & Preprocessing
* Flexible import of `.xlsx` and `.csv` files, with support for transposed formats (samples in columns).
* Configurable decimal and delimiter settings.
* Optional class-column detection and spectral data flag.
* Variable preprocessing: Auto-scaling, Mean Centering, Median Centering, and normality-inducing transformations (Box-Cox, Yeo-Johnson).

### ⬆️ Oversampling (Upsampling) Methods
| Method | Description |
| :--- | :--- |
| **SMOTE** | Synthetic Minority Over-sampling Technique — interpolation between nearest neighbors. |
| **SMOTE-NC** | SMOTE for datasets with numerical and categorical features. |
| **Borderline-SMOTE** | Focuses synthetic generation on boundary samples (danger zone). |
| **SVM-SMOTE** | Uses Support Vector Machine to identify support vectors as seeds for synthesis. |
| **ADASYN** | Adaptive Synthetic Sampling — density-based, generates more samples where classification is harder. |
| **Random Upsampling** | Simple duplication of minority class instances. |

### ⬇️ Undersampling (Downsampling) Methods
| Method | Description |
| :--- | :--- |
| **TOMEK Links** | Removes borderline majority-class samples forming Tomek links. |
| **NearMiss** | Distance-based majority reduction towards minority cluster centroids. |
| **Edited Nearest Neighbours (ENN)** | Removes samples whose class differs from the majority of their neighbors. |
| **One Sided Selection (OSS)** | Combines Tomek Links with CNN to clean the majority class. |
| **Clustering-Based** | Undersamples by replacing cluster members with cluster centroids. |
| **Random Downsampling** | Random removal of majority class instances. |

### 🔀 Hybrid Methods
| Method | Description |
| :--- | :--- |
| **SMOTE-TL** | SMOTE oversampling followed by Tomek Links cleaning. |
| **SMOTE-ENN** | SMOTE oversampling followed by ENN cleaning for a cleaner decision boundary. |
| **SMOTE-IPF** | SMOTE combined with Iterative Partitioning Filter for noise removal. |
| **SPIDER** | Selective Preprocessing of Imbalanced Data — strength-based selective oversampling. |

### 📊 Diagnostic & Results
* **PCA & Robust PCA:** Before/after comparison plots — Variance, Loadings, Scores, BiPlot, Residuals, Outliers (2D and 3D).
* **t-SNE Projection:** Non-linear dimensionality reduction for visual assessment of class separability.
* **Results Table:** Downloadable resampled dataset and class distribution summary.

---

## 💾 Installation & Usage

### **How to Run (R/RStudio)**
After the installation process, simply open the application in the RStudio environment and click the **"Run App"** button.

### **Python Environment**
This application requires a Python environment with the following packages:
```
imbalanced-learn
scikit-learn
numpy
pandas
smote-variants (optional, for extended variant library)
```
The app uses Python v3.13+ via `reticulate`. A compatible Python environment must be configured before launching.

### **Distribution Models**
* **Version Shinyapps Version 01:** Open source and free access.
* **Version Desktop Version 01:** Available as an executable.

---

## ⚠️ Methodological Guidelines

> [!IMPORTANT]
> **Critical recommendations for correct use of synthetic sampling:**
> - Do **not** excessively increase the number of samples for a single class to avoid overfitted predictive models.
> - Synthetic samples must **only** be present in the **training set** and never in the test set, to avoid trends, overfitting, and bias errors in model lifecycle evaluation.

---

## 📜 License & Intellectual Property Protection

> [!CAUTION]
> **All Rights Reserved — Intellectual Property Protection (INPI)**
> 
> This software, its source code, interface designs, visual assets, and underlying algorithmic implementations are protected under Intellectual Property laws (Brazilian Software Law No. 9.609/98 and Industrial Property Law No. 9.279/96) and registered at the **National Institute of Industrial Property (INPI)**. 

### **Terms of Use & Protection Clause:**
1. **Mandatory Attribution:** Any academic work, study, publication, software integration, or presentation utilizing or referencing this application **must explicitly credit** the original authors (**Paulo Henrique Couto Simões, Julio Cesar Siqueira, Licarion Pinto, Aderval Luna**) and the **Process Analytical Technology Laboratory (LTAP-UERJ)**.
2. **Prohibition of Unauthorized Reproduction & Redistribution:** Copying, modifying, decompiling, reverse engineering, re-licensing, sub-licensing, mirroring, or redistributing the source code or binary executables without explicit prior written consent from LTAP-UERJ is strictly prohibited.
3. **Non-Commercial Use Only:** The application may only be used for personal, educational, or non-commercial academic research purposes unless a specific commercial license has been granted by LTAP-UERJ.
4. **Disclaimer of Liability:** LTAP-UERJ and the developers accept no responsibility or liability for damages, misinterpretation, or loss resulting from the use of this software or its generated datasets. The software is provided "as is", without warranties of any kind.

For licensing inquiries or commercial use permissions, please contact [ltapuerj@gmail.com](mailto:ltapuerj@gmail.com).

---

## 📧 Contact & Team

**Process Analytical Technology Laboratory (LTAP/UERJ)**

| Name | Email |
| :--- | :--- |
| **Paulo Henrique Couto Simões** | [ph.simoes@gmail.com](mailto:ph.simoes@gmail.com) |
| **Julio Cesar Siqueira** | [juliosiqueira86@hotmail.com](mailto:juliosiqueira86@hotmail.com) |
| **Licarion Pinto** | [licarion@gmail.com](mailto:licarion@gmail.com) |
| **Aderval Luna** | [adsluna@gmail.com](mailto:adsluna@gmail.com) |

---

<p align="center">
  <a href="https://www.ltapuerj.com.br/">LTAP-UERJ</a> •
  <a href="https://www.uerj.br/">UERJ</a> •
  <a href="https://www.faperj.br/">FAPERJ</a> •
  <a href="https://www.gov.br/cnpq/pt-br">CNPq</a> •
  <a href="https://www.gov.br/capes/pt-br">CAPES</a>
</p>
