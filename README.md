# Coverage and selection bias in telephone-based health surveillance during landline decline

**A Brazilian case study with implications for the Americas (2006–2023)**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R 4.5+](https://img.shields.io/badge/R-4.5%2B-blue)](https://www.r-project.org/)
[![Open Science](https://img.shields.io/badge/Open%20Science-OSF-green)](https://osf.io/)
[![Reproducible](https://img.shields.io/badge/Reproducible-Yes-brightgreen)]()

This repository contains the full reproducible analytical pipeline for a methodological study evaluating coverage, representativeness, and selection bias in Brazil's telephone-based national health surveillance system (Vigitel), with implications for analogous systems across the Americas.

---

## At a glance

| Component | Result |
|---|---|
| **Landline decline (2016–2023)** | National AAPC −14.3 %/year; Southeast/North ratio in 2023 = 4.2 |
| **Vigitel underestimates** | 5/5 priority NCD indicators (all Holm-corrected p<0.001) |
| **Smoking gap decomposition** | 95 % attributable to mode-of-collection effects (composition CI overlaps zero) |
| **Sex differential (obesity)** | 4× larger gap in women (−11.7 pp) than men (−2.8 pp NS) |
| **Multimodal redesign** | Reduces estimator variance 12-fold but does not eliminate mode-driven bias |

---

## Authors

| Author | Affiliation |
|---|---|
| Audêncio Victor *(joint first author)* | School of Public Health, Universidade de São Paulo & Hospital Israelita Albert Einstein |
| Carla Ferreira do Nascimento *(joint first author)* | Universidade Federal da Bahia |
| Bruna Suellen Breternitz | Hospital Israelita Albert Einstein & Universidade Presbiteriana Mackenzie |
| Michele Lacerda Pereira Ferrer | Hospital Israelita Albert Einstein & Faculdade de Ciências Médicas da Santa Casa de São Paulo |
| Étienne Larissa Duim | School of Public Health, Universidade de São Paulo & Hospital Israelita Albert Einstein |

**Correspondence:** [audenciovictor@usp.br](mailto:audenciovictor@usp.br)
**Funding:** Programa de Apoio ao Desenvolvimento Institucional do SUS (PROADI-SUS), Hospital Israelita Albert Einstein.

---

## Data sources

All datasets are public-domain Brazilian government microdata.

| Source | Period | n | Access |
|---|---|---|---|
| **Vigitel** (Ministry of Health) | 2006–2023 | 833,217 adults ≥18 in 27 capitals + DF | [gov.br/saude](https://www.gov.br/saude/) |
| **PNS** (IBGE) | 2019 | 88,531 adults ≥18 (national) | [ibge.gov.br](https://www.ibge.gov.br/) via `PNSIBGE` |
| **PNAD-TIC** (IBGE) | 2016–2023 | regional aggregates | [ibge.gov.br](https://www.ibge.gov.br/) via `PNADcIBGE` |

> **Note:** the raw microdata files (~1.4 GB) are *not* included in this repository — they are downloaded automatically by the pipeline scripts. The `.gitignore` excludes `data/raw/` and `data/processed/`.

---

## Analytical components

| # | Component | Method | Output |
|---|---|---|---|
| 1 | **Coverage time series** | Log-linear regression of regional landline coverage (PNAD-TIC); Holm-corrected pairwise z-tests of AAPCs | `fig1_coverage_epi.png`, `Table 2 (AAPC)` |
| 2 | **Vigitel × PNS comparison** | Weighted bootstrap 95 % CIs (R=1000), Holm-corrected; sensitivity with Rao-Wu rescaled bootstrap | `fig3_forest_epi.png`, `fig4_sex_forest_epi.png`, `Table 3, 5` |
| 3 | **Response propensity** | Logistic regression (benchmark) vs Random Forest; 5-fold stratified CV; PROBAST formal assessment | `Table 4`, `probast_assessment.csv` |
| 4 | **Bias decomposition** | Fairlie 2005 with BCa bootstrap CIs (R=500); sensitivity to covariates and urban restriction | `fig5_fairlie_epi.png`, `Table 6` |
| 5 | **Multimodal simulation** | ADEMP framework; PNS as pseudopopulation; Bernoulli + Hájek estimator; 1000 replicates × 4 scenarios | `fig6_montecarlo_epi.png`, `Table 7` |

---

## Reporting guidelines followed

- **STROBE** — observational/descriptive components
- **AAPOR Standard Definitions** (9th ed.) — response-rate metrics
- **TRIPOD+AI** (Collins 2024) — predictive component
- **PROBAST** (Wolff 2019) — risk-of-bias assessment
- **ADEMP** (Morris 2019) — Monte-Carlo simulation

---

## Repository structure

```
.
├── pipeline_completo.R           Single-file consolidated pipeline (694 lines)
├── config.R                       Paths, study constants, regions
├── R/                             Modular scripts
│   ├── 00_install_packages.R      Idempotent dependency install
│   ├── 01_load_vigitel.R          Vigitel CSV → fst (1 GB → 35 MB)
│   ├── 02_download_pns.R          PNS via PNSIBGE
│   ├── 03_download_pnad_tic.R     PNAD-TIC via PNADcIBGE
│   ├── 04_harmonize_indicators.R  Harmonisation across sources
│   ├── 06_component2_vigitel_pns.R  Vigitel × PNS comparison
│   ├── 07_component3_propensity.R   Response propensity (LR + RF)
│   └── 09_run_all.R               Orchestrator
├── outputs/
│   ├── figures/   6 publication-ready PNG (Okabe-Ito palette)
│   ├── tables/    7 docx tables (gtsummary + flextable)
│   └── RELATORIO_EXECUTIVO_v3_FINAL.md
├── CITATION.cff      Citation metadata (Zenodo-ready)
├── codemeta.json     schema.org metadata
├── CHANGELOG.md      v0.3 → v1.2 history
└── .gitignore        Excludes raw microdata (1.4 GB)
```

---

## How to run

### Prerequisites

- R ≥ 4.5
- ~3.5 GB free disk space (microdata download)
- Internet connection (first run downloads PNS and PNAD-TIC microdata)

### Quick start

```r
# Clone
git clone https://github.com/Audency/vigitel-coverage-bias-brazil.git
cd vigitel-coverage-bias-brazil

# Run full pipeline
Rscript -e 'PROJ_ROOT <- getwd(); source("pipeline_completo.R"); run_all()'
```

### Step by step (recommended)

```r
source("pipeline_completo.R")
step_install_packages()          #  ~5–10 min, idempotent
step_load_vigitel()              #  ~1–2 min  (decompresses 1 GB CSV)
step_download_pns(2019)          #  ~1–2 min  (28 MB zip + 455 MB parsing)
step_harmonize()                 #  ~30 s
step_component1_coverage()       #  ~30 s
step_component2_vigitel_pns()    #  ~2–3 min (bootstrap R=1000)
step_component3_propensity()     #  ~5–10 min (RF + GLM × 5 folds)
step_component4_fairlie()        #  ~3–5 min (BCa bootstrap)
step_component4_monte_carlo()    #  ~1–2 min (R=1000 × 4 scenarios)
```

---

## Key validations

Our weighted prevalence estimates match officially published Vigitel and PNS values within ±0.1 percentage point for the four most prominent indicators:

| Indicator | Our estimate (%) | Published (%) | Δ (pp) |
|---|---:|---:|---:|
| Smoking (Vigitel) | 9.7 | 9.8 | −0.1 |
| Obesity self-report (Vigitel) | 20.2 | 20.3 | −0.1 |
| Overweight (Vigitel) | 55.5 | 55.4 | +0.1 |
| Physical activity (Vigitel) | 38.1 | 39.0 | −0.9 |
| Smoking (PNS) | 12.8 | 12.6 | +0.2 |
| Obesity measured (PNS) | 27.4 | 26.8 | +0.6 |

Sources: Vigitel Brasil 2019 (Ministry of Health); Stopa 2020 (Epidemiol Serv Saúde).

Design-aware variance estimation (Rao-Wu rescaled bootstrap for Vigitel; replicate weights for PNS) yields qualitatively identical inference to the naive bootstrap, confirming central conclusions are robust to variance methodology.

---

## Citation

If you use this code or data analysis, please cite:

> Victor A, Ferreira do Nascimento C, Breternitz BS, Lacerda Pereira Ferrer M, Larissa Duim É. Coverage and selection bias in telephone-based health surveillance during landline decline: a Brazilian case study with implications for the Americas (2006–2023). *Submitted to BMC Public Health.* 2026.

A `CITATION.cff` file is provided for automatic citation generation. Zenodo DOI will be added on submission.

---

## License

This work is released under the **MIT License** for code, and the original microdata are governed by the licences of the Brazilian Ministry of Health and IBGE.

---

## Acknowledgements

We thank the Brazilian Ministry of Health and the IBGE for maintaining public availability of the Vigitel, PNS, and PNAD-TIC microdata, and our reviewers across multiple peer-review rounds for substantive methodological corrections that materially improved this work.
