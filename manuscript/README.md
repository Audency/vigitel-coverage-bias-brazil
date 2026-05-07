# Manuscript and presentation

This folder contains the final manuscript and presentation for submission.

## Files

| File | Description | Format |
|---|---|---|
| `Manuscript_BMC_Public_Health_v1.2_FINAL.docx` | Final manuscript, BMC Public Health style | Word |
| `Presentation_v0.8.pptx` | Companion presentation (18 slides) | PowerPoint |

## Manuscript structure

The v1.2 manuscript follows BMC Public Health guidelines:

- **Abstract** (335 words; structured Background/Methods/Results/Conclusions)
- **Background** (3 paragraphs)
- **Methods** (organised by sub-section with explicit citations)
  - Study design and reporting guidelines
  - Data sources
  - Population and indicator harmonisation
  - Statistical analysis (5 components with citations)
  - Software, tables, figures, and reproducibility
- **Results** (with 6 figures and 6 tables embedded)
  - Sample characteristics and validation
  - Decline in landline coverage
  - Vigitel vs PNS prevalence comparison (overall + sex-stratified)
  - Response propensity
  - Bias decomposition with BCa CIs
  - Multimodal scenario simulation
- **Discussion** (6 paragraphs)
- **Conclusions**
- **List of abbreviations**
- **Declarations** (Ethics, Consent, Data, Competing interests, Funding, Authors' contributions, Acknowledgements)
- **References** (25 entries)

## Tables and figures

### Tables
1. Sample characteristics, Vigitel vs PNS
2. AAPC in landline coverage by region
3. Vigitel vs PNS prevalence comparison
4. Response propensity discrimination/calibration
5. Fairlie decomposition with BCa CIs
6. Monte-Carlo performance

Auxiliary tables (gtsummary auto-export) available in `../outputs/tables/`:
- `Table1_weighted.docx`, `Table2_weighted.docx` — gtsummary `tbl_svysummary` weighted output
- `Table3_propensity_logistic.docx` — gtsummary `tbl_regression`
- `Table4_fairlie_BCa.docx`, `Table5_sex_stratified.docx` — flextable

### Figures (in `../outputs/figures/`, Okabe-Ito palette, 300 dpi PNG)
1. Decline in landline coverage by region (`fig1_coverage_epi.png`)
2. Vigitel temporal trend 2006–2023 (`fig2_vigitel_trend_epi.png`)
3. Vigitel − PNS forest plot (`fig3_forest_epi.png`)
4. Sex-stratified forest plot (`fig4_sex_forest_epi.png`)
5. Fairlie decomposition stacked sensitivity (`fig5_fairlie_epi.png`)
6. Monte-Carlo simulation (`fig6_montecarlo_epi.png`)
