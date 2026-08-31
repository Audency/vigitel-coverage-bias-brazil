# Manuscript and presentation

Descrição detalhada do manuscrito da v1 e da apresentação que o acompanha. Os arquivos ficam em `../manuscript/`.

## Files

| File | Description | Format |
|---|---|---|
| `Manuscript_v1.3_FINAL.docx` | Manuscript v1.3, with embedded gtsummary tables and epi-themed figures | Word |
| `Presentation_v0.8.pptx` | Companion presentation, 18 slides, academic design | PowerPoint |

## v1.3 highlights (vs v1.2)

- **Embedded weighted gtsummary tables** (`tbl_svysummary`) directly in the manuscript text rather than only referenced
- All numeric values rendered with mid-dot decimal (`−14·3` not `-14.3`)
- En-dashes used consistently in numeric ranges and parenthetical notes
- Each Methods component cross-references the corresponding reporting guideline (STROBE [22], AAPOR [25], TRIPOD+AI [14], PROBAST [15], ADEMP [16], Fairlie [20])
- All references re-verified for Vancouver-style accuracy
- Each table caption includes underlying statistic notation, R package used, and footnote on derivation

## Manuscript structure

```
Title page
├── Abstract (≤350 words; structured Background/Methods/Results/Conclusions)
├── Keywords
├── Background (3 paragraphs)
├── Methods
│   ├── Study design and reporting guidelines
│   ├── Data sources
│   ├── Population and indicator harmonisation
│   ├── Statistical analysis (5 components, each with citations)
│   │   ├── Coverage time series
│   │   ├── Vigitel–PNS prevalence comparison
│   │   ├── Response propensity modelling
│   │   ├── Bias decomposition
│   │   └── Monte-Carlo simulation under ADEMP
│   ├── Validation against published values
│   └── Software, tables, figures, and reproducibility
├── Results (with 6 figures and 8 tables embedded)
│   ├── Sample characteristics and validation (Tables 1–2)
│   ├── Decline in landline coverage (Fig. 1, Table 3)
│   ├── Vigitel vs PNS prevalence comparison (Figs. 2–4, Tables 4–5)
│   ├── Response propensity (Table 6)
│   ├── Bias decomposition with BCa CIs (Fig. 5, Table 7)
│   └── Multimodal scenario simulation (Fig. 6, Table 8)
├── Discussion (6 paragraphs with limitations)
├── Conclusions
├── List of abbreviations
├── Declarations
│   ├── Ethics approval and consent to participate
│   ├── Consent for publication
│   ├── Availability of data and materials
│   ├── Competing interests
│   ├── Funding
│   ├── Authors' contributions
│   └── Acknowledgements
└── References (25 entries, Vancouver style)
```

## Tables and figures

### 8 tables (all embedded in manuscript)
1. Sample characteristics, Vigitel vs PNS (gtsummary `tbl_svysummary`)
2. Validation of weighted prevalence estimates against published official values
3. AAPC in landline coverage by region with Holm-corrected pairwise z-tests
4. Vigitel vs PNS prevalence comparison with Holm correction
5. Sex-stratified Vigitel vs PNS comparison
6. Discrimination and calibration of response propensity models
7. Fairlie decomposition with BCa bootstrap 95% CIs
8. Monte-Carlo performance metrics

### 6 figures (300 dpi PNG, Okabe-Ito colorblind-friendly palette, embedded in manuscript)
1. Decline in landline coverage by region (`fig1_coverage_epi.png`)
2. Vigitel temporal trend, 2006–2023 (`fig2_vigitel_trend_epi.png`)
3. Vigitel − PNS difference, forest plot (`fig3_forest_epi.png`)
4. Sex-stratified Vigitel − PNS difference (`fig4_sex_forest_epi.png`)
5. Fairlie decomposition stacked, sensitivity panels (`fig5_fairlie_epi.png`)
6. Monte-Carlo simulation under ADEMP (`fig6_montecarlo_epi.png`)

## Auxiliary tables (gtsummary auto-export, available in `../outputs/tables/`)

- `Table1_weighted.docx` — `tbl_svysummary` weighted output for sample characteristics
- `Table2_weighted.docx` — `tbl_svysummary` weighted output for prevalence comparison
- `Table3_propensity_logistic.docx` — `tbl_regression` for the logistic propensity model
- `Table4_fairlie_BCa.docx` — flextable with Fairlie BCa decomposition
- `Table5_sex_stratified.docx` — flextable for sex-stratified comparison
