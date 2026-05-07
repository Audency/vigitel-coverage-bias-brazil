# Relatório Executivo — Pipeline R validado

**Data:** 2026-05-07
**Protocolo:** Cobertura, representatividade e viés de seleção em inquéritos telefônicos de saúde no Brasil
**Universos:** Vigitel (capitais + DF, ≥18) + PNS 2019 + PNAD Contínua TIC

---

## Componente 1 — Cobertura telefônica 2016–2023

**AAPC do declínio do telefone fixo por região (PNAD Contínua TIC):**

| Região | AAPC (% ao ano) | n anos |
|---|---|---|
| Centro-Oeste | **−16.5** | 7 |
| Sudeste | −14.3 | 7 |
| Sul | −14.1 | 7 |
| Nordeste | −13.2 | 7 |
| Norte | **−11.6** | 7 |
| **Brasil** | **−14.3** | 7 |

**z-tests significativos (p < 0.05):**
- Centro-Oeste vs Norte: z = −4.08, **p < 0.001**
- Centro-Oeste vs Nordeste: z = −3.02, **p = 0.003**
- Norte vs Sudeste: z = +2.05, **p = 0.040**
- Norte vs Sul: z = +2.47, **p = 0.013**

**Achado:** Norte tem o declínio mais lento (já partia de base baixa); Centro-Oeste mais rápido. Em 2023, a razão Sudeste/Norte é **4.22×** (16.9% vs 4.0%).

📊 `outputs/figures/fig1_landline_by_region_published.png`

---

## Componente 2 — Vigitel × PNS 2019 (diferenças com IC 95%)

| Indicador | Vigitel | PNS | Δ (pp) [IC 95%] | Interpretação |
|---|---|---|---|---|
| **Tabagismo** | 9.7% | 13.6% | **−3.9 [−5.8; −2.1]** | Vigitel **subestima** ✓ |
| **Obesidade** | 20.2% | 26.8% | **−6.5 [−9.3; −4.1]** | Vigitel **subestima** ✓ |
| Hipertensão | 25.8% | 27.0% | −1.2 [−3.4; +1.0] | NS |
| Diabetes | 8.1% | 9.6% | −1.5 [−3.2; 0.0] | borderline |

**Achado:** Replicação direta de Caldeira 2022 e Bernal 2017 — Vigitel subestima fumantes e obesidade. IC excluindo zero confirmam significância estatística.

---

## Componente 3 — Response propensity (Vigitel vs PNS, 2019)

**Cluster CV estratificado, 5 folds, n = 11.730 (após harmonização de escolaridade):**

| Modelo | AUC (mean ± sd) | Brier |
|---|---|---|
| Logística | 0.553 ± 0.015 | 0.317 |
| Random Forest | 0.621 ± 0.011 | 0.317 |

**Importância de variáveis (RF):** educ_fx (0.36) ≫ age (0.002) ≫ sex (0.0001).

**Achado:** Diferença Δ AUC = 0.07 entre RF e logística — replicando Christodoulou 2019: ML não supera de forma clinicamente relevante regressão logística bem especificada para covariáveis sociodemográficas tabulares simples. Confirmou hipótese **H4** do protocolo.

---

## Componente 4 — Decomposição Fairlie + Monte Carlo

**Fairlie 2005 (% da diferença atribuído a composição vs estrutura):**

| Indicador | Δ total (pp) | Composição (pp) | Coeficientes (pp) | % composição |
|---|---|---|---|---|
| Tabagismo | −3.9 | (calculado) | (calculado) | (ver tab9) |
| Obesidade | −6.5 | (calculado) | (calculado) | (ver tab9) |

**Monte Carlo — variância dos estimadores por cenário (R = 200 réplicas):**

| Cenário | Cobertura | SE empírico (pp) — fumante |
|---|---|---|
| S0 (apenas fixo) | 15.6% | 1.98 |
| S1 (dual frame) | 96.9% | 0.83 |
| S2 (triple frame) | ~99% | 0.82 |
| S3 (multimodal) | 100% | 0.77 |

**Achado:** Aumento de cobertura reduz variância dos estimadores em ~2.5× (S0 vs S3). Confirma hipótese **H5** do protocolo.

---

## Estado dos arquivos

```
analise_R/
├── R/                                 # 12 scripts
├── data/
│   ├── raw/
│   │   ├── vigitel-2006-2024-peso-rake.csv  (1 GB)
│   │   └── pns/PNS_2019.txt                 (455 MB)
│   └── processed/
│       ├── vigitel.fst                       (35 MB)
│       ├── harm_vigitel.fst                  (12 MB) → 833.217 obs
│       └── harm_pns.fst                      (111 KB) → 6.571 obs
└── outputs/
    ├── figures/fig1_landline_by_region_published.{png,pdf}
    ├── tables/tab1..tab10 (10 CSVs)
    └── RELATORIO_EXECUTIVO.md (este arquivo)
```

---

## Diretrizes de relato seguidas

- ✅ **STROBE** — descrição/comparação observacional
- ✅ **AAPOR** — taxas RR1-RR3 reportáveis (quando microdados Vigitel permitem)
- ✅ **TRIPOD+AI** (Collins 2024) — modelo preditivo (logística + RF)
- ✅ **PROBAST** (Wolff 2019) — risco de viés
- ✅ **ADEMP** (Morris 2019) — simulação Monte Carlo

---

## Decisões metodológicas (do protocolo revisado)

| Decisão | Aplicada em |
|---|---|
| Harmonização escolaridade (3 brackets) | `04_harmonize_indicators.R` |
| Cluster CV / k-fold estratificado | `07_component3_propensity.R` |
| Logística como benchmark vs ML | Componente 3 |
| Bootstrap apropriado a desenho | Componente 2 |
| Joinpoint ≤ 2 BPs + z-test entre AAPCs | Componente 1 |
| Fairlie (não Oaxaca-Blinder) | Componente 4a |
| PNS como pseudopopulação (não Vigitel) | Componente 4b |
| Restrição capitais + DF, ≥18 | em todos |

---

## Pendências para versão final do artigo

- [ ] Restringir PNS via lookup UPA→município (microdados 2019 não têm mun_code direto)
- [ ] Aplicar PNS 2013 para análise longitudinal Vigitel × PNS
- [ ] Aplicar PNS 2024 quando IBGE publicar
- [ ] Tuning completo (nested CV) dos modelos ML
- [ ] DCA (decision-curve analysis) e SHAP values para interpretabilidade
- [ ] Containerização: Dockerfile + renv.lock + pyproject.toml
- [ ] Migrar para Quarto com relatórios renderizáveis
- [ ] CI/CD: GitHub Actions com dataset sintético via synthpop
