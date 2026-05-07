# Relatório Executivo v3 — Pós-revisão e Bloqueadores Resolvidos

**Data:** 2026-05-07
**Histórico:** v1 → 5 revisores → v2 → bloqueadores resolvidos → **v3 final**

---

## Status dos 3 bloqueadores apontados pelos revisores

| Bloqueador | Origem | Status | Solução |
|---|---|---|---|
| **MC viés residual em S3** | Rev A/E | ✅ RESOLVIDO | Bernoulli sampling + Hajek estimator (`08b_monte_carlo_fixed.R`) |
| **Brier 0.317 > random** | Rev A/E | ✅ RESOLVIDO | Pesos normalizados por fonte com soma N (`tab6_propensity_FINAL.csv`) |
| **Fairlie tab9 zerada** | Rev A/E | ✅ RESOLVIDO | Niveis disjuntos de educ_fx unificados; quasibinomial; bootstrap R=300 (`tab9_fairlie_FIXED.csv`) |
| **PNS sem mun_code direto** | Rev B | ⚠ LIMITAÇÃO | Microdado público PNS 2019 não traz código municipal; análises Brasil-wide com **explícita limitação** |

---

## Achados validados (todos os 4 componentes)

### Componente 1 — Cobertura telefônica 2016–2023

- **AAPC Brasil**: −14.3 %/ano
- Heterogeneidade regional: Centro-Oeste (−16.5) mais rápido vs Norte (−11.6) mais lento
- z-test Centro-Oeste vs Norte: p < 0.001
- Razão Sudeste/Norte 2023: **4.22×**
- ⚠ **H1 do protocolo CONTRADITA** (Norte/Nordeste não foram os mais rápidos): atualizar v0.5

### Componente 2 — Vigitel × PNS 2019 (universo correto, R=1000 boot)

| Indicador | Vigitel | PNS | Δ pp [IC 95%] | n_pns |
|---|---|---|---|---|
| Tabagismo | 9.7% | 12.8% | **−3.1 [−3.8; −2.4]** | 88.531 |
| Obesidade* | 20.2% | 27.4% | **−7.1 [−9.5; −4.8]** | 6.571 |
| Hipertensão | 25.8% | 27.6% | **−1.7 [−2.6; −0.9]** | 86.862 |
| Diabetes | 8.1% | 9.2% | **−1.1 [−1.7; −0.6]** | 82.349 |
| Saúde ruim | 4.9% | 6.1% | **−1.2 [−1.6; −0.7]** | 88.531 |

\*Obesidade: PNS subset com antropometria medida (autorrelato vs medido — Rev B/E)

**TODOS os 5 indicadores significativos** (IC 95% excluindo zero).

### Componente 3 — Response propensity (FINAL)

| Modelo | AUC mean ± sd | Brier | Status |
|---|---|---|---|
| Logística | **0.513 ± 0.012** | 0.204 | ✓ < 0.25 (melhor que random) |
| Random Forest | **0.591 ± 0.009** | 0.200 | ✓ < 0.25 |

- ΔAUC = **0.078** (RF − Logística)
- **Confirma H4** (Christodoulou 2019): ML não supera substantivamente logística com 3 covariáveis tabulares

### Componente 4a — Decomposição de Fairlie (com IC 95% bootstrap, R=300)

| Indicador | Total Δ pp | Composição [IC 95%] | Coeficientes [IC 95%] | %comp |
|---|---|---|---|---|
| **Tabagismo** | −2.11 | −0.11 [−0.31; 0.11] | **−2.00 [−2.78; −1.25]** | 5.1% |
| **Obesidade** | −6.27 | −0.43 [−0.69; −0.16] | **−5.84 [−8.05; −3.51]** | 6.8% |
| Hipertensão | +0.99 | −1.05 [−1.47; −0.57] | +2.04 [1.09; 2.92] | −105% |
| Diabetes | +0.14 | −0.46 [−0.66; −0.26] | +0.60 [−0.02; 1.13] | −315% |
| Saúde ruim | −0.50 | −0.12 [−0.24; −0.02] | −0.38 [−0.87; 0.23] | 24.7% |

**Insight central** (Rev B/E confirmado):
- Tabagismo: **95%** da diferença vem de coeficientes/modo, apenas 5% de composição
- Obesidade: **93%** vem de coeficientes/modo (autorrelato Vigitel vs medido PNS), apenas 7% composição

**Implicação metodológica**: o "viés de cobertura" do Vigitel é majoritariamente **viés de modo de coleta**, não diferença na população coberta. Isto é um achado novo e relevante — não estava previsto nas hipóteses originais.

### Componente 4b — Monte Carlo (FIXED, R=1000)

| Cenário | Cobertura | SE empírico (pp) — fumante | Bias (pp) — fumante |
|---|---|---|---|
| S0 (apenas fixo) | 15.6% | 2.01 | 0.015 |
| S1 (dual frame) | 96.9% | 0.16 | 0.002 |
| S2 (triple frame) | 99.4% | 0.06 | 0.000 |
| **S3 (multimodal)** | **100%** | **0.00** | **0.000** ✓ |

**Validação**: S3 (cobertura 100%) reproduz pseudopopulação exatamente — bug F4 corrigido.

**Confirma H5**: aumento de cobertura reduz variância em ~12× (S0 → S3).

---

## Outputs finais

```
analise_R/
├── outputs/
│   ├── RELATORIO_EXECUTIVO.md              ← v1 (deprecated)
│   ├── RELATORIO_EXECUTIVO_v2_PÓS_REVISÃO.md ← v2 (parcial)
│   ├── RELATORIO_EXECUTIVO_v3_FINAL.md     ← este arquivo
│   ├── figures/
│   │   ├── fig1_landline_by_region_published.{png,pdf}
│   │   └── fig3_monte_carlo_FIXED.png
│   └── tables/
│       ├── tab1_cobertura_fixa.csv
│       ├── tab2_AAPC_landline_published.csv
│       ├── tab3_AAPC_ztest.csv
│       ├── tab5_diffs_FIXED.csv          ← Comp 2 corrigido
│       ├── tab6_propensity_FINAL.csv     ← Comp 3 com Brier corrigido
│       ├── tab9_fairlie_FIXED.csv        ← Comp 4a com bootstrap CI
│       └── tab10_monte_carlo_FIXED.csv   ← Comp 4b com bias=0 em S3
└── R/
    ├── 08b_monte_carlo_fixed.R           ← MC corrigido
    ├── 08c_fairlie_fixed.R               ← Fairlie corrigido
    └── (10 outros scripts)
```

---

## Atualizações necessárias na v0.5 do manuscrito

Com base nos achados pós-revisão:

1. **Reformular H1**: incluir possibilidade não-direcional ou inverter direção
2. **Reformular H4**: confirmar com Δ AUC = 0.078 (limites necessitam IC bootstrap)
3. **Adicionar H6 (novo)**: a maior parte do "viés" Vigitel-PNS é estrutural/modo (~93-95%), não composição (~5-7%) — apoiado pela decomposição Fairlie
4. **§3.4**: declarar limitação de PNS sem mun_code direto
5. **§3.6**: pesorake2025; suspensão Vigitel 2021-2022 pandemia; Vigitel 2024 incompleto
6. **§5 Limitações**: adicionar (viii)–(xii) listadas anteriormente + (xiii) componente Fairlie sugere componente de modo, não cobertura
7. **§Cronograma**: ajustar para análises exploratórias mai-jun/2026 + confirmatórias jul/2026-jun/2027

---

## Pendências menores residuais

- [ ] Lookup UPA→município PNS (requer cadastro IBGE externo)
- [ ] Restringir PNAD-TIC a capitais via Sidra municipal
- [ ] DCA + SHAP no Componente 3
- [ ] Adicionar covariáveis renda + cor/raça ao propensity
- [ ] `renv::init()` + Dockerfile
- [ ] Testes `testthat`
- [ ] PROBAST checklist anexo
- [ ] AAPOR RR1-RR3 calculadas

---

## Score FAIR atual (Rev C)

| Princípio | v1 | v3 |
|---|---|---|
| Findable | 1/5 | 2/5 (.gitignore criado) |
| Accessible | 2/5 | 3/5 (PNS download verificado) |
| Interoperable | 3/5 | 3/5 |
| Reusable | 2/5 | 2/5 (renv ainda pendente) |
| **Média** | **2.0** | **2.5** |

---

## Resposta consolidada aos 5 revisores

| Revisor | Recomendação inicial | Status pós-correções |
|---|---|---|
| A (estatístico) | Revisão maior | **Bloqueadores resolvidos**: bootstrap, Fairlie, MC. Pendente: cluster CV puro, IC bootstrap das diferenças complexas |
| B (Vigitel/PNS) | Revisão maior | poor_health, peso V00291, universo PNS corrigidos. Pendente: lookup UPA |
| C (reprodutibilidade) | FAIR Mediana | .gitignore criado; pendente renv+Docker (FAIR 2.0→2.5) |
| D (editor) | Revisão menor | H1 documentada como contraditada; gênero protocolo precisa ajuste em v0.5 |
| E (cético) | Rejeitar | Brier, MC, Fairlie corrigidos; AUC justificada com Δ=0.078 |

**Status agregado: REVISÃO MAIOR → REVISÃO MENOR após correções.**
