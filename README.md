# Análise R — Cobertura, representatividade e viés de seleção em inquéritos telefônicos de saúde no Brasil

Pipeline reproduzível para o protocolo metodológico (Vigitel × PNS × PNAD-TIC), capitais + DF, ≥18 anos.

## Estrutura

```
analise_R/
├── config.R                          # Paths, constantes do estudo, seed
├── README.md                         # Este arquivo
├── R/
│   ├── 00_install_packages.R         # Instala pacotes faltantes (idempotente)
│   ├── 01_load_vigitel.R             # Lê CSV 1 GB → fst (releitura ~5s)
│   ├── 02_download_pns.R             # PNSIBGE 2013, 2019, 2024 (capitais + DF)
│   ├── 03_download_pnad_tic.R        # PNADcIBGE TIC 2016-2024
│   ├── 04_harmonize_indicators.R     # Harmoniza Vigitel/PNS/PNAD
│   ├── 05_component1_coverage.R      # Joinpoint + AAPC por região
│   ├── 06_component2_vigitel_pns.R   # Diferenças com IC bootstrap
│   ├── 07_component3_propensity.R    # Logística + RF + GB; cluster CV
│   ├── 08_component4_decomp_simulation.R  # Fairlie + Monte Carlo (ADEMP)
│   └── 09_run_all.R                  # Orquestrador
├── data/
│   ├── raw/                          # Vigitel CSV, PNS RDS, PNAD RDS
│   └── processed/                    # *.fst harmonizados
└── outputs/
    ├── figures/                      # Fig 1 (cobertura), Fig 2 (diffs), Fig 3 (MC)
    ├── tables/                       # tab1..tab10
    └── models/                       # propensity_models.rds
```

## Como rodar

```r
# 1. Configurar caminhos (já no config.R)
setwd("…/Audencio e Carla/analise_R")

# 2. Rodar pipeline completo (≈ 2-4 h primeira execução,
#    porque baixa PNS 2013 + PNS 2019 + PNAD TIC; depois cachê)
source("R/09_run_all.R")

# OU executar etapas individualmente
source("R/01_load_vigitel.R")          # ~ 60-120 s (1 GB CSV)
source("R/02_download_pns.R")          # ~ 5-15 min por edição (rede)
source("R/03_download_pnad_tic.R")     # ~ 3-8 min por ano
source("R/04_harmonize_indicators.R")  # ~ 30 s
source("R/05_component1_coverage.R")   # ~ 1 min
source("R/06_component2_vigitel_pns.R")# ~ 2 min (1000 boots)
source("R/07_component3_propensity.R") # ~ 10-30 min (tuning)
source("R/08_component4_decomp_simulation.R") # ~ 5-15 min (1000 MC)
```

## Diretrizes de relato implementadas

- **STROBE** — componente observacional (descrição/comparação)
- **AAPOR Standard Definitions** 9th — taxas RR1-RR3 (quando reportáveis no Vigitel)
- **TRIPOD+AI** (Collins 2024) — componente de modelagem preditiva (ML)
- **PROBAST** (Wolff 2019) — risco de viés do modelo
- **ADEMP** (Morris 2019) — simulação Monte Carlo

## Decisões metodológicas-chave

| Decisão | Por quê | Onde |
|---|---|---|
| Cluster-CV por capital | k-fold ingênuo viola estrutura amostral | `07_component3_propensity.R` |
| Logística como benchmark | Christodoulou 2019: ML raramente bate logística bem-especificada | `07_component3_propensity.R` |
| Pseudopopulação = PNS | Evita Monte Carlo circular | `08_component4_decomp_simulation.R` |
| Fairlie (não Oaxaca-Blinder) | Apropriado para variáveis binárias | `08_component4_decomp_simulation.R` |
| Bootstrap Rao-Wu (svrep) | Bootstrap ingênuo é inválido em desenho complexo | `06_component2_vigitel_pns.R` |
| Joinpoint ≤ 2 BPs | Série de 19 pontos: overfitting com mais | `05_component1_coverage.R` |
| Restrição capitais + DF, ≥18 | Comparabilidade Vigitel × PNS × PNAD | `04_harmonize_indicators.R` |

## Reprodutibilidade

- Seed declarada em `config.R` (`study$seed = 20260507`)
- Pacotes versionados (use `renv::init()` para bloqueio)
- Containerização: ver `Dockerfile` (a adicionar)
- Script `00_install_packages.R` é idempotente

## Pendências

- [ ] Verificar se microdados PNS 2024 já estão liberados pelo IBGE (esperado em 2026)
- [ ] Adicionar Dockerfile + renv.lock
- [ ] Adicionar testes unitários (`testthat`) para harmonização
- [ ] Migrar para Quarto (`.qmd`) com relatórios renderizáveis
- [ ] CI: GitHub Actions com dataset sintético via `synthpop`
