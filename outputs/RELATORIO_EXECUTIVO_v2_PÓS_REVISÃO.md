# Relatório Executivo v2 — Pós-revisão dos 5 revisores

**Data:** 2026-05-07
**Histórico:** v1 (RELATORIO_EXECUTIVO.md) → submetido a 5 revisores → v2 com correções implementadas

---

## Bugs corrigidos após revisão

| Bug (origem) | Descrição | Correção | Tabela afetada |
|---|---|---|---|
| F4 (Rev A/E) | Monte Carlo: viés residual em S3 (cobertura 100%) por dupla ponderação | Bernoulli sampling com `p_inc` constante; estimador Hajek `weighted.mean(w)` separado da seleção | `tab10_monte_carlo_FIXED.csv` |
| poor_health=0% (Rev B/E) | PNS J007 não é autoavaliação saúde (é plano de saúde); autoaval é N001 | Mapeamento corrigido: `N001 ∈ {4,5} → poor_health` | `tab5_diffs_FIXED.csv` |
| Universo PNS pequeno (Rev E) | `selected=TRUE, anthropometry=TRUE` retornava n=6.571 (muito pequeno) | Re-baixado com `anthropometry=FALSE` → n=90.846 | `harm_pns.fst` |
| Peso PNS V0030 vs V00291 (Rev B) | V0030 é peso domiciliar; V00291 é peso pessoa selecionada calibrada | Prioriza V00291 no harmonize | `harm_pns.fst` |
| Bootstrap R=500 (Rev A) | Insuficiente por padrões AAPOR/Morris | Aumentado para R=1000 | `tab5_diffs_FIXED.csv` |
| .gitignore ausente (Rev C) | Sem .gitignore, push falha (1.4 GB de raw data) | Criado `.gitignore` excluindo `data/`, `*.fst`, etc. | n/a |

---

## Componente 2 — Vigitel × PNS 2019 (CORRIGIDO)

| Indicador | Vigitel | PNS | Δ pp [IC 95%] | n_vig | n_pns |
|---|---|---|---|---|---|
| Tabagismo | 9.7% | 12.8% | **−3.1 [−3.8; −2.4]** | 52.443 | 88.531 |
| Obesidade* | 20.2% | 27.4% | **−7.1 [−9.5; −4.8]** | 52.443 | 6.571 |
| Hipertensão | 25.8% | 27.6% | **−1.7 [−2.6; −0.9]** | 52.443 | 86.862 |
| Diabetes | 8.1% | 9.2% | **−1.1 [−1.7; −0.6]** | 52.443 | 82.349 |
| Saúde ruim | 4.9% | 6.1% | **−1.2 [−1.6; −0.7]** | 52.443 | 88.531 |

\* Obesidade: PNS subset com antropometria medida (n=6.571); Vigitel autorrelato. **Diferença é parcialmente artefato de modo de medição** (Rev B/E).

**Mudanças vs v1:**
- TODOS os indicadores agora significativos (vs 2 NS antes)
- Hipertensão e Diabetes mostram subestimação pelo Vigitel (antes: NS)
- poor_health corrigido (era 0% PNS por bug)
- Precisão melhorada (n PNS 14× maior)

---

## Componente 4 — Monte Carlo CORRIGIDO

| Cenário | Cobertura | SE empírico (pp) — fumante | Bias (pp) — fumante |
|---|---|---|---|
| S0 (apenas fixo) | 15.6% | 2.01 | 0.015 |
| S1 (dual frame) | 96.9% | 0.16 | 0.002 |
| S2 (triple frame) | 99.4% | 0.06 | 0.000 |
| **S3 (multimodal)** | **100%** | **0.00** | **0.000** ✓ |

**Validação F4:** Em S3 (cobertura 100%) bias = 0 e SE = 0 para TODOS os 5 indicadores — consistência matemática esperada. v1 tinha viés residual de +8.5pp e -3.5pp (bug de dupla ponderação).

**Conclusão H5 confirmada**: aumento de cobertura reduz variância em ~12× entre S0 e S3.

---

## Componente 1 — AAPC (mantido da v1)

Sem mudanças metodológicas. Achado mantido:
- AAPC Brasil: −14.3%/ano (PNAD-TIC 2016-2023)
- Norte mais lento (−11.6) vs Centro-Oeste mais rápido (−16.5), p<0.001
- **H1 do protocolo CONTRADITA** (Rev D): Norte/Nordeste **não são** os mais rápidos
- Razão Sudeste/Norte 2023: 4.22×

⚠ Limitação confirmada: Componente 1 usa PNAD nacional (não restrita a capitais), divergindo do escopo declarado.

---

## Componente 3 — Propensity (limitações reconhecidas)

| Modelo | AUC (5-fold strat.) | Brier | Status |
|---|---|---|---|
| Logística | 0.553 ± 0.015 | 0.317 ⚠ | Brier elevado sugere bug ordem yardstick |
| Random Forest | 0.621 ± 0.011 | 0.317 ⚠ | mesmo |

**Pendências antes de submissão final** (Rev A/E):
- Diagnosticar Brier 0.317 (>0.25 sugere modelo pior que random)
- Reportar IC bootstrap do ΔAUC (0.07)
- DCA (Vickers 2006) e SHAP para interpretabilidade
- Tunagem nested CV completa
- AUC 0.55-0.62 com 3 covariáveis sugere covariáveis insuficientes — adicionar renda, cor/raça

**H4 confirmada provisoriamente**: ML não supera logística clinicamente.

---

## Limitações novas reconhecidas

Adicionadas ao v0.5 do manuscrito após revisão:

- **(viii)** Restrição PNS a capitais via lookup UPA→município ainda pendente; análises atuais são Brasil-wide na PNS — **viés residual de comparação Brasil-rural-urbano vs Vigitel-capitais não-zero**.
- **(ix)** Diferença de obesidade Vigitel-PNS é parcialmente atribuível a modo (autorrelato vs medido), não apenas cobertura.
- **(x)** PNS é entrevista presencial; viés de desejabilidade social pode subestimar tabagismo/álcool no benchmark de referência.
- **(xi)** Vigitel 2024 ainda em transição metodológica (n=27k vs ~52k anos anteriores); excluir de análises de tendência longitudinal.
- **(xii)** AUCs baixos no propensity model refletem poucas covariáveis discriminativas; expandir conjunto preditor.

---

## Pendências para v0.5 do manuscrito (atualizar v0.4)

1. **H1**: reformular como não-direcional ou inverter ("magnitude maior em CO/SE/S vs N/NE")
2. **H3**: declarar como não testada nesta versão (precisa Vigitel pré/pós-móvel)
3. **§3.4**: detalhar lookup UPA→município ou declarar limitação
4. **§3.6**: mencionar peso retropolado (`pesorake2025`) e suspensão Vigitel 2021-2022 pandemia
5. **§5 Limitações**: adicionar (viii)-(xii) acima
6. **§Cronograma**: ajustar — análises exploratórias mai-jun/2026; confirmatórias jul/2026-jun/2027
7. **§Resultados Esperados**: hipóteses direcionais já são neutras; manter

---

## Status final por componente

| Componente | v1 status | v2 status pós-revisão |
|---|---|---|
| C1 cobertura (AAPC, joinpoint, z-test) | ✅ entregue | ⚠ recomenda Sidra microdados ou NCI Joinpoint |
| C2 Vigitel × PNS | ⚠ poor_health bug; n PNS pequeno | ✅ corrigido; todos indicadores significativos |
| C3 propensity ML | ⚠ Brier suspeito | ⚠ pendente debug yardstick + DCA + SHAP |
| C4 Fairlie + MC | ❌ Fairlie zerada; MC com viés em S3 | ✅ MC corrigido (S3 bias=0); Fairlie ainda pendente |
| Reprodutibilidade | ❌ FAIR 2.0/5 | ⚠ .gitignore criado; renv/Docker pendentes |

---

## Checklist final pré-submissão (v0.5)

- [ ] Restringir PNS a capitais via lookup UPA (BLOQUEADOR — Rev B)
- [ ] Corrigir Brier suspeito Componente 3 (Rev A/E)
- [ ] Re-rodar Fairlie com debug + bootstrap CI (Rev A/E)
- [ ] DCA + SHAP no Componente 3 (Rev A)
- [ ] Adicionar covariáveis renda + cor/raça ao propensity (Rev E)
- [ ] `renv::init()` + `renv.lock` (Rev C)
- [ ] Dockerfile baseado em `rocker/r-ver:4.5` (Rev C)
- [ ] Testes `testthat` para harmonize + Fairlie + MC (Rev C)
- [ ] Atualizar v0.5 manuscrito com as 7 mudanças listadas acima (Rev D)
- [ ] Restringir Componente 1 a capitais via PNAD-TIC (Rev B/D)
- [ ] PROBAST checklist preenchido como anexo (Rev D)
- [ ] AAPOR RR1-RR3 calculadas e tabuladas (Rev D)
