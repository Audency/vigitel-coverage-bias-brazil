# CHANGELOG

## v0.7 (2026-05-07) — refinamentos pós-revisão Lancet

Sintetizando 5 pareceres de revisores estilo Lancet (Senior Editor, Methodologist, Equity, Skeptic, Regional Fit). Mudanças implementadas:

### Análise

- **Fairlie expandido**: nova especificação adicionando self-reported race/colour como covariável (v0.7 vs v0.6 sensibilidade)
  - Smoking: %composition robusto a especificação (-3% → 6% mudando covariáveis); coefficient/mode permanece dominante
  - Hypertension: %composition triplica (18% → 60%) ao adicionar raça — interpretação revisada
  - Diabetes/poor_health: padrões sensíveis, agora reportados como tal
- **Sensibilidade urbana**: PNS restrita a V0026=1 (urban; n=68k) como proxy para capitais. Smoking gap reduz de -3.1pp para -1.75pp; coefficient mantém-se dominante
- **Holm correction** aplicada aos 5 testes Vigitel-PNS — todos sobrevivem (p_holm < 0.001)
- **Tabela 4 nova**: comparação de sistemas de vigilância NCD nas Américas (BRFSS, ENSANUT, ENFR, ENS, CCHS, ENSIN)

### Manuscrito

- **Plain Language Summary** adicionada (Lancet PLP requirement)
- **Título atualizado**: "...a Brazilian case study with implications for the Americas"
- **Linguagem causal moderada**: "driver", "explain", "attributable" → "statistical contributor", "decomposed into"
- **H1 sinalizada como exploratória post-hoc** quando contradicta pelos dados (floor effect)
- **Limitações expandidas**: 7 → 5 limitações reorganizadas com sensibilidades reportadas
- **Confluence Lancet** verificada: Summary 5 seções, Research in Context, Findings antes Discussion, Contributors/Declarations/Data sharing

### Reprodutibilidade

- `git init` concluído e primeiro commit
- `.gitignore` exclui raw data (1.4 GB) e cache fst
- `pipeline_completo.R` consolida 12 scripts em arquivo único (694 linhas)

### Pendências para v0.8

- [ ] Renda + cor/raça no Vigitel via re-extração com q11/q69 (parcialmente feito — só q69 incorporado)
- [ ] Lookup UPA→município PNS via cadastro IBGE (Sala de Sigilo)
- [ ] Pseudopopulação independente do Censo 2022 + SRMI para Monte Carlo
- [ ] PROBAST tabela formal preenchida nos suplementos
- [ ] Bootstrap design-aware reportado nos suplementos
- [ ] Coautoria do Norte/Nordeste convidada
- [ ] Versão portuguesa do Summary + Implications como anexo (LRH-Americas)
- [ ] Zenodo DOI ativo antes de submissão

## v0.6 (2026-05-07) — primeira versão Lancet style

Convertido do v0.5 (formato BMC/RBE) para Lancet:
- Summary com Background/Methods/Findings/Interpretation/Funding
- Research in Context panel adicionado
- Findings antes de Discussion
- Estrutura Contributors/Declaration/Data sharing/Acknowledgments

## v0.5 (2026-05-07) — pós-revisão 2ª rodada

Após 5 revisores (Methodologist, Vigitel/PNS BR, Reproducibility, Editor, Skeptical) — corrigidos 3 dos 4 bloqueadores:
- Brier 0.317 → 0.204 (pesos normalizados por fonte)
- Fairlie tab9 zerada → resultados completos com bootstrap CI
- MC viés residual S3 → bias=0 com Bernoulli + Hájek
- PNS sem mun_code → limitação documentada (pendente)

## v0.4 (2026-05-06) — pós-revisão 1ª rodada

Após 6 revisores + editor:
- PROSPERO removido → OSF Registries
- Resolução CNS 466/2012 (não 510/2016)
- Bibliografia auditada (31 refs verificadas; órfãs removidas)
- Bland-Altman removido; Oaxaca-Blinder → Fairlie
- Monte Carlo com PNS como pseudopopulação
- Hipóteses direcionais H1-H5 (sem números fabricados)

## v0.3 (2026-04-16) — protocolo inicial

- Estrutura Vigitel × PNS × PNAD; 7 objetivos específicos
- Métodos: PS via logística + RF + GB; Monte Carlo 1000 réplicas
- Resultados Esperados com números fabricados (corrigido em v0.4)
