# CHANGELOG

## v2.0 (2026-08-31) — novo desenho: partição do gap e simulação ADEMP

Redesenho completo do estudo. O pipeline em três componentes com escopo 2006–2023
foi arquivado em `archive_v1/`; o `main` passa a ser o desenho pareado de 2019.

### Desenho

- **Escopo pareado 2019**: Vigitel 2019 × PNS 2019, 26 capitais + DF, adultos ≥18 anos.
  A PNS 2019 é a única edição com módulo de posse de telefone, o que permite estimar
  o viés de não cobertura **inteiramente dentro da PNS**, sem depender do Vigitel.
- **Partição do gap** em componente de não cobertura e resíduo (Tabela 4). O resíduo
  não é chamado de "efeito de modo": absorve conjuntamente modo de coleta,
  autorrelato, instrumento, não resposta e calibragem, e este desenho não os separa.
- **Simulação ADEMP**, 5 cenários de quadro amostral × região × indicador, 1.000
  réplicas por célula, semente fixa (Tabela 5, Figura 2).
- **Validação externa da previsão** contra a adoção real do cadastro duplo pelo
  Vigitel em 2023 (Tabela 6).
- Bootstrap de Rao-Wu sobre o desenho amostral, 1.000 réplicas, para todos os
  intervalos das quantidades derivadas.

### Resultados principais

- 60,3% dos adultos das 27 capitais não tinham telefone fixo em 2019.
- O componente de não cobertura excede o gap observado ou tem sinal oposto a ele em
  todos os quatro indicadores; para hipertensão, +6,33 pp de viés de cobertura são
  compensados por −4,25 pp de resíduo.
- Vício relativo de Cochran acima de 0,40 nas 24 células indicador × domínio.
- REQM médio cai de 1,65 pp (só fixo) para 0,68 pp (cadastro duplo); além do duplo,
  o ganho é de centésimos.
- A transição real de 2023 concorda em sinal com a previsão em 3 dos 4 indicadores.

### Infraestrutura

- Pipeline em 13 scripts numerados, executados por `run_all.R`, parando no primeiro erro.
- **Versão em inglês** de tabelas, figuras e suplemento (`run_en.R`), lida dos mesmos
  objetos derivados: nada é recalculado, só rótulos e formatação numérica mudam.
- `tools/traduz_docx.py`: traduz as tabelas e legendas dentro do `.docx` sem
  redigitar nenhum número; aborta se faltar tradução de qualquer segmento.
- Microdados (9,4 GB) permanecem fora do repositório.

## v0.7 (2026-05-07) — refinamentos pós-revisão editorial interna

Sintetizando 5 pareceres de uma revisão editorial interna simulada (Senior Editor, Methodologist, Equity, Skeptic, Regional Fit). Mudanças implementadas:

### Análise

- **Fairlie expandido**: nova especificação adicionando self-reported race/colour como covariável (v0.7 vs v0.6 sensibilidade)
  - Smoking: %composition robusto a especificação (-3% → 6% mudando covariáveis); coefficient/mode permanece dominante
  - Hypertension: %composition triplica (18% → 60%) ao adicionar raça — interpretação revisada
  - Diabetes/poor_health: padrões sensíveis, agora reportados como tal
- **Sensibilidade urbana**: PNS restrita a V0026=1 (urban; n=68k) como proxy para capitais. Smoking gap reduz de -3.1pp para -1.75pp; coefficient mantém-se dominante
- **Holm correction** aplicada aos 5 testes Vigitel-PNS — todos sobrevivem (p_holm < 0.001)
- **Tabela 4 nova**: comparação de sistemas de vigilância NCD nas Américas (BRFSS, ENSANUT, ENFR, ENS, CCHS, ENSIN)

### Manuscrito

- **Plain Language Summary** adicionada (resumo em linguagem simples)
- **Título atualizado**: "...a Brazilian case study with implications for the Americas"
- **Linguagem causal moderada**: "driver", "explain", "attributable" → "statistical contributor", "decomposed into"
- **H1 sinalizada como exploratória post-hoc** quando contradicta pelos dados (floor effect)
- **Limitações expandidas**: 7 → 5 limitações reorganizadas com sensibilidades reportadas
- **Estrutura do artigo** verificada: Summary 5 seções, Research in Context, Findings antes Discussion, Contributors/Declarations/Data sharing

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

## v0.6 (2026-05-07) — primeira versão em formato de artigo

Convertido do v0.5 para o formato de artigo:
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
