# Vigitel × PNS — viés de não cobertura e estratégias multimodais

Quantifica o viés de não cobertura do Vigitel (inquérito por telefone fixo) usando
a PNS como referência probabilística, decompõe o gap de prevalência em componente
de não cobertura e resíduo, e simula o desempenho de cinco cenários de quadro
amostral (fixo, celular, dual-frame, triplo-frame, multimodal) sob o arcabouço ADEMP.
Nenhum número deste projeto vem da literatura ou de memória: todo valor sai de código
que rodou sobre os microdados baixados.

## Ordem de execução

Rode na ordem. Todo script começa com `source(here::here("R", "00_setup.R"))`.

| # | Script | O que produz |
|---|---|---|
| 00 | `R/00_setup.R` | Pacotes, semente (20260803), parâmetros do estudo, paleta única, `logs/sessioninfo.txt` |
| 01 | `R/01_download.R` | Microdados em `data-raw/` (PNS, PNAD-C TIC, Censo/SIDRA); checa presença dos arquivos do Vigitel; `logs/download_summary.csv` |
| 02 | `R/02_harmonize.R` | `data/vigitel.rds`, `data/pns.rds`, `data/pnadc.rds`, `data/equivalencia.rds` (→ Tabela S1) |
| 03 | `R/03_design.R` | Objetos de desenho `srvyr` + validação contra prevalências oficiais publicadas |
| 04 | `R/04_table1.R` | `output/tables/table1.*` — características das amostras com SMD |
| 05 | `R/05_prevalences.R` | `output/tables/table2.*` — prevalências, Δ em pp, RP, interação sexo × inquérito, Holm |
| 06 | `R/06_noncoverage.R` | `output/tables/table3.*` — viés de não cobertura por indicador e região, vício relativo de Cochran |
| 07 | `R/07_partition.R` | `output/tables/table4.*` — partição gap = componente de não cobertura + resíduo |
| 08 | `R/08_simulation.R` | Simulação ADEMP, 5 cenários × região × indicador, ≥1.000 réplicas |
| 09 | `R/09_figures.R` | `output/figures/figure1.*` (Δ por indicador e sexo), `figure2.*` (REQM por cenário), `figureS1`–`figureS3` |
| 10 | `R/10_supplement.R` | `output/supplement/supplementary_material.docx` (Tabelas S1–S7, Figuras S1–S3, Textos S1–S4) + checagem de citação |
| 11 | `R/11_qa.R` | Verificações da seção 13 do protocolo; `logs/qa_resultado.csv` |
| 99 | `R/99_resultados_docx.R` | `output/resultados_vigitel_pns.docx` — resultados narrados, com todo número interpolado dos objetos gravados |

## Versão em inglês das tabelas e figuras

Os três scripts abaixo produzem a versão em inglês do material do artigo. Eles leem
os mesmos objetos de `data/derivado/` e **não recalculam nada**: traduzem rótulos,
notas e formatação numérica (ponto decimal, vírgula de milhar). Exigem que o
pipeline principal já tenha rodado.

| # | Script | O que produz |
|---|---|---|
| — | `R/labels_en.R` | Dicionários PT→EN e formatadores em inglês; carregado pelos três scripts abaixo |
| 10 | `R/10_tabelas_en.R` | `output/tables_en/table{1..6}.{rds,docx,html}` |
| 11 | `R/11_figuras_en.R` | `output/figures_en/figure{1,2,3}.{pdf,png}`, `figureS{1,2,3}.{pdf,png}` |
| 12 | `R/12_suplemento_en.R` | `output/tables_en/tableS{1..7}.*` e `output/supplement/supplementary_material_en.docx` |

`Rscript run_en.R` roda os três na ordem. Os enunciados dos questionários na Tabela S1
são traduzidos pelos autores; códigos de variável e a sintaxe oficial do Vigitel
permanecem no original.

### Manuscrito em ingles

`tools/traduz_docx.py` traduz para o ingles, dentro do proprio .docx, as tabelas e
as legendas de tabelas e figuras, preservando estrutura, estilos, numeracao e
marcadores de nota. Numeros nao sao redigitados: so a notacao muda (virgula
decimal -> ponto, milhar -> virgula, " a " -> " to "). O dicionario PT->EN fica em
`tools/traducoes_tabelas.py`; se algum segmento nao tiver traducao, o script
aborta sem gravar.

```
python3 tools/traduz_docx.py "Artigo versao.  boaoooooo.docx" --dry-run
python3 tools/traduz_docx.py "Artigo versao.  boaoooooo.docx"
```

As figuras do manuscrito foram trocadas pelas de `output/figures_en/`. A Figura 1
do manuscrito e a diferenca por indicador e sexo (geometria da v1), gerada em
ingles por `R/11_figuras_en.R` como `figure1_sex`.

`R/functions.R` é carregado pelo setup e não roda sozinho: formatação numérica
(vírgula decimal em PT, ponto em EN), estilo único das tabelas `gt`, exportação de
tabelas (.html/.docx/.rds) e figuras (.pdf/.png 300 dpi), SMD e helpers de log.

## Fontes e anos

| Fonte | Ano | Obtenção |
|---|---|---|
| PNS | 2019 | `PNSIBGE::get_pns()` — única edição com módulo de posse de telefone |
| Vigitel | 2019 | **manual** — sem API; ver `01_download.R` para os arquivos esperados em `data-raw/vigitel/` |
| PNAD Contínua TIC | 2019 | `PNADcIBGE::get_pnadc()` — parâmetros de posse de telefone para a simulação |
| Censo | 2022 | `sidrar::get_sidra()` — população adulta por sexo, faixa etária e capital |

## Convenções

- **Sinal:** Δ = Vigitel − PNS, em todas as tabelas e figuras, sem exceção.
- **Resíduo:** o que sobra do gap após a não cobertura chama-se "resíduo" ou
  "componente não atribuível à não cobertura". Nunca "efeito de modo" — modo é
  hipótese discutida, não rótulo de coluna.
- Comentários do código em português; nomes de objetos e funções em inglês.
- Todo caminho via `here::here()`; nunca `setwd()`.
- `[XX]` marca número ainda não calculado; `# PROXY:` marca aproximação metodológica.
  Ambos são varridos no QA (seção 13 do protocolo) antes da entrega.

## Dados

`data-raw/` e `data/` estão no `.gitignore`. Os microdados originais nunca são
editados: toda transformação acontece em `02_harmonize.R` e é gravada em `data/`.
