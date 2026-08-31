# output — o que cada pasta contém

Tudo aqui é gerado por código, na ordem descrita no [README](../README.md).
Nada é editado à mão.

| Pasta | Conteúdo |
|---|---|
| `tables_en/` | Tabelas 1–6 e S1–S7 em inglês: `.docx` (para o manuscrito), `.html` (para leitura rápida) e `*_dados.rds` (os dados por trás de cada tabela) |
| `tables/` | As mesmas tabelas em português |
| `figures_en/` | Figuras em inglês: `.pdf` vetorial e `.png` a 300 dpi, 180 mm de largura |
| `figures/` | As mesmas figuras em português |
| `supplement/` | Material suplementar montado (`supplementary_material_en.docx` e `material_suplementar.docx`) |
| `logs/` | Proveniência: validação externa contra os valores publicados, grade completa da simulação, estrutura etária, versões de pacotes e `sessioninfo.txt` |

## Correspondência entre as figuras

| Arquivo | O que mostra |
|---|---|
| `figure1_sex` | Δ de prevalência por indicador e sexo — **Figura 1 do manuscrito** |
| `figure2` | REQM por cenário de quadro amostral e região — **Figura 2** |
| `figure1` | Diferença observada ao lado das suas duas parcelas (componente de não cobertura e resíduo) |
| `figure3` | Previsto em 2019 × observado na transição de 2023, com linha de identidade |
| `figureS1`–`figureS3` | Figuras suplementares |

Os objetos `gt` intermediários (`table*.rds`, 1,3 MB cada) não vão para o
repositório: o entregável é o `.docx`. Os `*_dados.rds`, esses sim, ficam.
