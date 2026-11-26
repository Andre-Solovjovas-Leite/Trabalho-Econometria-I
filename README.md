# O efeito *flypaper* nos municípios brasileiros

Código do artigo **“O efeito flypaper nos municípios brasileiros: evidências com variáveis instrumentais a partir do Fundo de Participação dos Municípios em 2023”**, de André Solovjovas Leite e Enzo Cascaldi da Paz (Econometria I, FGV EESP, 2026).

O artigo testa se as transferências incondicionais do Fundo de Participação dos Municípios (FPM) elevam a despesa municipal mais do que um aumento equivalente do PIB local. Usamos um corte transversal de 5.483 municípios em 2023 e instrumentamos o FPM efetivamente recebido pelo valor que resultaria da aplicação estrita da tabela de faixas populacionais do Decreto-Lei nº 1.881/1981 à população do Censo 2022.

## Especificação

$$\ln G_i = \beta_0 + \beta_1 \ln FPM_i + \beta_2 \ln PIB_i + X_i'\gamma + \delta_{UF} + \varepsilon_i$$

Todas as variáveis estão em termos *per capita*. Os controles $X_i$ são ln da população, taxa de urbanização e proporções de residentes com até 14 e com 65 anos ou mais. O instrumento é o FPM teórico

$$Z_i = \frac{FPM_{UF} \cdot c_i / \sum_{j \in UF} c_j}{Pop_i},$$

em que $c_i$ é o coeficiente legal da faixa populacional do município (de 0,6 a 4,0). Os erros-padrão são robustos à heterocedasticidade (HC1).

## Estrutura do repositório

```
.
├── flypaper_iv.R        # todo o processo: dados, estimação, testes e figuras
├── bases/               # dados de entrada (ver "Dados")
│   ├── rec_orc.csv
│   ├── desp_orc.csv
│   ├── desp_por-func.csv
│   └── ibge.csv         # criado pelo script na primeira execução
└── figuras/             # criada pelo script
```

## Dados

| Arquivo | Fonte | Conteúdo |
|---|---|---|
| `rec_orc.csv` | FINBRA/Siconfi, Receitas Orçamentárias (Anexo I-C) | FPM bruto realizado |
| `desp_orc.csv` | FINBRA/Siconfi, Despesas Orçamentárias (Anexo I-D) | despesas correntes e investimentos |
| `desp_por-func.csv` | FINBRA/Siconfi, Despesas por Função (Anexo I-E) | despesa total liquidada |
| `ibge.csv` | IBGE/SIDRA, tabelas 5938, 4709, 9514 e 9923 | PIB 2023, população, estrutura etária e população urbana (Censo 2022) |

**FINBRA.** No [Siconfi](https://siconfi.tesouro.gov.br), consulta FINBRA, Contas Anuais, exercício 2023, escopo Municípios. Baixe as três tabelas acima em CSV e renomeie-as como indicado. Não edite os arquivos: o script espera o formato exportado (três linhas de cabeçalho, separador `;`, vírgula decimal e codificação Latin-1).

**IBGE.** O script baixa as tabelas do SIDRA pela API na primeira execução (requer internet) e salva o resultado em `bases/ibge.csv`. Nas execuções seguintes, usa esse arquivo.

> **Atenção:** cada CSV do FINBRA tem entre 140 e 170 MB, acima do limite de 100 MB por arquivo do GitHub. Por isso eles estão no `.gitignore` e precisam ser baixados à parte (ou versionados com [Git LFS](https://git-lfs.com)). O `ibge.csv` tem menos de 1 MB e pode ser versionado.

## Como rodar

1. Coloque os CSV na pasta `bases`, ao lado de `flypaper_iv.R`.
2. Rode o script inteiro:
   - no RStudio: abra `flypaper_iv.R` e clique em **Source**;
   - no terminal: `Rscript flypaper_iv.R` (funciona também a partir de outro diretório, com o caminho do script).

O script localiza a pasta `bases` sozinho e instala os pacotes que faltarem (`AER`, `lmtest`, `sandwich`, `gmm`, `ggplot2` e `patchwork`; o `jsonlite` só é instalado se for preciso baixar os dados do SIDRA). A manipulação dos dados usa apenas funções básicas do R. Os resultados aparecem no console, identificados pela tabela ou seção do artigo, e as figuras são salvas em `figuras/`. Testado no R 4.3.

## Correspondência com o artigo

| Seção do script | Resultado no artigo |
|---|---|
| 3. Base e instrumento | amostra (seção 4.2) e instrumento (equação 4) |
| 4. Análise descritiva | Tabela 1 e números da seção 4.3 |
| 5. Estimação | Tabela 2: MQO, primeiro estágio e MQ2E |
| 6. Testes do efeito flypaper | Tabela 3: testes em reais e em elasticidades |
| 7. Diagnóstico do modelo | seção 5.3: Breusch-Pagan e RESET |
| 8. Especificações alternativas | Tabela 4 (incluindo o J de Hansen) e seção 5.5 (controles flexíveis de população e PIB) |
| 9. Figuras | Figuras 1 a 4 |

## Principais resultados

| | Estimativa |
|---|---|
| Elasticidade da despesa ao FPM, MQO | 0,499 (0,033) |
| Elasticidade da despesa ao FPM, MQ2E | 0,611 (0,019) |
| Elasticidade da despesa ao PIB, MQ2E | 0,271 (0,008) |
| F do primeiro estágio | 6.418,7 |
| Wu-Hausman | 23,3 (*p* < 0,001) |
| Propensão marginal a gastar do FPM (R$/R$) | 1,670 (0,051) |
| Propensão marginal a gastar do PIB (R$/R$) | 0,044 (0,001) |
| J de Hansen, *dummies* de faixa | 224,5 (16 gl; *p* < 0,001) |

Erros-padrão robustos (HC1) entre parênteses. A equivalência é rejeitada em reais e também no critério mais conservador, em elasticidades (*z* = 16,2). Com controles mais flexíveis de população (polinômio de quarto grau, que passa no RESET), a elasticidade ao FPM cai para 0,491 (0,025), e a equivalência continua rejeitada (*z* = 18,9 em reais); ver seção 5.5 do artigo.
