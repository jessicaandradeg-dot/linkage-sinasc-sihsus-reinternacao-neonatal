# Linkage SINASC–SIH-SUS para Identificação de Reinternação Neonatal

Projeto de integração e pareamento de registros entre as bases do Sistema de Informações sobre Nascidos Vivos (SINASC) e do Sistema de Informações Hospitalares do SUS (SIH-SUS), desenvolvido para identificação de reinternações neonatais.

## Sobre o projeto

A identificação de reinternações neonatais a partir de bases administrativas do SUS apresenta um desafio de integração de dados, pois o SINASC e o SIH-SUS não possuem um identificador individual comum.

Este projeto desenvolve e avalia um procedimento de **linkage híbrido**, combinando estratégias determinísticas e probabilísticas para relacionar registros de nascimento e internação.

A análise foi realizada com dados referentes ao Estado do Rio de Janeiro em 2024, considerando nascimentos hospitalares registrados no SINASC e internações registradas no SIH-SUS.

## Objetivo

Desenvolver uma estratégia de pareamento entre registros do SINASC e SIH-SUS para identificar reinternações neonatais ocorridas entre o 4º e o 27º dia de vida.

## Metodologia

O processo de linkage foi estruturado em duas etapas principais:

### 1. Linkage determinístico

Foi utilizada uma chave composta por:

* Data de nascimento (`DTNASC`)
* Sexo (`SEXO`)
* Raça/cor (`RACACOR`)
* Município de residência (`CODMUNRES`)
* Município do evento (`CODMUNOCOR`)

Os casos com correspondência única foram identificados diretamente.

### 2. Linkage baseado em evidências

Para situações ambíguas, foi utilizado um **modelo de regras ponderadas (rule-based evidence score)**, considerando:

* concordância do CNES;
* concordância entre prematuridade e diagnóstico P07;
* concordância entre baixo peso e diagnóstico P05;
* concordância entre Apgar baixo e diagnósticos selecionados.

Os pesos utilizados foram:

* CNES: 3,0
* Prematuridade + P07: 1,5
* Baixo peso + P05: 1,5
* Apgar baixo + diagnósticos selecionados: 1,5

A resolução das ambiguidades foi realizada por meio de um algoritmo **greedy-global**, selecionando iterativamente o par com maior evidência de correspondência dentro de cada grupo de candidatos.

Foi utilizado um limiar de decisão de **0,70**, além de uma análise de sensibilidade com diferentes valores de cutoff.

## Dados

Foram considerados:

* **148.494 registros de nascimento** após aplicação dos critérios iniciais de pareamento;
* **32.421 registros de internação** elegíveis para a etapa de linkage.

Após a aplicação das restrições do estudo e do procedimento de pareamento, foram identificadas **323 reinternações neonatais**, correspondendo a uma taxa de **0,23%** na coorte final de 138.705 nascimentos.

A análise de sensibilidade avaliou cutoffs entre **0,60 e 0,80**.

## Resultados principais

| Indicador                   | Resultado |
| --------------------------- | --------: |
| Nascimentos analisados      |   148.494 |
| Internações analisadas      |    32.421 |
| Pareamentos iniciais        |     1.899 |
| Nascimentos na coorte final |   138.705 |
| Reinternações identificadas |       323 |
| Taxa identificada           |     0,23% |
| Cutoffs avaliados           | 0,60–0,80 |

A taxa de 0,23% corresponde aos casos identificados pela regra de linkage adotada e deve ser interpretada considerando as restrições da chave de pareamento e as limitações do procedimento.

## Análise de sensibilidade

A robustez do procedimento foi avaliada variando o limiar de decisão e comparando o escore completo com uma versão baseada apenas na concordância de CNES.

Os resultados permaneceram próximos de 0,23% para os limiares entre 0,65 e 0,80, enquanto o cutoff de 0,60 produziu aumento da taxa identificada.

## Tecnologias

* R
* dplyr
* tibble
* stringr
* lubridate
* broom

## Estrutura do repositório

```text
linkage-sinasc-sihsus-reinternacao-neonatal/
│
├── README.md
│
├── codigo/
│   └── scripts_R
│
├── artigo/
│   └── artigo.pdf
│
└── resultados/
    └── README.md
```

## Reprodutibilidade e proteção de dados

Os dados individuais utilizados no estudo **não são disponibilizados neste repositório**.

O repositório contém código, documentação e o artigo relacionado ao projeto. Dados nominais ou registros que possam permitir a identificação de indivíduos não são incluídos.

## Referência

GOMES, Jéssica de Andrade; ALMEIDA, Núbia Karla de Oliveira; ALMEIDA, Renan Moritz VR. *Pareamento Híbrido de Registros entre SINASC e SIH-SUS em Estudos sobre Reinternação Neonatal*.

Trabalho desenvolvido no contexto de pesquisa em Engenharia Biomédica, com aplicação de métodos de integração e análise de dados de saúde pública.

## Autoria

**Jéssica de Andrade Gomes**
Programa de Engenharia Biomédica — COPPE/UFRJ

**Núbia Karla de Oliveira Almeida**
Departamento de Estatística — Universidade Federal Fluminense

**Renan Moritz VR Almeida**
Programa de Engenharia Biomédica — COPPE/UFRJ


