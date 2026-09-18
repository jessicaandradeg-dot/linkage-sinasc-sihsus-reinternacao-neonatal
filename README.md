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

A resolução das ambiguidades foi realizada por meio de um algoritmo **greedy-global**, selecionando iterativamente o par com m
