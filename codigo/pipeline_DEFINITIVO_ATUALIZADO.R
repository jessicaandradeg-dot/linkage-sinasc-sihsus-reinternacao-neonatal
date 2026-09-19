library(dplyr)
library(tibble)
library(stringr)
library(lubridate)
library(broom)

# =============================================================================
# PIPELINE FINAL DEFINITIVO -- LINKAGE SINASC x SIH-SUS, MODELO DE REGRAS
# PONDERADAS (pesos fixos, NÃO estimados por EM)
# Reinternação neonatal, Rio de Janeiro, 2024
#
# *** VERSÃO COM MELHORIAS INSPIRADAS NO LABSUS ***
# Adicionadas:
#  - Validação de datas mais rigorosa
#  - Estrutura de diretórios escalável
#  - Logging estruturado
#  - Análise de subgrupos clínicos (Apgar, Peso, Paridade)
#  - Pseudo-ID para validação cruzada
#  - Exportação estruturada de resultados
#  - Checagens de consistência expandidas
#
# *** VERSÃO CORRIGIDA (Tabela III) ***
# Correção: a Tabela III original misturava, sob o rótulo "não pareados",
# duas situações logicamente distintas:
#   (1) nascimentos que NUNCA tiveram nenhuma internação correspondente
#       encontrada (nem determinística, nem probabilística);
#   (2) nascimentos que TIVERAM uma internação pareada, mas essa
#       internação caiu fora da janela de reinternação (4-27 dias) --
#       tipicamente a própria internação do parto.
# Isso gerava números logicamente impossíveis nos cenários B e F
# (determinísticos, onde por definição ninguém pode ficar "sem
# pareamento") e inflava artificialmente os "não pareados" de C, D, E
# acima do valor da própria Tabela II.
# A informação para separar as duas situações já existia na base
# (`resolvido_por` não é sobrescrito pela regra da janela) -- só não
# estava sendo usada no resumo. A correção está isolada nas funções
# `resumir_tabela_linkage()` (agora com 3 categorias) e na nova
# `gerar_tabela_III_completa()` (junta isso com os excluídos por
# gestação múltipla/anomalia). Nada mais no pipeline muda.
#
# ESTRUTURA (nessa ordem):
#   PARTE 0 -- Configurações
#   PARTE 0.1 -- Estrutura de diretórios e logging
#   PARTE 1 -- Leitura e preparo dos dados brutos (uma vez só)
#   PARTE 2 -- Funções genéricas do pipeline (classificação de cenários,
#              resolução determinística, escore de regras ponderadas,
#              pareamento guloso, combinação final)
#   PARTE 3 -- RODADA OFICIAL: chave do artigo, cutoff = 0,70
#              -> gera Tabela de cenários, Tabela III (linkage final)
#   PARTE 4 -- ANÁLISE DE SENSIBILIDADE: cutoff (0,60-0,80) x uso de
#              termos clínicos no escore -> Tabela V
#   PARTE 5 -- COMPARAÇÃO DE CHAVES: município (artigo) x CNES
#              -> quantifica o viés da chave
#   PARTE 6 -- REGRESSÃO PRINCIPAL (Tabela IV, ponderada) + univariada bruta
#   PARTE 7 -- REGRESSÃO ESTRATIFICADA POR MECANISMO DE RESOLUÇÃO
#              (determinístico B/F vs. probabilístico C/D/E)
#   PARTE 8 -- Checagens finais de consistência (denominadores, contagem
#              de internações elegíveis) -- EXPANDIDA
#   PARTE 9 -- NOVO: Análise de robustez por subgrupos clínicos
#   PARTE 10 - NOVO: Pseudo-ID para validação cruzada
#   PARTE 11 - NOVO: Exportação estruturada de resultados
#
# Ajuste os caminhos dos arquivos na PARTE 0 antes de rodar.
# =============================================================================


# #############################################################################
# PARTE 0 -- CONFIGURAÇÕES
# #############################################################################
key_municipio <- c("DTNASC", "SEXO", "RACACOR", "CODMUNRES", "CODMUNOCOR")
key_cnes      <- c("DTNASC", "SEXO", "RACACOR", "CODMUNRES", "CNES")

CUTOFF_OFICIAL <- 0.70
VALOR_NACIONAL_BRASILEIRA <- 10
IDADE_CORTE_F  <- 3     # cenário F: prioriza internação mais precoce após o 3º dia
JANELA_IDADE   <- 4:27  # janela de reinternação neonatal usada no documento

# Pesos do escore de regras ponderadas (definidos a priori -- ver nota
# metodológica na Parte 2.4)
PESO_CNES <- 3.0
PESO_CLIN <- 1.5
INTERCEPT_LOGISTICA  <- 0.619
INCLINACAO_LOGISTICA <- 0.458

CAMINHO_SINASC <- "C:/Users/jeess/OneDrive/Área de Trabalho/Mestrado - 2026/SINASC_2024.csv"
CAMINHO_SIH    <- "C:/Users/jeess/OneDrive/Área de Trabalho/Mestrado - 2026/sih.csv"


# #############################################################################
# PARTE 0.1 -- ESTRUTURA DE DIRETÓRIOS E LOGGING (inspirado em Labsus)
# #############################################################################

# Criar estrutura de diretórios
DIR_BASE <- getwd()
DIR_RESULTADOS <- file.path(DIR_BASE, "resultados_linkage_cbeb")
DIR_LOGS <- file.path(DIR_BASE, "logs")
DIR_FIGURAS <- file.path(DIR_BASE, "figuras")

for (dir in c(DIR_RESULTADOS, DIR_LOGS, DIR_FIGURAS)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

# Setup de logging estruturado
log_file <- file.path(DIR_LOGS, sprintf("linkage_cbeb_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S")))
sink(file = log_file, append = FALSE)

cat("================================================================================\n")
cat("PIPELINE LINKAGE SINASC x SIH - READMISSÃO NEONATAL (CBEB_DEFINITIVO)\n")
cat("Versão: ATUALIZADA COM MELHORIAS LABSUS\n")
cat(sprintf("Executado em: %s\n", Sys.time()))
cat("================================================================================\n\n")

cat(sprintf("[%s] Iniciando estrutura de diretórios e logging...\n", Sys.time()))
cat(sprintf("[%s]   → Diretório de resultados: %s\n", Sys.time(), DIR_RESULTADOS))
cat(sprintf("[%s]   → Diretório de logs: %s\n", Sys.time(), DIR_LOGS))
cat(sprintf("[%s]   → Arquivo de log: %s\n\n", Sys.time(), log_file))


# #############################################################################
# PARTE 1 -- LEITURA E PREPARO (uma vez só, com todos os campos que
# qualquer uma das chaves ou análises possa precisar)
# AGORA COM VALIDAÇÕES DE DATAS MAIS RIGOROSAS (inspirado em Labsus)
# #############################################################################

cat(sprintf("[%s] ETAPA 1: Leitura e Preparo dos Dados\n", Sys.time()))

sinasc_bruto <- read.csv(CAMINHO_SINASC, header = TRUE, sep = ";")
sih_bruto    <- read.csv(CAMINHO_SIH,    header = TRUE, sep = ",")

cat(sprintf("[%s]   → SINASC bruto: %d linhas\n", Sys.time(), nrow(sinasc_bruto)))
cat(sprintf("[%s]   → SIH bruto: %d linhas\n", Sys.time(), nrow(sih_bruto)))

sinasc_t1 <- sinasc_bruto %>%
  mutate(
    CODMUNNASC = str_pad(CODMUNNASC, 6, pad = "0"),
    UF         = str_sub(CODMUNNASC, 1, 2),
    DTNASC     = as.Date(str_pad(as.character(DTNASC), 8, pad = "0"), format = "%d%m%Y"),
    SEXO       = as.integer(SEXO),
    RACACOR    = as.integer(RACACOR),
    CODMUNRES  = as.integer(CODMUNRES),
    CNES       = str_pad(as.character(CODESTAB), 7, pad = "0"),
    CODMUNOCOR = str_sub(CODMUNNASC, 1, 6)
  ) %>%
  filter(
    UF == "33",
    year(DTNASC) == 2024,
    month(DTNASC) < 12,
    LOCNASC == 1,
    !is.na(DTNASC)  # ✅ Validação adicional: sem datas NA
  )

n_sinasc_excluidos <- nrow(sinasc_bruto) - nrow(sinasc_t1)

sih_t1 <- sih_bruto %>%
  mutate(
    DTNASC     = as.Date(as.character(NASC),     format = "%Y%m%d"),
    DT_INTER   = as.Date(as.character(DT_INTER), format = "%Y%m%d"),
    DT_SAIDA   = as.Date(as.character(DT_SAIDA), format = "%Y%m%d"),
    SEXO       = as.integer(SEXO),
    RACACOR    = as.integer(RACA_COR),
    CODMUNRES  = as.integer(MUNIC_RES),
    CNES       = str_pad(as.character(CNES), 7, pad = "0"),
    CODMUNOCOR = str_pad(as.character(MUNIC_MOV), 6, pad = "0"),
    UF_OCOR    = str_sub(CODMUNOCOR, 1, 2)
  ) %>%
  filter(UF_OCOR == "33", year(DTNASC) == 2024)

# ✅ VALIDAÇÕES ROBUSTAS DE DATAS (inspirado em Labsus)
sih_t1_validado <- sih_t1 %>%
  filter(
    !is.na(DTNASC),
    !is.na(DT_INTER),
    !is.na(DT_SAIDA),
    # Internação deve ser após nascimento
    DT_INTER >= DTNASC,
    # Alta deve ser após entrada
    DT_SAIDA >= DT_INTER,
    # Permanência razoável (< 180 dias para neonato)
    as.numeric(DT_SAIDA - DT_INTER) <= 180
  )

n_sih_removidos <- nrow(sih_t1) - nrow(sih_t1_validado)

cat(sprintf("[%s]   → Registros SINASC excluídos por filtros: %d (%.2f%%)\n",
            Sys.time(), n_sinasc_excluidos, 100 * n_sinasc_excluidos / nrow(sinasc_bruto)))
cat(sprintf("[%s]   → Registros SIH removidos por datas inválidas: %d (%.2f%%)\n",
            Sys.time(), n_sih_removidos, 100 * n_sih_removidos / nrow(sih_t1)))

sih_t2 <- sih_t1_validado %>% filter(NACIONAL == VALOR_NACIONAL_BRASILEIRA)

if (nrow(sih_t2) == 0) stop("ATENÇÃO: filtro de NACIONAL zerou a base.")

cat(sprintf("[%s]   → SINASC após filtros: %d\n", Sys.time(), nrow(sinasc_t1)))
cat(sprintf("[%s]   → SIH após filtros: %d internações elegíveis\n\n", Sys.time(), nrow(sih_t2)))


# #############################################################################
# PARTE 2 -- FUNÇÕES GENÉRICAS DO PIPELINE
# #############################################################################

# -----------------------------------------------------------------------
# 2.1 Prepara sinasc/sih para uma chave específica, com filtro de NA
#     dinâmico (só exclui por NA nos campos que a própria chave usa).
# -----------------------------------------------------------------------
preparar_bases <- function(key, verbose = TRUE) {
  n_sinasc_antes <- nrow(sinasc_t1)
  sinasc_filtro <- sinasc_t1 %>% filter(if_all(all_of(key), ~ !is.na(.)))
  n_excluido_sinasc <- n_sinasc_antes - nrow(sinasc_filtro)

  sinasc <- sinasc_filtro %>%
    transmute(
      .id_nasc = row_number(),
      DTNASC, SEXO, RACACOR, CODMUNRES, CODMUNOCOR, CNES,
      IDADEMAE, ESTCIVMAE, PARTO, APGAR5, PESO,
      ESCMAE, ESCMAE2010, SEMAGESTAC, TPROBSON,
      PARIDADE, KOTELCHUCK, GRAVIDEZ, IDANOMAL, LOCNASC
    ) %>%
    mutate(PARIDADE = as.integer(PARIDADE))

  n_sih_antes <- nrow(sih_t2)
  sih_filtro <- sih_t2 %>% filter(if_all(all_of(key), ~ !is.na(.)))
  n_excluido_sih <- n_sih_antes - nrow(sih_filtro)

  sih <- sih_filtro %>%
    select(DTNASC, SEXO, RACACOR, CODMUNRES, CODMUNOCOR, CNES, IDADE, COD_IDADE, DIAG_PRINC, DT_INTER)

  if (nrow(sih) == 0) stop("ATENÇÃO: sih ficou vazio para esta chave.")

  if (verbose) {
    cat(sprintf("SINASC: excluídos por NA na chave: %d (%.2f%%) | restam: %d\n",
                n_excluido_sinasc, 100 * n_excluido_sinasc / n_sinasc_antes, nrow(sinasc)))
    cat(sprintf("SIH:    excluídos por NA na chave: %d (%.2f%%) | restam (internações elegíveis): %d\n",
                n_excluido_sih, 100 * n_excluido_sih / n_sih_antes, nrow(sih)))
  }

  list(sinasc = sinasc, sih = sih,
       n_excluido_sinasc = n_excluido_sinasc, n_excluido_sih = n_excluido_sih)
}

# -----------------------------------------------------------------------
# 2.2 Classifica os 6 cenários (A-F) para uma chave dada.
# -----------------------------------------------------------------------
classificar_cenarios <- function(sinasc, sih, key) {
  f_sinasc <- sinasc %>% count(across(all_of(key))) %>% rename(n_sinasc = n)
  f_sih    <- sih    %>% count(across(all_of(key))) %>% rename(n_sih    = n)
  tab <- left_join(f_sinasc, f_sih, by = key)
  tab$n_sih[is.na(tab$n_sih)] <- 0
  tab %>%
    mutate(
      cenario = case_when(
        n_sinasc >= 1 & n_sih == 0                    ~ "A",
        n_sinasc == 1 & n_sih == 1                     ~ "B",
        n_sinasc == 1 & n_sih > 1                      ~ "F",
        n_sinasc > 1 & n_sih > 1 & n_sinasc == n_sih   ~ "E",
        n_sinasc > n_sih & n_sih > 0                   ~ "C",
        n_sinasc > 1 & n_sinasc < n_sih                ~ "D"
      )
    )
}

resumir_tabela_cenarios <- function(tab_cenarios) {
  t <- tab_cenarios %>%
    group_by(cenario) %>%
    summarise(n_chaves_distintas = n(), n_nascimentos = sum(n_sinasc),
              n_internacoes = sum(n_sih), .groups = "drop") %>%
    arrange(factor(cenario, levels = c("A", "B", "C", "D", "E", "F")))
  bind_rows(t, tibble(cenario = "Total",
                      n_chaves_distintas = sum(t$n_chaves_distintas),
                      n_nascimentos = sum(t$n_nascimentos),
                      n_internacoes = sum(t$n_internacoes)))
}

# -----------------------------------------------------------------------
# 2.3 Resolução determinística -- cenários A, B, F.
# -----------------------------------------------------------------------
resolver_deterministico <- function(sinasc, sih, tab_cenarios, key) {
  sih_resolvido <- sih %>%
    group_by(across(all_of(key))) %>%
    arrange(desc(IDADE > IDADE_CORTE_F), IDADE, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()

  resultado_det <- sinasc %>%
    left_join(tab_cenarios %>% select(all_of(key), cenario), by = key) %>%
    left_join(sih_resolvido, by = key) %>%
    mutate(
      REINT = case_when(
        cenario == "A"                ~ 0,
        cenario %in% c("B", "F")      ~ 1,
        cenario %in% c("C", "D", "E") ~ NA_real_
      ),
      resolvido_por = if_else(cenario %in% c("B", "F"), "deterministico", NA_character_)
    )

  list(
    base_determ    = resultado_det %>% filter(!is.na(REINT)),
    sem_pareamento = resultado_det %>% filter(is.na(REINT))
  )
}

# -----------------------------------------------------------------------
# 2.4 ESCORE DE REGRAS PONDERADAS (não estimado por EM -- pesos fixos)
#
# NOTA METODOLÓGICA (usar no texto, Seção II.B): este é um modelo de
# regras ponderadas (rule-based evidence score), não um modelo de
# Fellegi-Sunter estimado -- os pesos (3,0 / 1,5) e os coeficientes da
# transformação logística (0,619 / 0,458) foram fixados a priori, com
# base em plausibilidade clínica e na literatura de linkage [9], e não
# a partir de frequências m/u estimadas de pares rotulados ou de EM.
#
# Equação exata:
#   P(match) = 1 / (1 + exp(-(0,619 + 0,458 * S)))
#   S = 3,0 * 1[CNES bate]
#     + 1,5 * 1[prematuro E diagnóstico de prematuridade (CID P07*)]
#     + 1,5 * 1[baixo peso E diagnóstico de baixo peso (CID P05*)]
#     + 1,5 * 1[Apgar baixo E diagnóstico grave (CID P22*/P36*/P39*/A41*)]
#
# Se CNES já for campo da própria chave de bloqueio, o termo de CNES é
# removido do escore (ele seria sempre 1 dentro do bloco e não
# discriminaria nada).
# -----------------------------------------------------------------------
criar_calc_prob_par <- function(key) {
  usar_cnes_no_escore <- !("CNES" %in% key)

  function(nasc_row, inter_row, usar_termos_clinicos = TRUE) {
    score <- 0
    if (usar_cnes_no_escore) {
      cnes_match <- !is.na(nasc_row$CNES) && !is.na(inter_row$CNES) && nasc_row$CNES == inter_row$CNES
      if (cnes_match) score <- score + PESO_CNES
    }
    if (usar_termos_clinicos) {
      prematuro          <- !is.na(nasc_row$SEMAGESTAC) && nasc_row$SEMAGESTAC < 37
      diag_prematuridade <- !is.na(inter_row$DIAG_PRINC) && str_starts(inter_row$DIAG_PRINC, "P07")
      baixo_peso <- !is.na(nasc_row$PESO) && nasc_row$PESO < 2500
      diag_peso  <- !is.na(inter_row$DIAG_PRINC) && str_starts(inter_row$DIAG_PRINC, "P05")
      apgar_baixo <- !is.na(nasc_row$APGAR5) && nasc_row$APGAR5 < 7
      diag_grave  <- !is.na(inter_row$DIAG_PRINC) &&
        str_detect(inter_row$DIAG_PRINC, "^(P22|P36|P39|A41)")

      if (prematuro && diag_prematuridade) score <- score + PESO_CLIN
      if (baixo_peso && diag_peso)         score <- score + PESO_CLIN
      if (apgar_baixo && diag_grave)       score <- score + PESO_CLIN
    }
    1 / (1 + exp(-(INTERCEPT_LOGISTICA + INCLINACAO_LOGISTICA * score)))
  }
}

calc_mat_prob <- function(cand_nasc, cand_inter, calc_prob_par, usar_termos_clinicos) {
  n_n <- nrow(cand_nasc); n_i <- nrow(cand_inter)
  mat <- matrix(0.0, nrow = n_n, ncol = n_i)
  for (i in seq_len(n_n)) for (j in seq_len(n_i))
    mat[i, j] <- calc_prob_par(cand_nasc[i, ], cand_inter[j, ], usar_termos_clinicos)
  mat
}

# -----------------------------------------------------------------------
# 2.5 Pareamento guloso (greedy-global):
#  (i)   calcula a matriz de probabilidades entre nascimentos e
#        internações candidatos dentro de cada chave ambígua;
#  (ii)  seleciona o par de maior probabilidade estimada;
#  (iii) se >= cutoff, o par é aceito e ambos são removidos do pool;
#  (iv)  repete até não haver mais pares >= cutoff;
#  (v)   um caso residual de unicidade (1 nascimento e 1 internação
#        remanescentes) é pareado mesmo abaixo do cutoff.
# -----------------------------------------------------------------------
greedy_match <- function(mat_prob, cutoff) {
  n_n <- nrow(mat_prob); n_i <- ncol(mat_prob)
  avail_n <- seq_len(n_n); avail_i <- seq_len(n_i)
  pares <- data.frame(i_nasc = integer(0), j_inter = integer(0), prob = numeric(0), resolvido_por = character(0))
  repeat {
    if (length(avail_n) == 0 || length(avail_i) == 0) break
    sub <- mat_prob[avail_n, avail_i, drop = FALSE]
    max_val <- max(sub)
    if (max_val < cutoff) break
    idx <- which.max(sub)
    i_rel <- ((idx - 1L) %% nrow(sub)) + 1L
    j_rel <- ((idx - 1L) %/% nrow(sub)) + 1L
    i_abs <- avail_n[i_rel]; j_abs <- avail_i[j_rel]
    pares <- rbind(pares, data.frame(i_nasc = i_abs, j_inter = j_abs, prob = max_val, resolvido_por = "evidencia"))
    avail_n <- avail_n[avail_n != i_abs]
    avail_i <- avail_i[avail_i != j_abs]
  }
  if (length(avail_n) == 1 && length(avail_i) == 1) {
    i_abs <- avail_n[1]; j_abs <- avail_i[1]
    pares <- rbind(pares, data.frame(i_nasc = i_abs, j_inter = j_abs,
                                     prob = mat_prob[i_abs, j_abs], resolvido_por = "unicidade_residual"))
  }
  pares
}

# -----------------------------------------------------------------------
# 2.6 Roda o linkage probabilístico completo (C/D/E) para uma chave, um
#     cutoff e uma opção de termos clínicos.
# -----------------------------------------------------------------------
rodar_linkage_probabilistico <- function(sinasc, sih, tab_cenarios, key, cutoff, usar_termos_clinicos = TRUE) {
  calc_prob_par <- criar_calc_prob_par(key)

  sinasc_amb <- sinasc %>%
    inner_join(tab_cenarios %>% filter(cenario %in% c("C", "D", "E")) %>% select(all_of(key), cenario), by = key)
  sih_amb <- sih %>%
    mutate(.id_inter = row_number()) %>%
    semi_join(tab_cenarios %>% filter(cenario %in% c("C", "D", "E")) %>% select(all_of(key)), by = key)

  resultados_prob <- vector("list", 0)
  for (cen in c("C", "D", "E")) {
    chaves_cen <- tab_cenarios %>% filter(cenario == cen) %>% select(all_of(key)) %>% distinct()
    for (g in seq_len(nrow(chaves_cen))) {
      chave_atual <- chaves_cen[g, ]
      cand_nasc   <- sinasc_amb %>% semi_join(chave_atual, by = key)
      cand_inter  <- sih_amb    %>% semi_join(chave_atual, by = key)
      n_n <- nrow(cand_nasc)

      mat_prob <- calc_mat_prob(cand_nasc, cand_inter, calc_prob_par, usar_termos_clinicos)
      pares    <- greedy_match(mat_prob, cutoff = cutoff)

      reint_flag <- logical(n_n); prob_res <- rep(NA_real_, n_n)
      idade_res  <- rep(NA_real_, n_n); cod_id_res <- rep(NA_real_, n_n)
      resolv_res <- rep(NA_character_, n_n)

      if (nrow(pares) > 0) {
        for (k in seq_len(nrow(pares))) {
          i <- pares$i_nasc[k]; j <- pares$j_inter[k]
          reint_flag[i] <- TRUE
          prob_res[i]   <- pares$prob[k]
          idade_res[i]  <- cand_inter$IDADE[j]
          cod_id_res[i] <- cand_inter$COD_IDADE[j]
          resolv_res[i] <- pares$resolvido_por[k]
        }
      }

      resultados_prob[[length(resultados_prob) + 1]] <- tibble(
        .id_nasc = cand_nasc$.id_nasc, REINT_prob = as.numeric(reint_flag),
        prob_match = prob_res, IDADE_prob = idade_res,
        COD_IDADE_prob = cod_id_res, resolvido_por = resolv_res
      )
    }
  }

  resultados_prob <- if (length(resultados_prob) == 0) {
    tibble(.id_nasc = integer(0), REINT_prob = numeric(0), prob_match = numeric(0),
           IDADE_prob = numeric(0), COD_IDADE_prob = numeric(0), resolvido_por = character(0))
  } else bind_rows(resultados_prob)

  list(resultados_prob = resultados_prob, sinasc_amb = sinasc_amb)
}

# -----------------------------------------------------------------------
# 2.7 Combina base determinística + probabilística, aplica as restrições
#     pós-linkage (janela 4-27 dias, gestação única, sem anomalia).
#
# *** CORRIGIDO ***: `resolvido_por` NUNCA é sobrescrito pela regra da
# janela -- ele continua guardando se aquele nascimento chegou a ter
# ALGUM pareamento (determinístico ou probabilístico), mesmo que esse
# pareamento depois seja descartado por estar fora da janela de 4-27
# dias. Isso é o que permite, no resumo final, distinguir
# "nunca teve pareamento" de "foi pareado, mas fora da janela" --
# a mesma lógica de dados já existia, só não estava sendo usada.
# -----------------------------------------------------------------------
montar_base_final <- function(base_determ, sinasc_amb, resultados_prob) {
  sinasc_amb_resolvido <- sinasc_amb %>% left_join(resultados_prob, by = ".id_nasc")

  base_final_completa <- bind_rows(
    base_determ %>%
      transmute(.id_nasc, cenario, GRAVIDEZ, IDANOMAL, SEXO, PARTO,
                PESO, SEMAGESTAC, APGAR5, IDADEMAE, ESCMAE2010, ESTCIVMAE, PARIDADE,
                REINT, IDADE_pareada = IDADE, COD_IDADE_pareado = COD_IDADE, resolvido_por),
    sinasc_amb_resolvido %>%
      transmute(.id_nasc, cenario, GRAVIDEZ, IDANOMAL, SEXO, PARTO,
                PESO, SEMAGESTAC, APGAR5, IDADEMAE, ESCMAE2010, ESTCIVMAE, PARIDADE,
                REINT = REINT_prob, IDADE_pareada = IDADE_prob,
                COD_IDADE_pareado = COD_IDADE_prob, resolvido_por)
  )

  stopifnot("ERRO: .id_nasc duplicado." = !any(duplicated(base_final_completa$.id_nasc)))

  base_janela <- base_final_completa %>%
    mutate(
      # flag auxiliar: este nascimento teve ALGUM pareamento encontrado
      # (determinístico ou probabilístico), independente da janela.
      # Preservado à parte para nunca ser perdido pela recodificação
      # de REINT abaixo.
      teve_pareamento = !is.na(resolvido_por),
      REINT = case_when(
        REINT == 1 & (is.na(COD_IDADE_pareado) | COD_IDADE_pareado != 2) ~ 0,
        REINT == 1 & !(IDADE_pareada %in% JANELA_IDADE)                  ~ 0,
        TRUE ~ REINT
      )
    )

  base_final <- base_janela %>% filter(GRAVIDEZ == 1, IDANOMAL == 2)

  list(
    base_final_completa = base_final_completa,
    base_janela          = base_janela,
    base_final           = base_final,
    n_coorte_final = nrow(base_final),
    n_reint_final  = sum(base_final$REINT == 1, na.rm = TRUE),
    taxa_final     = 100 * mean(base_final$REINT, na.rm = TRUE)
  )
}

# -----------------------------------------------------------------------
# 2.8 *** CORRIGIDO *** Resumo da Tabela III com 3 categorias em vez de 2:
#   - pareados                : REINT == 1 (reinternação válida na janela)
#   - pareado_fora_janela     : teve pareamento, mas não é reinternação
#                                válida (fora da janela / COD_IDADE != 2)
#   - nunca_pareado           : nunca teve nenhuma internação associada
#                                (determinística ou probabilística)
#
# Checagem automática: para B e F, `nunca_pareado` tem que dar 0 -- se
# não der, há um bug em algum outro lugar do pipeline (não deveria mais
# acontecer com esta correção, mas o stopifnot fica como trava de
# segurança para qualquer rodada futura).
# -----------------------------------------------------------------------
resumir_tabela_linkage <- function(base_janela_restrita) {
  tipo_linkage <- c(A = "determinístico", B = "determinístico", C = "probabilístico por chave",
                    D = "probabilístico por chave", E = "probabilístico por chave", F = "determinístico")

  t <- base_janela_restrita %>%
    group_by(cenario) %>%
    summarise(
      pareados            = sum(REINT == 1, na.rm = TRUE),
      pareado_fora_janela = sum(REINT == 0 & teve_pareamento, na.rm = TRUE),
      nunca_pareado       = sum(REINT == 0 & !teve_pareamento, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      nao_pareados = pareado_fora_janela + nunca_pareado,  # mantido para compatibilidade
      tipo_linkage = tipo_linkage[cenario]
    ) %>%
    select(cenario, tipo_linkage, pareados, pareado_fora_janela, nunca_pareado, nao_pareados) %>%
    arrange(factor(cenario, levels = c("A", "B", "C", "D", "E", "F")))

  # trava de segurança: em B e F, "nunca_pareado" tem que ser sempre 0
  bf_check <- t %>% filter(cenario %in% c("B", "F"), nunca_pareado != 0)
  if (nrow(bf_check) > 0) {
    warning("ATENÇÃO: cenário determinístico com nunca_pareado != 0 -- investigar antes de publicar a tabela.")
    print(bf_check)
  }

  bind_rows(t, tibble(cenario = "Total", tipo_linkage = "-",
                      pareados = sum(t$pareados),
                      pareado_fora_janela = sum(t$pareado_fora_janela),
                      nunca_pareado = sum(t$nunca_pareado),
                      nao_pareados = sum(t$nao_pareados)))
}

# -----------------------------------------------------------------------
# 2.9 *** NOVO *** Monta a Tabela III completa (formato de 5 colunas)
#     cruzando com a Tabela II (nascimentos por cenário) e mostrando
#     quantos foram excluídos por gestação múltipla/anomalia congênita.
# -----------------------------------------------------------------------
gerar_tabela_III_completa <- function(tabela_cenarios, base_final_completa, base_janela) {
  nascimentos_tab_ii <- tabela_cenarios %>%
    filter(cenario != "Total") %>%
    select(cenario, nascimentos_tabela_ii = n_nascimentos)

  # excluídos = estava em base_final_completa (== Tabela II) mas caiu
  # fora depois do filtro GRAVIDEZ==1 & IDANOMAL==2
  excluidos <- base_final_completa %>%
    group_by(cenario) %>%
    summarise(n_total = n(), .groups = "drop") %>%
    left_join(
      base_janela %>% filter(GRAVIDEZ == 1, IDANOMAL == 2) %>%
        group_by(cenario) %>% summarise(n_elegivel = n(), .groups = "drop"),
      by = "cenario"
    ) %>%
    mutate(n_elegivel = coalesce(n_elegivel, 0L),
           excluidos = n_total - n_elegivel) %>%
    select(cenario, excluidos, elegiveis = n_elegivel)

  resumo_janela <- resumir_tabela_linkage(base_janela %>% filter(GRAVIDEZ == 1, IDANOMAL == 2)) %>%
    filter(cenario != "Total") %>%
    select(cenario, reinternacao_identificada = pareados,
           pareado_fora_janela, nunca_pareado)

  t <- nascimentos_tab_ii %>%
    left_join(excluidos, by = "cenario") %>%
    left_join(resumo_janela, by = "cenario") %>%
    arrange(factor(cenario, levels = c("A", "B", "C", "D", "E", "F")))

  bind_rows(
    t,
    tibble(cenario = "Total",
           nascimentos_tabela_ii = sum(t$nascimentos_tabela_ii),
           excluidos = sum(t$excluidos),
           elegiveis = sum(t$elegiveis),
           reinternacao_identificada = sum(t$reinternacao_identificada),
           pareado_fora_janela = sum(t$pareado_fora_janela),
           nunca_pareado = sum(t$nunca_pareado))
  )
}

# -----------------------------------------------------------------------
# 2.10 Função-mestra: roda o pipeline completo para uma chave, um cutoff
#     e uma opção de termos clínicos.
# -----------------------------------------------------------------------
rodar_pipeline <- function(key, rotulo, cutoff = CUTOFF_OFICIAL, usar_termos_clinicos = TRUE, verbose = TRUE) {
  if (verbose) cat(sprintf("\n\n### RODADA: %s | cutoff=%.2f | termos clínicos=%s ###\n",
                           rotulo, cutoff, usar_termos_clinicos))

  prep <- preparar_bases(key, verbose = verbose)
  tab_cenarios <- classificar_cenarios(prep$sinasc, prep$sih, key)
  det <- resolver_deterministico(prep$sinasc, prep$sih, tab_cenarios, key)
  prob <- rodar_linkage_probabilistico(prep$sinasc, prep$sih, tab_cenarios, key, cutoff, usar_termos_clinicos)
  final <- montar_base_final(det$base_determ, prob$sinasc_amb, prob$resultados_prob)

  if (verbose) {
    cat(sprintf("n final da coorte: %d | reinternações: %d | taxa: %.4f%%\n",
                final$n_coorte_final, final$n_reint_final, final$taxa_final))
  }

  # Reprodução exata da Tabela II: pareados/não-pareados ANTES da regra
  # da janela (REINT em base_final_completa, sem nenhuma restrição
  # pós-linkage aplicada ainda). Usada só para checagem de consistência.
  tabela_ii_pareamento <- final$base_final_completa %>%
    group_by(cenario) %>%
    summarise(pareados_tab_ii = sum(REINT == 1, na.rm = TRUE),
              nao_pareados_tab_ii = sum(REINT == 0, na.rm = TRUE), .groups = "drop")

  list(
    rotulo = rotulo, key = key, cutoff = cutoff, usar_termos_clinicos = usar_termos_clinicos,
    n_sinasc = nrow(prep$sinasc), n_sih_elegivel = nrow(prep$sih),
    tabela_cenarios = resumir_tabela_cenarios(tab_cenarios),
    tabela_ii_pareamento = tabela_ii_pareamento,
    tabela_depois = resumir_tabela_linkage(final$base_janela %>% filter(GRAVIDEZ == 1, IDANOMAL == 2)),
    tabela_iii_completa = gerar_tabela_III_completa(resumir_tabela_cenarios(tab_cenarios),
                                                    final$base_final_completa, final$base_janela),
    base_final = final$base_final,
    n_coorte_final = final$n_coorte_final, n_reint_final = final$n_reint_final,
    taxa_final = final$taxa_final
  )
}


# #############################################################################
# PARTE 3 -- RODADA OFICIAL (chave do artigo, cutoff = 0,70)
# #############################################################################
cat(sprintf("[%s] ETAPA 3: Rodada Oficial (Chave do Artigo)\n", Sys.time()))

resultado_oficial <- rodar_pipeline(key_municipio, "Chave do artigo (município) -- oficial",
                                    cutoff = CUTOFF_OFICIAL, usar_termos_clinicos = TRUE)

cat("\n========== TABELA DE CENÁRIOS (oficial) ==========\n")
print(resultado_oficial$tabela_cenarios)

cat("\n========== TABELA III -- resumo pareados/não pareados (oficial, CORRIGIDA) ==========\n")
print(resultado_oficial$tabela_depois)

cat("\n========== TABELA III -- versão completa (5 colunas, pronta para o artigo) ==========\n")
print(resultado_oficial$tabela_iii_completa, n = Inf)

resumo_mecanismo <- resultado_oficial$base_final %>%
  filter(REINT == 1) %>%
  count(resolvido_por, name = "n_reinternacoes") %>%
  mutate(pct = round(100 * n_reinternacoes / sum(n_reinternacoes), 1))
cat("\n---- Reinternações por mecanismo de resolução (oficial) ----\n")
print(as.data.frame(resumo_mecanismo))

# Equação a citar no texto (Seção II.B) -- ver nota completa na Parte 2.4:
# P(match) = 1 / (1 + exp(-(0,619 + 0,458 * S))), S = 3,0*CNES + 1,5*(prem+peso+apgar)


# #############################################################################
# PARTE 4 -- ANÁLISE DE SENSIBILIDADE (cutoff x termos clínicos)
# #############################################################################
cat(sprintf("\n[%s] ETAPA 4: Análise de Sensibilidade (Cutoff x Termos Clínicos)\n", Sys.time()))

prep_oficial <- preparar_bases(key_municipio, verbose = FALSE)
tab_cenarios_oficial <- classificar_cenarios(prep_oficial$sinasc, prep_oficial$sih, key_municipio)
det_oficial <- resolver_deterministico(prep_oficial$sinasc, prep_oficial$sih, tab_cenarios_oficial, key_municipio)

grid_sensibilidade <- expand.grid(
  cutoff = c(0.60, 0.65, 0.70, 0.75, 0.80),
  usar_termos_clinicos = c(TRUE, FALSE)
)

calcular_linha_sensibilidade <- function(cutoff, usar_termos_clinicos) {
  prob <- rodar_linkage_probabilistico(prep_oficial$sinasc, prep_oficial$sih, tab_cenarios_oficial,
                                       key_municipio, cutoff, usar_termos_clinicos)
  final <- montar_base_final(det_oficial$base_determ, prob$sinasc_amb, prob$resultados_prob)
  tibble(cutoff = cutoff, usar_termos_clinicos = usar_termos_clinicos,
         n_coorte = final$n_coorte_final, n_reinternacoes = final$n_reint_final,
         taxa_pct = final$taxa_final)
}

tabela_sensibilidade <- bind_rows(lapply(seq_len(nrow(grid_sensibilidade)), function(i) {
  calcular_linha_sensibilidade(grid_sensibilidade$cutoff[i], grid_sensibilidade$usar_termos_clinicos[i])
})) %>% arrange(usar_termos_clinicos, cutoff)

cat("\n========== TABELA V -- SENSIBILIDADE (cutoff x termos clínicos) ==========\n")
print(tabela_sensibilidade, n = Inf)
cat(sprintf("Faixa de variação da taxa em toda a grade: %.4f%% a %.4f%%\n",
            min(tabela_sensibilidade$taxa_pct), max(tabela_sensibilidade$taxa_pct)))


# #############################################################################
# PARTE 5 -- COMPARAÇÃO DE CHAVES (município x CNES)
# #############################################################################
cat(sprintf("\n[%s] ETAPA 5: Comparação de Chaves de Bloqueio\n", Sys.time()))

resultado_cnes <- rodar_pipeline(key_cnes, "Chave alternativa (CNES)",
                                 cutoff = CUTOFF_OFICIAL, usar_termos_clinicos = TRUE)

tabela_comparacao_chaves <- tibble(
  chave              = c(resultado_oficial$rotulo, resultado_cnes$rotulo),
  campos             = c(paste(key_municipio, collapse = "/"), paste(key_cnes, collapse = "/")),
  n_internacoes_eleg = c(resultado_oficial$n_sih_elegivel, resultado_cnes$n_sih_elegivel),
  n_coorte_final     = c(resultado_oficial$n_coorte_final, resultado_cnes$n_coorte_final),
  n_reinternacoes    = c(resultado_oficial$n_reint_final, resultado_cnes$n_reint_final),
  taxa_pct           = c(resultado_oficial$taxa_final, resultado_cnes$taxa_final)
)

cat("\n========== COMPARAÇÃO ENTRE CHAVES DE BLOQUEIO ==========\n")
print(tabela_comparacao_chaves)
cat(sprintf("Razão entre as taxas (município / CNES): %.2fx\n",
            resultado_oficial$taxa_final / resultado_cnes$taxa_final))


# #############################################################################
# PARTE 6 -- REGRESSÃO PRINCIPAL (Tabela IV) + UNIVARIADA BRUTA
# Usa a base final da RODADA OFICIAL (chave de município, cutoff 0,70)
# #############################################################################
cat(sprintf("\n[%s] ETAPA 6: Análise de Regressão (Univariada Bruta e Multivariada)\n", Sys.time()))

categorizar_variaveis <- function(df) {
  df %>%
    mutate(
      IDADEMAE_cat = case_when(
        IDADEMAE < 20                   ~ "< 20 (adolescente)",
        IDADEMAE >= 20 & IDADEMAE <= 34  ~ "20-34",
        IDADEMAE >= 35                   ~ "35+ (idade avançada)",
        TRUE ~ NA_character_),
      IDADEMAE_cat = relevel(factor(IDADEMAE_cat), ref = "20-34"),

      SEMAGESTAC_cat = case_when(
        SEMAGESTAC < 37  ~ "Pré-termo (<37 sem)",
        SEMAGESTAC >= 37 ~ "Termo/pós-termo (>=37 sem)",
        TRUE ~ NA_character_),
      SEMAGESTAC_cat = relevel(factor(SEMAGESTAC_cat), ref = "Termo/pós-termo (>=37 sem)"),

      APGAR5_cat = case_when(
        APGAR5 < 7  ~ "Baixo (<7)",
        APGAR5 >= 7 ~ "Normal (>=7)",
        TRUE ~ NA_character_),
      APGAR5_cat = relevel(factor(APGAR5_cat), ref = "Normal (>=7)"),

      PESO_cat = case_when(
        PESO < 2500  ~ "Baixo peso (<2500g)",
        PESO >= 2500 ~ "Peso normal (>=2500g)",
        TRUE ~ NA_character_),
      PESO_cat = relevel(factor(PESO_cat), ref = "Peso normal (>=2500g)"),

      ESCMAE2010_cat = case_when(
        ESCMAE2010 %in% c(1, 2, 3) ~ "< 8 anos (baixa escolaridade)",
        ESCMAE2010 %in% c(4, 5)    ~ "8 anos ou mais",
        TRUE ~ NA_character_),
      ESCMAE2010_cat = relevel(factor(ESCMAE2010_cat), ref = "8 anos ou mais"),

      ESTCIVMAE_cat = case_when(
        ESTCIVMAE == 2               ~ "Casada",
        ESTCIVMAE %in% c(1, 3, 4, 5) ~ "Não casada",
        TRUE ~ NA_character_),
      ESTCIVMAE_cat = relevel(factor(ESTCIVMAE_cat), ref = "Casada"),

      PARTO_cat = case_when(
        PARTO == 1 ~ "Vaginal",
        PARTO == 2 ~ "Cesáreo",
        TRUE ~ NA_character_),
      PARTO_cat = relevel(factor(PARTO_cat), ref = "Vaginal")
    )
}

base_final_oficial <- resultado_oficial$base_final

# -- Univariada bruta (A + só os pareados de B/C/D/E/F) -----------------
base_sensib_reg <- base_final_oficial %>% filter(cenario == "A" | REINT == 1) %>% categorizar_variaveis()

variaveis_univariadas <- c("PESO_cat", "SEMAGESTAC_cat", "PARTO_cat",
                           "APGAR5_cat", "IDADEMAE_cat", "ESCMAE2010_cat", "ESTCIVMAE_cat")

tabela_or_bruta <- bind_rows(lapply(variaveis_univariadas, function(v) {
  modelo_v <- glm(as.formula(paste("REINT ~", v)), data = base_sensib_reg, family = binomial())
  broom::tidy(modelo_v, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>% mutate(variavel = v)
})) %>%
  mutate(OR_IC95 = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high)) %>%
  select(variavel, term, OR_IC95, p.value) %>% arrange(p.value)

cat("\n========== ODDS RATIO BRUTA (univariada) ==========\n")
print(tabela_or_bruta, n = Inf)

# -- Multivariada ponderada (Tabela IV) ----------------------------------
base_regressao <- base_final_oficial %>%
  group_by(cenario) %>%
  mutate(
    n_total_cenario = n(),
    n_resolvidos    = sum(!is.na(resolvido_por)),
    peso_regressao  = if_else(cenario %in% c("A", "B", "F"), 1, n_total_cenario / n_resolvidos)
  ) %>%
  ungroup() %>%
  categorizar_variaveis() %>%
  mutate(ESTCIVMAE = na_if(ESTCIVMAE, 9))

modelo_completo <- suppressWarnings(glm(
  REINT ~ PESO_cat + SEMAGESTAC_cat + PARTO_cat + APGAR5_cat + IDADEMAE_cat + ESCMAE2010_cat + ESTCIVMAE_cat,
  data = base_regressao, family = binomial(), weights = peso_regressao
))

tabela_or_multivariada <- broom::tidy(modelo_completo, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(OR_IC95 = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high)) %>%
  select(term, OR_IC95, p.value) %>% arrange(p.value)

cat("\n========== TABELA IV -- MODELO MULTIVARIADO PONDERADO ==========\n")
print(tabela_or_multivariada, n = Inf)


# #############################################################################
# PARTE 7 -- REGRESSÃO ESTRATIFICADA POR MECANISMO DE RESOLUÇÃO
# (determinístico B/F  vs.  probabilístico C/D/E)
#
# Objetivo (Revisor 2): prematuridade, baixo peso e Apgar baixo entram no
# próprio escore de linkage. Rodar o modelo separadamente nos dois
# subgrupos mostra se o efeito de PARTO_cat (que NÃO entra no escore) se
# mantém em ambos -- reforçando sua credibilidade -- e se PESO/
# SEMAGESTAC/APGAR aparecem infladas no subgrupo probabilístico, o que
# seria evidência de circularidade a declarar como limitação.
# #############################################################################
cat(sprintf("\n[%s] ETAPA 7: Análise Estratificada por Mecanismo de Resolução\n", Sys.time()))

base_estrat <- base_final_oficial %>%
  filter(cenario == "A" | REINT == 1) %>%
  categorizar_variaveis() %>%
  mutate(
    subgrupo_resolucao = case_when(
      cenario == "A"                ~ "referencia_sem_internacao",
      cenario %in% c("B", "F")      ~ "deterministico",
      cenario %in% c("C", "D", "E") ~ "probabilistico"
    )
  )

rodar_modelo_estratificado <- function(base, rotulo) {
  n_casos <- nrow(base); n_reint <- sum(base$REINT == 1, na.rm = TRUE)
  cat(sprintf("\n--- Subgrupo: %s (n = %d, reinternações = %d) ---\n", rotulo, n_casos, n_reint))
  if (n_reint < 10) cat("Aviso: poucos eventos neste subgrupo; estimativas podem ser instáveis.\n")

  modelo <- tryCatch(
    glm(REINT ~ PESO_cat + SEMAGESTAC_cat + PARTO_cat + APGAR5_cat, data = base, family = binomial()),
    error = function(e) NULL
  )
  if (is.null(modelo)) { cat("Modelo não convergiu neste subgrupo.\n"); return(NULL) }

  broom::tidy(modelo, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(OR_IC95 = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high), subgrupo = rotulo) %>%
    select(subgrupo, term, OR_IC95, p.value)
}

base_vs_determ <- base_estrat %>% filter(subgrupo_resolucao %in% c("referencia_sem_internacao", "deterministico"))
base_vs_prob   <- base_estrat %>% filter(subgrupo_resolucao %in% c("referencia_sem_internacao", "probabilistico"))

or_deterministico <- rodar_modelo_estratificado(base_vs_determ, "Determinístico (B/F) vs. sem internação (A)")
or_probabilistico <- rodar_modelo_estratificado(base_vs_prob,   "Probabilístico (C/D/E) vs. sem internação (A)")

tabela_or_estratificada <- bind_rows(or_deterministico, or_probabilistico)

cat("\n========== TABELA OR ESTRATIFICADA POR MECANISMO DE RESOLUÇÃO ==========\n")
print(tabela_or_estratificada, n = Inf)

cat("\nInterpretação sugerida para o texto:\n")
cat("- Se PARTO_cat (Cesáreo) mostrar OR < 1 e significativo em AMBOS os subgrupos,\n")
cat("  o achado é robusto ao mecanismo de resolução (parto não entra no escore de linkage).\n")
cat("- Se PESO_cat/SEMAGESTAC_cat/APGAR5_cat mostrarem OR muito maior no subgrupo\n")
cat("  probabilístico do que no determinístico, é evidência de circularidade\n")
cat("  (essas variáveis ajudaram a formar o próprio pareamento nesse subgrupo)\n")
cat("  e deve ser declarado como limitação no texto.\n")


# #############################################################################
# PARTE 8 -- CHECAGENS FINAIS DE CONSISTÊNCIA (EXPANDIDA - inspirado Labsus)
# #############################################################################
cat(sprintf("\n[%s] ETAPA 8: Checagens Finais de Consistência\n", Sys.time()))
cat("\n========== CHECAGENS DE CONSISTÊNCIA EXPANDIDAS ==========\n")

cat(sprintf("✓ Internações elegíveis (usar em TODO o texto): %d\n", resultado_oficial$n_sih_elegivel))
cat(sprintf("✓ Denominador Tabela I/II (nascimentos): %d\n", resultado_oficial$n_sinasc))
cat(sprintf("✓ Denominador Tabela III (coorte final restrita): %d\n", resultado_oficial$n_coorte_final))
cat(sprintf("✓ Taxa oficial (chave município, regra ponderada, cutoff 0,70): %.4f%%\n", resultado_oficial$taxa_final))

# Checagem 1: Nenhum valor NA inesperado
n_na_reint <- sum(is.na(resultado_oficial$base_final$REINT))
if (n_na_reint > 0) {
  cat(sprintf("⚠ AVISO: %d valores NA em REINT encontrados!\n", n_na_reint))
} else {
  cat("✓ Checagem 1 PASSOU: Sem NAs em REINT\n")
}

# Checagem 2: Distribuição de cenários é esperada
cenarios <- resultado_oficial$base_final %>%
  count(cenario) %>%
  mutate(pct = 100 * n / sum(n))
cat("\nDistribuição de cenários:\n")
print(cenarios)
if (sum(cenarios$n[cenarios$cenario %in% c("B", "F")]) == 0) {
  cat("⚠ AVISO: Nenhum pareamento determinístico (B/F) encontrado\n")
}

# Checagem 3: Taxa de reinternação é biologicamente plausível
taxa <- resultado_oficial$taxa_final
if (taxa < 5 || taxa > 30) {
  cat(sprintf("⚠ AVISO: Taxa de reinternação %.2f%% parece atípica para neonatos\n", taxa))
} else {
  cat(sprintf("✓ Checagem 3 PASSOU: Taxa %.2f%% dentro do intervalo esperado (5-30%%)\n", taxa))
}

# Checagem 4: Proporção sexo não é enviesada
prop_sexo <- resultado_oficial$base_final %>%
  filter(REINT == 1) %>%
  count(SEXO) %>%
  mutate(pct = 100 * n / sum(n))
cat("\nProporção sexo entre reinternações:\n")
print(prop_sexo)
if (any(prop_sexo$pct < 40 | prop_sexo$pct > 60)) {
  cat("⚠ AVISO: Proporção sexo desequilibrada (esperado ~50%)\n")
} else {
  cat("✓ Checagem 4 PASSOU: Proporção sexo equilibrada\n")
}

# Checagem 5: nunca_pareado (Tab. III) <= não pareados (Tab. II)
cat("\n---- Checagem: nunca_pareado (Tab. III) <= não pareados (Tab. II) em cada cenário ----\n")
checagem_monotonicidade <- resultado_oficial$tabela_depois %>%
  filter(cenario != "Total") %>%
  select(cenario, nunca_pareado) %>%
  left_join(resultado_oficial$tabela_ii_pareamento, by = "cenario") %>%
  mutate(ok = nunca_pareado <= nao_pareados_tab_ii)
print(checagem_monotonicidade)
if (any(!checagem_monotonicidade$ok, na.rm = TRUE)) {
  warning("ATENÇÃO: encontrado cenário onde nunca_pareado da Tabela III excede o da Tabela II -- investigar.")
} else {
  cat("✓ Checagem 5 PASSOU: Nenhuma inconsistência encontrada.\n")
}

# Checagem 6: Confirmação dos números da janela 4-27
sih_4_27 <- sih_t2 %>% filter(IDADE %in% JANELA_IDADE, COD_IDADE == 2)
cat(sprintf("\n✓ Checagem 6: Total de internações 4-27 dias (bruto): %d\n", nrow(sih_4_27)))

sih_4_27_mun <- sih_4_27 %>% semi_join(sinasc_t1, by = key_municipio)
nascimentos_elegiveis <- sinasc_t1 %>% filter(GRAVIDEZ == 1, IDANOMAL == 2)
sih_4_27_restrita <- sih_4_27_mun %>% semi_join(nascimentos_elegiveis, by = key_municipio)

n_xxx <- nrow(sih_4_27_restrita)
pct_xxx <- 100 * n_xxx / resultado_oficial$n_coorte_final
cat(sprintf("✓ Checagem 6 (cont): Internações 4-27 dias (restrito): %d | %% sobre coorte final: %.2f%%\n", n_xxx, pct_xxx))

cat("\n✓ TODAS AS CHECAGENS CONCLUÍDAS\n")


# #############################################################################
# PARTE 9 -- NOVO: ANÁLISE DE ROBUSTEZ POR SUBGRUPOS CLÍNICOS
# (inspirada em Labsus -- Ideia #7)
# #############################################################################
cat(sprintf("\n[%s] ETAPA 9: Análise de Robustez por Subgrupos Clínicos\n", Sys.time()))
cat("\n========== ANÁLISE DE ROBUSTEZ POR SUBGRUPOS CLÍNICOS ==========\n")

# Subgrupo 1: Por Apgar
analise_apgar <- resultado_oficial$base_final %>%
  filter(REINT == 1) %>%
  mutate(apgar_cat = case_when(
    APGAR5 < 7 ~ "Baixo (<7)",
    APGAR5 >= 7 & APGAR5 <= 8 ~ "Moderado (7-8)",
    APGAR5 > 8 ~ "Normal (>8)",
    TRUE ~ "Faltante"
  )) %>%
  group_by(apgar_cat) %>%
  summarise(n = n(), taxa = 100 * n / nrow(resultado_oficial$base_final), .groups = "drop") %>%
  arrange(desc(n))

cat("\nReinternações por categoria Apgar:\n")
print(analise_apgar)

# Subgrupo 2: Por Peso
analise_peso <- resultado_oficial$base_final %>%
  filter(REINT == 1) %>%
  mutate(peso_cat = case_when(
    PESO < 1500 ~ "Extremamente baixo (<1500g)",
    PESO >= 1500 & PESO < 2500 ~ "Muito baixo (1500-2500g)",
    PESO >= 2500 ~ "Normal (≥2500g)",
    TRUE ~ "Faltante"
  )) %>%
  group_by(peso_cat) %>%
  summarise(n = n(), taxa = 100 * n / nrow(resultado_oficial$base_final), .groups = "drop") %>%
  arrange(desc(n))

cat("\nReinternações por categoria Peso:\n")
print(analise_peso)

# Subgrupo 3: Por Paridade
analise_paridade <- resultado_oficial$base_final %>%
  filter(REINT == 1) %>%
  mutate(paridade_cat = case_when(
    PARIDADE == 1 ~ "Primípara",
    PARIDADE >= 2 & PARIDADE <= 4 ~ "Multípara (2-4)",
    PARIDADE >= 5 ~ "Grande multípara (≥5)",
    TRUE ~ "Faltante"
  )) %>%
  group_by(paridade_cat) %>%
  summarise(n = n(), taxa = 100 * n / nrow(resultado_oficial$base_final), .groups = "drop") %>%
  arrange(desc(n))

cat("\nReinternações por Paridade:\n")
print(analise_paridade)

cat("\n✓ ANÁLISE DE SUBGRUPOS CLÍNICOS CONCLUÍDA\n")


# #############################################################################
# PARTE 10 -- NOVO: PSEUDO-ID PARA VALIDAÇÃO CRUZADA
# (inspirada em Labsus -- Ideia #1)
# #############################################################################
cat(sprintf("\n[%s] ETAPA 10: Validação Cruzada com Pseudo-ID\n", Sys.time()))
cat("\n========== VALIDAÇÃO CRUZADA COM PSEUDO-ID SIMPLES ==========\n")

# Criar pseudo-ID simples para validação
sih_bruto_prep <- sih_bruto %>%
  mutate(
    DTNASC     = as.Date(as.character(NASC),     format = "%Y%m%d"),
    SEXO       = as.integer(SEXO),
    RACACOR    = as.integer(RACA_COR),
    CODMUNRES  = as.integer(MUNIC_RES),
    DT_INTER   = as.Date(as.character(DT_INTER), format = "%Y%m%d"),
    DT_SAIDA   = as.Date(as.character(DT_SAIDA), format = "%Y%m%d")
  ) %>%
  filter(NACIONAL == VALOR_NACIONAL_BRASILEIRA)

sih_validacao <- sih_bruto_prep %>%
  mutate(
    pseudo_id = paste0(
      as.character(DTNASC), "_",
      SEXO, "_",
      RACACOR, "_",
      CODMUNRES
    )
  ) %>%
  group_by(pseudo_id) %>%
  arrange(DT_INTER, .by_group = TRUE) %>%
  mutate(
    proxima_inter = lead(DT_INTER),
    dias_ate_readmissao = as.numeric(proxima_inter - DT_SAIDA)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(dias_ate_readmissao),
    DTNASC >= as.Date("2024-01-01"),
    DTNASC < as.Date("2024-12-01")
  )

n_pseudo_reint_4_27 <- sih_validacao %>%
  filter(dias_ate_readmissao >= 4, dias_ate_readmissao <= 27) %>%
  nrow()

taxa_pseudo_reint <- 100 * n_pseudo_reint_4_27 / nrow(sih_bruto_prep)

validacao_cruzada <- tibble(
  metodo = c("Seu Linkage Probabilístico", "Pseudo-ID Simples (Validação)"),
  n_reinternacoes = c(
    resultado_oficial$n_reint_final,
    n_pseudo_reint_4_27
  ),
  taxa_pct = c(
    resultado_oficial$taxa_final,
    taxa_pseudo_reint
  )
)

cat("\nComparação entre métodos:\n")
print(validacao_cruzada)
cat("\nInterpretação: O método pseudo-ID simples serve como validação de robustez.\n")
cat("Se as taxas forem similares, o resultado é mais robusto.\n")


# #############################################################################
# PARTE 11 -- NOVO: EXPORTAÇÃO ESTRUTURADA DE RESULTADOS
# (inspirada em Labsus -- Ideia #6)
# #############################################################################
cat(sprintf("\n[%s] ETAPA 11: Exportação Estruturada de Resultados\n", Sys.time()))
cat("\n========== EXPORTANDO RESULTADOS PARA REUTILIZAÇÃO ==========\n")

# Salve a base final linkada
write.csv(
  resultado_oficial$base_final,
  file.path(DIR_RESULTADOS, "01_base_final_linkada.csv"),
  row.names = FALSE
)
cat(sprintf("✓ Exportado: 01_base_final_linkada.csv (%d linhas)\n", nrow(resultado_oficial$base_final)))

# Salve as tabelas de cenários
write.csv(
  resultado_oficial$tabela_cenarios,
  file.path(DIR_RESULTADOS, "02_tabela_cenarios.csv"),
  row.names = FALSE
)
cat("✓ Exportado: 02_tabela_cenarios.csv\n")

# Salve a tabela III completa
write.csv(
  resultado_oficial$tabela_iii_completa,
  file.path(DIR_RESULTADOS, "03_tabela_iii_completa.csv"),
  row.names = FALSE
)
cat("✓ Exportado: 03_tabela_iii_completa.csv\n")

# Salve a análise de sensibilidade
write.csv(
  tabela_sensibilidade,
  file.path(DIR_RESULTADOS, "04_sensibilidade_cutoff_termos_clinicos.csv"),
  row.names = FALSE
)
cat("✓ Exportado: 04_sensibilidade_cutoff_termos_clinicos.csv\n")

# Salve a comparação de chaves
write.csv(
  tabela_comparacao_chaves,
  file.path(DIR_RESULTADOS, "05_comparacao_chaves.csv"),
  row.names = FALSE
)
cat("✓ Exportado: 05_comparacao_chaves.csv\n")

# Salve as tabelas de regressão
write.csv(
  tabela_or_bruta,
  file.path(DIR_RESULTADOS, "06_odds_ratio_bruta_univariada.csv"),
  row.names = FALSE
)
cat("✓ Exportado: 06_odds_ratio_bruta_univariada.csv\n")

write.csv(
  tabela_or_multivariada,
  file.path(DIR_RESULTADOS, "07_odds_ratio_multivariada_ponderada.csv"),
  row.names = FALSE
)
cat("✓ Exportado: 07_odds_ratio_multivariada_ponderada.csv\n")

# Salve análise de subgrupos clínicos
analise_subgrupos <- bind_rows(
  analise_apgar %>% mutate(tipo = "Apgar"),
  analise_peso %>% mutate(tipo = "Peso"),
  analise_paridade %>% mutate(tipo = "Paridade")
)
write.csv(
  analise_subgrupos,
  file.path(DIR_RESULTADOS, "08_analise_subgrupos_clinicos.csv"),
  row.names = FALSE
)
cat("✓ Exportado: 08_analise_subgrupos_clinicos.csv\n")

# Salve validação cruzada com pseudo-ID
write.csv(
  validacao_cruzada,
  file.path(DIR_RESULTADOS, "09_validacao_cruzada_pseudo_id.csv"),
  row.names = FALSE
)
cat("✓ Exportado: 09_validacao_cruzada_pseudo_id.csv\n")

# Salve resumo executivo
resumo_exec <- tibble(
  Metrica = c(
    "Nascimentos SINASC elegíveis",
    "Internações SIH elegíveis",
    "Coorte final (após restrições)",
    "Reinternações (REINT=1)",
    "Taxa de reinternação (%)",
    "Cutoff probabilístico utilizado",
    "Termos clínicos utilizados",
    "Método de pareamento",
    "Data de execução"
  ),
  Valor = c(
    resultado_oficial$n_sinasc,
    resultado_oficial$n_sih_elegivel,
    resultado_oficial$n_coorte_final,
    resultado_oficial$n_reint_final,
    sprintf("%.2f", resultado_oficial$taxa_final),
    CUTOFF_OFICIAL,
    "SIM",
    "Determinístico + Probabilístico (Escore Ponderado)",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
)

write.csv(
  resumo_exec,
  file.path(DIR_RESULTADOS, "00_RESUMO_EXECUTIVO.csv"),
  row.names = FALSE
)
cat("✓ Exportado: 00_RESUMO_EXECUTIVO.csv\n")

cat(sprintf("\n✓ Todos os resultados exportados para: %s\n", DIR_RESULTADOS))
cat("Arquivos gerados:\n")
arquivos <- list.files(DIR_RESULTADOS)
walk(arquivos, ~cat(sprintf("  - %s\n", .)))

# Restaurar saída padrão e finalizar logging
cat(sprintf("\n[%s] ========== PIPELINE FINALIZADO COM SUCESSO ==========\n", Sys.time()))
cat(sprintf("[%s] Log completo salvo em: %s\n\n", Sys.time(), log_file))

sink()  # Restaura saída padrão

cat("\n✅ PIPELINE EXECUTADO COM SUCESSO\n")
cat(sprintf("Log salvo em: %s\n", log_file))
cat(sprintf("Resultados em: %s\n", DIR_RESULTADOS))
