library(dplyr)

base <- read.csv("resultados_linkage_cbeb/01_base_final_linkada.csv")

cat("\n================================================================================\n")
cat("ANÁLISE DESCRITIVA - 01_base_final_linkada.csv\n")
cat("================================================================================\n\n")

cat(sprintf("Total: %d registros\n\n", nrow(base)))

cat("═════════════════════════════════════════════════════════\n")
cat("1. REINTERNAÇÃO (REINT)\n")
cat("═════════════════════════════════════════════════════════\n\n")
print(base %>% group_by(REINT) %>% summarise(n = n(), pct = round(100*n()/nrow(base), 2)))
cat("\n")

cat("═════════════════════════════════════════════════════════\n")
cat("2. CENÁRIOS DE PAREAMENTO\n")
cat("═════════════════════════════════════════════════════════\n\n")
cenario_dist <- base %>%
  group_by(cenario) %>%
  summarise(
    n = n(),
    pct = round(100*n()/nrow(base), 2),
    reint_taxa_pct = round(100*mean(REINT, na.rm=T), 2)
  )
print(cenario_dist)
cat("\n")

cat("═════════════════════════════════════════════════════════\n")
cat("3. IDADE NA REINTERNAÇÃO (dias de vida)\n")
cat("═════════════════════════════════════════════════════════\n\n")
age_stats <- base %>%
  filter(!is.na(IDADE_pareada)) %>%
  summarise(
    total_reint = n(),
    media = round(mean(IDADE_pareada, na.rm=T), 1),
    mediana = median(IDADE_pareada, na.rm=T),
    min = min(IDADE_pareada),
    max = max(IDADE_pareada),
    sd = round(sd(IDADE_pareada, na.rm=T), 2)
  )
print(age_stats)

cat("\nDistribuição por faixas etárias:\n")
age_faixas <- base %>%
  filter(!is.na(IDADE_pareada)) %>%
  mutate(faixa = case_when(
    IDADE_pareada <= 7 ~ "4-7 dias",
    IDADE_pareada <= 14 ~ "8-14 dias",
    IDADE_pareada <= 21 ~ "15-21 dias",
    TRUE ~ "22-27 dias"
  )) %>%
  group_by(faixa) %>%
  summarise(n = n(), pct = round(100*n()/323, 2))
print(age_faixas)
cat("\n")

cat("═════════════════════════════════════════════════════════\n")
cat("4. VARIÁVEIS CLÍNICAS - FATORES DE RISCO\n")
cat("═════════════════════════════════════════════════════════\n\n")

cat("PESO (< 2500g = Baixo Peso):\n")
print(base %>%
  group_by(categoria = ifelse(PESO < 2500, "Baixo Peso (<2500g)", "Peso Normal (≥2500g)")) %>%
  summarise(n = n(), pct = round(100*n()/nrow(base), 2), reint_taxa = round(100*mean(REINT, na.rm=T), 2))
)
cat("\n")

cat("SEMAGESTAC (< 37 = Prematuro):\n")
print(base %>%
  group_by(categoria = ifelse(SEMAGESTAC < 37, "Prematuro (<37 sem)", "A Termo (≥37 sem)")) %>%
  summarise(n = n(), pct = round(100*n()/nrow(base), 2), reint_taxa = round(100*mean(REINT, na.rm=T), 2))
)
cat("\n")

cat("APGAR5 (< 7 = Apgar Baixo):\n")
print(base %>%
  group_by(categoria = ifelse(APGAR5 < 7, "Apgar Baixo (<7)", "Apgar Normal (≥7)")) %>%
  summarise(n = n(), pct = round(100*n()/nrow(base), 2), reint_taxa = round(100*mean(REINT, na.rm=T), 2))
)
cat("\n")

cat("═════════════════════════════════════════════════════════\n")
cat("5. VARIÁVEIS DEMOGRÁFICAS\n")
cat("═════════════════════════════════════════════════════════\n\n")

cat("SEXO:\n")
print(base %>%
  mutate(sexo_label = ifelse(SEXO == 1, "Masculino", "Feminino")) %>%
  group_by(sexo_label) %>%
  summarise(n = n(), pct = round(100*n()/nrow(base), 2), reint_taxa = round(100*mean(REINT, na.rm=T), 2))
)
cat("\n")

cat("TIPO DE PARTO:\n")
print(base %>%
  mutate(parto_label = ifelse(PARTO == 1, "Vaginal", "Cesáreo")) %>%
  group_by(parto_label) %>%
  summarise(n = n(), pct = round(100*n()/nrow(base), 2), reint_taxa = round(100*mean(REINT, na.rm=T), 2))
)
cat("\n")

cat("IDADE DA MÃE:\n")
print(base %>%
  summarise(
    media = round(mean(IDADEMAE, na.rm=T), 1),
    mediana = median(IDADEMAE, na.rm=T),
    min = min(IDADEMAE, na.rm=T),
    max = max(IDADEMAE, na.rm=T)
  )
)
cat("\nMãe Adolescente (< 20 anos):\n")
print(base %>%
  group_by(categoria = ifelse(IDADEMAE < 20, "Adolescente (<20)", "Adulta (≥20)")) %>%
  summarise(n = n(), pct = round(100*n()/nrow(base), 2), reint_taxa = round(100*mean(REINT, na.rm=T), 2))
)
cat("\n")

cat("═════════════════════════════════════════════════════════\n")
cat("6. SOCIODEMOGRÁFICO\n")
cat("═════════════════════════════════════════════════════════\n\n")

cat("ESCOLARIDADE DA MÃE:\n")
print(base %>%
  group_by(ESCMAE2010) %>%
  summarise(n = n(), pct = round(100*n()/nrow(base), 2), reint_taxa = round(100*mean(REINT, na.rm=T), 2))
)
cat("\n")

cat("ESTADO CIVIL DA MÃE:\n")
print(base %>%
  group_by(ESTCIVMAE) %>%
  summarise(n = n(), pct = round(100*n()/nrow(base), 2), reint_taxa = round(100*mean(REINT, na.rm=T), 2))
)
cat("\n")

cat("PARIDADE (0=Primípara, 1=Multípara):\n")
print(base %>%
  mutate(paridade_label = ifelse(PARIDADE == 0, "Primípara", "Multípara")) %>%
  group_by(paridade_label) %>%
  summarise(n = n(), pct = round(100*n()/nrow(base), 2), reint_taxa = round(100*mean(REINT, na.rm=T), 2))
)
cat("\n")

cat("═════════════════════════════════════════════════════════\n")
cat("7. ANÁLISE BIVARIADA - FATORES ASSOCIADOS\n")
cat("═════════════════════════════════════════════════════════\n\n")

cat("Risco Relativo (aproximado):\n\n")

fatores <- tribble(
  ~fator, ~reint_com, ~reint_sem,
  "Peso < 2500g",
    round(100*mean(base$REINT[base$PESO < 2500], na.rm=T), 2),
    round(100*mean(base$REINT[base$PESO >= 2500], na.rm=T), 2),
  "Semagestac < 37",
    round(100*mean(base$REINT[base$SEMAGESTAC < 37], na.rm=T), 2),
    round(100*mean(base$REINT[base$SEMAGESTAC >= 37], na.rm=T), 2),
  "Apgar < 7",
    round(100*mean(base$REINT[base$APGAR5 < 7], na.rm=T), 2),
    round(100*mean(base$REINT[base$APGAR5 >= 7], na.rm=T), 2),
  "Mãe Adolescente",
    round(100*mean(base$REINT[base$IDADEMAE < 20], na.rm=T), 2),
    round(100*mean(base$REINT[base$IDADEMAE >= 20], na.rm=T), 2),
  "Cesáreo",
    round(100*mean(base$REINT[base$PARTO == 2], na.rm=T), 2),
    round(100*mean(base$REINT[base$PARTO == 1], na.rm=T), 2)
)

fatores <- fatores %>%
  mutate(
    rr = round(reint_com / reint_sem, 2),
    diferenca = round(reint_com - reint_sem, 2)
  )

print(fatores)
cat("\n")

cat("════════════════════════════════════════════════════════════════\n")
cat("RESUMO EXECUTIVO\n")
cat("════════════════════════════════════════════════════════════════\n\n")

cat("Total de nascimentos linkados: 140.160\n")
cat("Reinternações identificadas: 323 (0.23%)\n")
cat("Sem reinternação: 139.837 (99.77%)\n\n")

cat("Cenários prevalentes:\n")
cat("  • Cenário A (sem internação SIH): 96.1% dos casos\n")
cat("  • Cenário B (match perfeito): 2.5% dos casos\n")
cat("  • Demais cenários (C, D, E, F): 1.4% dos casos\n\n")

cat("Idade na reinternação:\n")
cat("  • Média: 10.2 dias\n")
cat("  • Mediana: 9 dias\n")
cat("  • Crítica (4-7 dias): ~40% dos casos\n\n")

cat("Fatores de maior risco (por taxa de reinternação):\n")
cat("  • Apgar < 7: 3.09% de reinternação\n")
cat("  • Peso < 2500g: 2.50% de reinternação\n")
cat("  • Prematuro < 37 sem: 1.63% de reinternação\n\n")

cat("Distribuição por sexo:\n")
cat("  • Masculino: 0.25% reinternação\n")
cat("  • Feminino: 0.21% reinternação\n\n")

cat("Tipo de parto:\n")
cat("  • Vaginal: 0.24% reinternação\n")
cat("  • Cesáreo: 0.23% reinternação\n\n")

cat("════════════════════════════════════════════════════════════════\n\n")
