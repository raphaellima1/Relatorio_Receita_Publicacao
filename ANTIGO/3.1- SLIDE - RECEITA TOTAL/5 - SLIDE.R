# library(readxl)
# library(dplyr)
# library(tidyverse)
# library(rlang)

setwd("./../")
setwd('./0 - DADOS/')

# PLOA 2024 --------------------

df <- read_excel(dir(full.names=T, pattern="^RECEITA_LIQUIDA_MONITOR_PLDO25.*\\.xlsx$"), sheet = "PLOA2024")
df_PLOA2024 <- df %>% 
  filter(RECEITAS == "1. RECEITAS CORRENTES" |
           RECEITAS == "1.1. IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA" |
           Column1 == "ICMS" |
           Column1 == "IPVA" |
           Column1 == "ITCD" |
           Column1 == "IRRF" |
           Column1 == "Outras receitas tributárias" |
           RECEITAS == "1.2. CONTRIBUIÇÕES" |
           RECEITAS == "1.3. RECEITA PATRIMONIAL" |
           RECEITAS == "1.4. RECEITA AGROPECUÁRIA" |
           RECEITAS == "1.5. RECEITA INDUSTRIAL" |
           RECEITAS == "1.6. RECEITA DE SERVIÇOS" |
           RECEITAS == "1.7. TRANSFERÊNCIAS CORRENTES" |
           Column1 == "Cota-Parte do FPE" |
           Column1 == "Transferências da LC 61/1989" |
           Column1 == "Transferências do FUNDEB" |
           Column1 == "Outras Transferências Correntes" |
           RECEITAS == "1.9. OUTRAS RECEITAS CORRENTES" |
           Column1 == "Contribuições ao PROTEGE" |
           Column1 == "Contribuição PROTEGE - Lei 20.367/2018 - Art. 3º, I e II" |
           Column1 == "FUNDEINFRA" |
           Column1 == "Demais Receitas Correntes" |
           RECEITAS == "2. RECEITAS DE CAPITAL" |
           RECEITAS == "2.1. OPERAÇÕES DE CRÉDITO" |
           RECEITAS == "2.2. ALIENAÇÃO DE BENS" |
           RECEITAS == "2.3. AMORTIZAÇÃO DE EMPRÉSTIMOS" |
           RECEITAS == "2.3. TRANSFERENCIAS DE CAPITAL" |
           RECEITAS == "2.4. OUTRAS RECEITAS DE CAPITAL" |
           RECEITAS == "RECEITA TOTAL LÍQUIDA (EXCETO INTRAORÇAMENTÁRIA)" |
           RECEITAS == "Receitas Intraorçamentárias" |
           RECEITAS == "RECEITA TOTAL") %>% 
  unite(RECEITAS, c("RECEITAS", "Column1"), na.rm = T) %>% 
  select(1, starts_with(glue::glue("{year(Sys.Date())}"))) %>% 
  # group_by(RECEITAS) %>% 
  # mutate(across(starts_with(glue::glue("{year(Sys.Date())}")), cumsum))
  mutate(`PLOA 2024` = rowSums(select(., 2:13), na.rm = TRUE) / 1000000) %>% 
  select(1, ncol(.))

# CEN. GER. 323 ACUMULADO ANO --------------------

df <- read_excel(dir(full.names=T, pattern="^RECEITA_LIQUIDA_MONITOR_PLDO25.*\\.xlsx$"), sheet = "PLDO2025")
df_G323_T <- df %>% 
  filter(RECEITAS == "1. RECEITAS CORRENTES" |
           RECEITAS == "1.1. IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA" |
           Colunas1 == "ICMS" |
           Colunas1 == "IPVA" |
           Colunas1 == "ITCD" |
           Colunas1 == "IRRF" |
           Colunas1 == "Outras receitas tributárias" |
           RECEITAS == "1.2. CONTRIBUIÇÕES" |
           RECEITAS == "1.3. RECEITA PATRIMONIAL" |
           RECEITAS == "1.4. RECEITA AGROPECUÁRIA" |
           RECEITAS == "1.5. RECEITA INDUSTRIAL" |
           RECEITAS == "1.6. RECEITA DE SERVIÇOS" |
           RECEITAS == "1.7. TRANSFERÊNCIAS CORRENTES" |
           Colunas1 == "Cota-Parte do FPE" |
           Colunas1 == "Transferências da LC 61/1989" |
           Colunas1 == "Transferências do FUNDEB" |
           Colunas1 == "Outras Transferências Correntes" |
           RECEITAS == "1.9. OUTRAS RECEITAS CORRENTES" |
           Colunas1 == "Contribuições ao PROTEGE" |
           Colunas1 == "Contribuição PROTEGE - Lei 20.367/2018 - Art. 3º, I e II" |
           Colunas1 == "FUNDEINFRA" |
           Colunas1 == "Demais Receitas Correntes" |
           RECEITAS == "2. RECEITAS DE CAPITAL" |
           RECEITAS == "2.1. OPERAÇÕES DE CRÉDITO" |
           RECEITAS == "2.2. ALIENAÇÃO DE BENS" |
           RECEITAS == "2.3. AMORTIZAÇÃO DE EMPRÉSTIMOS" |
           RECEITAS == "2.3. TRANSFERENCIAS DE CAPITAL" |
           RECEITAS == "2.4. OUTRAS RECEITAS DE CAPITAL" |
           RECEITAS == "RECEITA TOTAL LÍQUIDA (EXCETO INTRAORÇAMENTÁRIA)" |
           RECEITAS == "Receitas Intraorçamentárias" |
           RECEITAS == "RECEITA TOTAL") %>% 
  unite(RECEITAS, c("RECEITAS", "Colunas1"), na.rm = T) %>% 
  select(1, starts_with(glue::glue("{year(Sys.Date())}"))) %>% 
  # group_by(RECEITAS) %>% 
  # mutate(across(starts_with(glue::glue("{year(Sys.Date())}")), cumsum))
  mutate(`GERENCIAL` = rowSums(select(., 2:13), na.rm = TRUE) / 1000000) %>% 
  select(1, ncol(.))


# CEN. GER. 323 ACUMULADO ATÉ O MÊS --------------------

df <- read_excel(dir(full.names=T, pattern="^RECEITA_LIQUIDA_MONITOR_PLDO25.*\\.xlsx$"), sheet = "PLDO2025")
mes_dados <- glue::glue("Até {format(Sys.Date() %m-% months(1), '%B')}")

df_G323 <- df %>% 
  filter(RECEITAS == "1. RECEITAS CORRENTES" |
           RECEITAS == "1.1. IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA" |
           Colunas1 == "ICMS" |
           Colunas1 == "IPVA" |
           Colunas1 == "ITCD" |
           Colunas1 == "IRRF" |
           Colunas1 == "Outras receitas tributárias" |
           RECEITAS == "1.2. CONTRIBUIÇÕES" |
           RECEITAS == "1.3. RECEITA PATRIMONIAL" |
           RECEITAS == "1.4. RECEITA AGROPECUÁRIA" |
           RECEITAS == "1.5. RECEITA INDUSTRIAL" |
           RECEITAS == "1.6. RECEITA DE SERVIÇOS" |
           RECEITAS == "1.7. TRANSFERÊNCIAS CORRENTES" |
           Colunas1 == "Cota-Parte do FPE" |
           Colunas1 == "Transferências da LC 61/1989" |
           Colunas1 == "Transferências do FUNDEB" |
           Colunas1 == "Outras Transferências Correntes" |
           RECEITAS == "1.9. OUTRAS RECEITAS CORRENTES" |
           Colunas1 == "Contribuições ao PROTEGE" |
           Colunas1 == "Contribuição PROTEGE - Lei 20.367/2018 - Art. 3º, I e II" |
           Colunas1 == "FUNDEINFRA" |
           Colunas1 == "Demais Receitas Correntes" |
           RECEITAS == "2. RECEITAS DE CAPITAL" |
           RECEITAS == "2.1. OPERAÇÕES DE CRÉDITO" |
           RECEITAS == "2.2. ALIENAÇÃO DE BENS" |
           RECEITAS == "2.3. AMORTIZAÇÃO DE EMPRÉSTIMOS" |
           RECEITAS == "2.3. TRANSFERENCIAS DE CAPITAL" |
           RECEITAS == "2.4. OUTRAS RECEITAS DE CAPITAL" |
           RECEITAS == "RECEITA TOTAL LÍQUIDA (EXCETO INTRAORÇAMENTÁRIA)" |
           RECEITAS == "Receitas Intraorçamentárias" |
           RECEITAS == "RECEITA TOTAL") %>% 
  unite(RECEITAS, c("RECEITAS", "Colunas1"), na.rm = T) %>% 
  select(1, starts_with(glue::glue("{year(Sys.Date())}"))) %>% 
  # group_by(RECEITAS) %>% 
  # mutate(across(starts_with(glue::glue("{year(Sys.Date())}")), cumsum))
  mutate(!!mes_dados := rowSums(select(., 2:month(Sys.Date())), na.rm = TRUE) / 1000000) %>%    # Usando month(Sys.Date()) como index da coluna
  select(1, ncol(.))


# REALIZADO ACUM --------------------

df <- read_excel(dir(full.names=T, pattern="^RECEITA_LIQUIDA_MONITOR_PLDO25.*\\.xlsx$"), sheet = "realizado") %>% 
  slice(-1) %>% 
  setNames(.[1,]) %>% 
  .[-1,]

df_acum23 <- df %>% 
  filter(RECEITAS == "1. RECEITAS CORRENTES" |
           RECEITAS == "1.1. IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA" |
           Column1 == "ICMS" |
           Column1 == "IPVA" |
           Column1 == "ITCD" |
           Column1 == "IRRF" |
           Column1 == "Outras receitas tributárias" |
           RECEITAS == "1.2. CONTRIBUIÇÕES" |
           RECEITAS == "1.3. RECEITA PATRIMONIAL" |
           RECEITAS == "1.4. RECEITA AGROPECUÁRIA" |
           RECEITAS == "1.5. RECEITA INDUSTRIAL" |
           RECEITAS == "1.6. RECEITA DE SERVIÇOS" |
           RECEITAS == "1.7. TRANSFERÊNCIAS CORRENTES" |
           Column1 == "Cota-Parte do FPE" |
           Column1 == "Transferências da LC 61/1989" |
           Column1 == "Transferências do FUNDEB" |
           Column1 == "Outras Transferências Correntes" |
           RECEITAS == "1.9. OUTRAS RECEITAS CORRENTES" |
           Column1 == "Contribuições ao PROTEGE" |
           Column1 == "Contribuição PROTEGE - Lei 20.367/2018 - Art. 3º, I e II" |
           Column1 == "FUNDEINFRA" |
           Column1 == "Demais Receitas Correntes" |
           RECEITAS == "2. RECEITAS DE CAPITAL" |
           RECEITAS == "2.1. OPERAÇÕES DE CRÉDITO" |
           RECEITAS == "2.2. ALIENAÇÃO DE BENS" |
           RECEITAS == "2.3. AMORTIZAÇÃO DE EMPRÉSTIMOS" |
           RECEITAS == "2.3. TRANSFERENCIAS DE CAPITAL" |
           RECEITAS == "2.4. OUTRAS RECEITAS DE CAPITAL" |
           RECEITAS == "RECEITA TOTAL LÍQUIDA (EXCETO INTRAORÇAMENTÁRIA)" |
           RECEITAS == "Receitas Intraorçamentárias" |
           RECEITAS == "RECEITA TOTAL") %>% 
  unite(RECEITAS, c("RECEITAS", "Column1"), na.rm = T) %>% 
  select(1, starts_with(glue::glue("{year(Sys.Date())-1}"))) %>%
  mutate(across(starts_with(glue::glue("{year(Sys.Date())-1}")), as.numeric)) %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% 
  rowwise() %>%
  mutate(acumulado = list(cumsum(c_across(starts_with(glue::glue("{year(Sys.Date())-1}")))))) %>%
  unnest_wider(acumulado, names_sep = "_")

# Ajustar os nomes das colunas acumuladas para incluir "acumulado_" seguido pelo nome original da coluna
original_col_names <- names(df_acum23)[grepl(glue::glue("{year(Sys.Date())-1}"), names(df_acum23))]
new_col_names <- paste0("acumulado_", original_col_names)
names(df_acum23)[(ncol(df_acum23) - length(original_col_names) + 1):ncol(df_acum23)] <- new_col_names

df_acum23 <- df_acum23 %>% 
  select(1, glue::glue("acumulado_{format(floor_date(Sys.Date(), 'month') %m-% months(1) %m-% years(1), '%Y%m')}")) %>% 
  mutate(across(where(is.numeric), ~ . / 1000000))

# ----------------------------

df_acum24 <- df %>% 
  filter(RECEITAS == "1. RECEITAS CORRENTES" |
           RECEITAS == "1.1. IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA" |
           Column1 == "ICMS" |
           Column1 == "IPVA" |
           Column1 == "ITCD" |
           Column1 == "IRRF" |
           Column1 == "Outras receitas tributárias" |
           RECEITAS == "1.2. CONTRIBUIÇÕES" |
           RECEITAS == "1.3. RECEITA PATRIMONIAL" |
           RECEITAS == "1.4. RECEITA AGROPECUÁRIA" |
           RECEITAS == "1.5. RECEITA INDUSTRIAL" |
           RECEITAS == "1.6. RECEITA DE SERVIÇOS" |
           RECEITAS == "1.7. TRANSFERÊNCIAS CORRENTES" |
           Column1 == "Cota-Parte do FPE" |
           Column1 == "Transferências da LC 61/1989" |
           Column1 == "Transferências do FUNDEB" |
           Column1 == "Outras Transferências Correntes" |
           RECEITAS == "1.9. OUTRAS RECEITAS CORRENTES" |
           Column1 == "Contribuições ao PROTEGE" |
           Column1 == "Contribuição PROTEGE - Lei 20.367/2018 - Art. 3º, I e II" |
           Column1 == "FUNDEINFRA" |
           Column1 == "Demais Receitas Correntes" |
           RECEITAS == "2. RECEITAS DE CAPITAL" |
           RECEITAS == "2.1. OPERAÇÕES DE CRÉDITO" |
           RECEITAS == "2.2. ALIENAÇÃO DE BENS" |
           RECEITAS == "2.3. AMORTIZAÇÃO DE EMPRÉSTIMOS" |
           RECEITAS == "2.3. TRANSFERENCIAS DE CAPITAL" |
           RECEITAS == "2.4. OUTRAS RECEITAS DE CAPITAL" |
           RECEITAS == "RECEITA TOTAL LÍQUIDA (EXCETO INTRAORÇAMENTÁRIA)" |
           RECEITAS == "Receitas Intraorçamentárias" |
           RECEITAS == "RECEITA TOTAL") %>% 
  unite(RECEITAS, c("RECEITAS", "Column1"), na.rm = T) %>% 
  select(1, starts_with(glue::glue("{year(Sys.Date())}"))) %>% 
  mutate(across(starts_with(glue::glue("{year(Sys.Date())}")), as.numeric)) %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% 
  rowwise() %>%
  mutate(acumulado = list(cumsum(c_across(starts_with(glue::glue("{year(Sys.Date())}")))))) %>%
  unnest_wider(acumulado, names_sep = "_")

# Ajustar os nomes das colunas acumuladas para incluir "acumulado_" seguido pelo nome original da coluna
original_col_names <- names(df_acum24)[grepl(glue::glue("{year(Sys.Date())}"), names(df_acum24))]
new_col_names <- paste0("acumulado_", original_col_names)
names(df_acum24)[(ncol(df_acum24) - length(original_col_names) + 1):ncol(df_acum24)] <- new_col_names

df_acum24 <- df_acum24 %>% 
  select(1, glue::glue("acumulado_{format(floor_date(Sys.Date(), 'month') %m-% months(1), '%Y%m')}")) %>% 
  mutate(across(where(is.numeric), ~ . / 1000000))


# TABELA FINAL ----------------------
TAB_GER <- df_PLOA2024 %>% 
  bind_cols(
    df_G323_T %>% select(-1)
  ) %>% 
  bind_cols(
    df_G323 %>% select(-1)
  ) %>% 
  bind_cols(
    df_acum23 %>% select(-1)
  ) %>% 
  bind_cols(
    df_acum24 %>% select(-1)
  ) %>% 
  rowwise() %>% 
  mutate(
    `Dif. (R$)` = sum(c_across(starts_with(glue::glue("acumulado_{year(Sys.Date())}"))) - !!sym(mes_dados) ),
    `Dif. (%)` = round(sum((c_across(starts_with(glue::glue("acumulado_{year(Sys.Date())}"))) / !!sym(mes_dados) - 1) * 100), 2),
    `Dif.  (R$)` = sum(c_across(starts_with(glue::glue("acumulado_{year(Sys.Date())}"))) - c_across(starts_with(glue::glue("acumulado_{year(Sys.Date())-1}")))),
    `Dif.  (%)` = round(sum((c_across(starts_with(glue::glue("acumulado_{year(Sys.Date())}"))) / c_across(starts_with(glue::glue("acumulado_{year(Sys.Date())-1}"))) - 1) * 100), 2)
  ) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, ~ ifelse(. == 0, NA, .)) %>% 
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>% 
  rename("Acumulado 2023" = 5, "Acumulado 2024" = 6) %>% 
  add_column(col_space = " ", .name_repair = "unique") %>% 
  add_column(col_space = " ", .name_repair = "unique") %>% 
  add_column(col_space = " ", .name_repair = "unique") %>% 
  add_column(col_space = " ", .name_repair = "unique") %>% 
  relocate(1,2,3,11,4,12,5,6,13,7,8,14)


# FORMATAÇÃO DA TABELA -----------------

set_flextable_defaults(
  font.size = 10, font.family = "Calibri",
  font.color = "#333333",
  table.layout = "fixed",
  border.color = "gray")


std_border <- fp_border(color = "black", width = 1.1)


tabela_acumulado <- TAB_GER %>%
  flextable() %>% 
  fontsize(size = 10, part = "header") %>% 
  fontsize(size = 10, part = "body") %>%
  border_remove() %>%
  colformat_double(j = c("PLOA 2024", "GERENCIAL", glue::glue("Até {format(Sys.Date() %m-% months(1), '%B')}"), "Acumulado 2023",
                         "Acumulado 2024", "Dif. (R$)", "Dif.  (R$)"),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>% 
  
  colformat_double(j = c("Dif. (%)", "Dif.  (%)"),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>% 
  
  set_header_labels(values = c("RECEITAS", "PLOA 2024","GERENCIAL", " ",
                               glue::glue("Até {format(Sys.Date() %m-% months(1), '%B')}"), " ",
                               "Acumulado 2023", "Acumulado 2024", " ", "Dif. (R$)", "Dif. (%)", " ",
                               "Dif. (R$)", "Dif. (%)")) %>% 
  
  bg(., 
     part = "header", 
     bg = "gray70") %>% 
  style( pr_t = fp_text_default(
    bold = T
    #,color = "black"
  ),
  part = 'header') %>% 
  bg(., i= c(1:2,8:13, 18, 23:29, 31), 
     part = "body", 
     bg = "#E3E7E7") %>% 
  
  bg(., i= c(3:7, 14:17, 19:22, 30), 
     part = "body", 
     bg = "gray98") %>%
  
  bg(., i= ~ RECEITAS == "RECEITA TOTAL", 
     part = "body", 
     bg = "gray60") %>%
  
  style(i =  ~ RECEITAS == "RECEITA TOTAL", 
        pr_t = fp_text_default(
          bold = T
          #color = "black"
        )) %>% 
  color( ~ `Dif. (R$)` < 0, ~ `Dif. (R$)`,  color = 'red' ) %>% 
  color( ~ `Dif.  (R$)` < 0, ~ `Dif.  (R$)`,  color = 'red' ) %>% 
  color( ~ `Dif. (%)` < 0, ~ `Dif. (%)`,  color = 'red' ) %>% 
  color( ~ `Dif.  (%)` < 0, ~ `Dif.  (%)`,  color = 'red' ) %>% 
  add_header_row(values = c("RECEITAS", "Previsão para o Exercício", " ", "Gerencial", " ",
                            glue::glue("Realizado até {format(Sys.Date() %m-% months(1), '%B')}"), " ",
                            "Gerencial", " ", "Acum24/Acum23"), 
                 colwidths = c(1,2,1,1,1,2,1,2,1,2)) %>% 
  align(align = "center", part = "header") %>% 
  align(j = 1, align = "left", part = "header") %>% 
  width(j = 1, width = 8.3, unit = 'cm') %>%
  width(j = c(4,6,9,12), width = .2, unit = 'cm') %>% 
  width(j = c(2,3,5,7,8,10,11,13,14), width = 2.4, unit = 'cm') %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>% 
  hline(i = 1, j = c(2:3,5,7:8,10:11,13:14), border = fp_border(color = "black", width = 1), part = 'header') %>% 
  height(height = 0.4, part = "header", unit = 'cm') %>%
  height(height = 0.4, part = "body", unit = 'cm') %>% 
  padding(padding.left = 2, padding.right = 2, padding.top = 0, padding.bottom = 0, part = "all") %>% 
  padding( i=c(3:7, 14:17, 19:22, 30), j=1, padding.left=8) %>% 
  fontsize(j = c(1:14), size = 10, part = "body")
  


setwd("./../")