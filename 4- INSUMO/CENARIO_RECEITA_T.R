

df <- read_excel('./0 - DADOS/RECEITA_LIQUIDA_MONITOR_PLDO25_20240417.xlsx', sheet = "PLOA2024")
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
  select(1, starts_with(glue::glue("{year(mes_atualizacao)}"))) %>% 
  # group_by(RECEITAS) %>% 
  # mutate(across(starts_with(glue::glue("{year(mes_atualizacao)}")), cumsum))
  mutate(`PLOA 2024` = rowSums(select(., 2:13), na.rm = TRUE) / 1000000) %>% 
  select(1, ncol(.))

# CEN. GER. 323 ACUMULADO ANO --------------------

df <- read_excel('./0 - DADOS/RECEITA_LIQUIDA_MONITOR_PLDO25_20240417.xlsx', sheet = "PLDO2025")
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
  select(1, starts_with(glue::glue("{year(mes_atualizacao)}"))) %>% 
  # group_by(RECEITAS) %>% 
  # mutate(across(starts_with(glue::glue("{year(mes_atualizacao)}")), cumsum))
  mutate(`GERENCIAL` = rowSums(select(., 2:13), na.rm = TRUE) / 1000000) %>% 
  select(1, ncol(.))


# CEN. GER. 323 ACUMULADO ATÉ O MÊS --------------------

df <- read_excel('./0 - DADOS/RECEITA_LIQUIDA_MONITOR_PLDO25_20240417.xlsx', sheet = "PLDO2025")
mes_dados <- glue::glue("Até {format(mes_atualizacao, '%B')}")

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
  select(1, starts_with(glue::glue("{year(mes_atualizacao)}"))) %>% 
  # group_by(RECEITAS) %>% 
  # mutate(across(starts_with(glue::glue("{year(mes_atualizacao)}")), cumsum))
  mutate(!!mes_dados := rowSums(select(., 1:month(mes_atualizacao)+1), na.rm = TRUE) / 1000000) %>%    # Usando month(mes_atualizacao) como index da coluna
  select(1, ncol(.))


# REALIZADO ACUM --------------------

df <- read_excel('./0 - DADOS/RECEITA_LIQUIDA_MONITOR_PLDO25_20240417.xlsx', sheet = "realizado") %>% 
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
           Column1 == "Cota-Parte do IPI -  LC 61/1989" |
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
  select(1, starts_with(glue::glue("{year(mes_atualizacao)-1}"))) %>%
  mutate(across(starts_with(glue::glue("{year(mes_atualizacao)-1}")), as.numeric)) %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% 
  rowwise() %>%
  mutate(acumulado = list(cumsum(c_across(starts_with(glue::glue("{year(mes_atualizacao)-1}")))))) %>%
  unnest_wider(acumulado, names_sep = "_")

# Ajustar os nomes das colunas acumuladas para incluir "acumulado_" seguido pelo nome original da coluna
original_col_names <- names(df_acum23)[grepl(glue::glue("{year(mes_atualizacao)-1}"), names(df_acum23))]
new_col_names <- paste0("acumulado_", original_col_names)
names(df_acum23)[(ncol(df_acum23) - length(original_col_names) + 1):ncol(df_acum23)] <- new_col_names

df_acum23 <- df_acum23 %>% 
  select(1, glue::glue("acumulado_{format(floor_date(mes_atualizacao, 'month') %m-% years(1), '%Y%m')}")) %>% 
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
           Column1 == "Cota-Parte do IPI -  LC 61/1989" |
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
  select(1, starts_with(glue::glue("{year(mes_atualizacao)}"))) %>% 
  mutate(across(starts_with(glue::glue("{year(mes_atualizacao)}")), as.numeric)) %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% 
  rowwise() %>%
  mutate(acumulado = list(cumsum(c_across(starts_with(glue::glue("{year(mes_atualizacao)}")))))) %>%
  unnest_wider(acumulado, names_sep = "_")

# Ajustar os nomes das colunas acumuladas para incluir "acumulado_" seguido pelo nome original da coluna
original_col_names <- names(df_acum24)[grepl(glue::glue("{year(mes_atualizacao)}"), names(df_acum24))]
new_col_names <- paste0("acumulado_", original_col_names)
names(df_acum24)[(ncol(df_acum24) - length(original_col_names) + 1):ncol(df_acum24)] <- new_col_names

df_acum24 <- df_acum24 %>% 
  select(1, glue::glue("acumulado_{format(mes_atualizacao, '%Y%m')}")) %>% 
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
    `Dif. (R$)` = sum(c_across(starts_with(glue::glue("acumulado_{year(mes_atualizacao)}"))) - !!sym(mes_dados) ),
    `Dif. (%)` = round(sum((c_across(starts_with(glue::glue("acumulado_{year(mes_atualizacao)}"))) / !!sym(mes_dados) - 1) * 100), 2),
    `Dif.  (R$)` = sum(c_across(starts_with(glue::glue("acumulado_{year(mes_atualizacao)}"))) - c_across(starts_with(glue::glue("acumulado_{year(mes_atualizacao)-1}")))),
    `Dif.  (%)` = round(sum((c_across(starts_with(glue::glue("acumulado_{year(mes_atualizacao)}"))) / c_across(starts_with(glue::glue("acumulado_{year(mes_atualizacao)-1}"))) - 1) * 100), 2)
  ) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, ~ ifelse(. == 0, NA, .)) %>% 
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>% 
  rename("Acumulado 2023" = 5, "Acumulado 2024" = 6) %>% 
  #add_column(col_space = " ", .name_repair = "unique") %>% 
  add_column(col_space = " ", .name_repair = "unique") %>% 
  add_column(col_space = " ", .name_repair = "unique") %>% 
  add_column(col_space = " ", .name_repair = "unique") %>% 
  relocate(1,2,3,4,12,5,6,13,7,8,11)


# FORMATAÇÃO DA TABELA -----------------

tabela_acumulado <- TAB_GER %>%
  flextable() %>% 
  fontsize(size = 10, part = "header") %>% 
  fontsize(size = 10, part = "body") %>%
  border_remove() %>%
  colformat_double(j = c("PLOA 2024", "GERENCIAL", 
                         glue::glue("Até {format(mes_atualizacao, '%B')}"), "Acumulado 2023",
                         "Acumulado 2024", "Dif. (R$)", "Dif.  (R$)"),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>% 
  
  colformat_double(j = c("Dif. (%)", "Dif.  (%)"),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 1, 
                   na_str = "--",
                   suffix = ' %') %>% 

  
  set_header_labels(values = c("RECEITAS", "LOA 2024","(2024)", 
                               glue::glue("Até {format(mes_atualizacao, '%B')}"), " ",
                               "Acumulado 2023", "Acumulado 2024", " ", "Dif. (R$)", "Dif. (%)", " ",
                               "Dif. (R$)", "Dif. (%)")) %>% 
  
  bg(., 
     part = "header", 
     bg = cor1[2]) %>% 
  flextable::style( pr_t = fp_text_default(
    bold = F,
    color = cor1[3]
  ),
  part = 'header') %>% 
  bg(., i= c(1:3,8:13, 18, 23:29, 31), 
     part = "body", 
     bg = cor1[1]) %>% 
  
  bg(., i= c(4:7, 14:17, 19:22, 30), 
     part = "body", 
     bg = cor1[3]) %>%
  
  bg(., i= ~ RECEITAS == "RECEITA TOTAL", 
     part = "body", 
     bg = cor1[2]) %>%
  
  flextable::style(i =  ~ RECEITAS == "RECEITA TOTAL", 
        pr_t = fp_text_default(
          bold = F,
          color = cor1[3]
        )) %>% 
  color( ~ `Dif. (R$)` < 0, ~ `Dif. (R$)`,  color = cor1[4]) %>% 
  color( ~ `Dif.  (R$)` < 0, ~ `Dif.  (R$)`,  color = cor1[4]) %>% 
  color( ~ `Dif. (%)` < 0, ~ `Dif. (%)`,  color = cor1[4]) %>% 
  color( ~ `Dif.  (%)` < 0, ~ `Dif.  (%)`,  color = cor1[4]) %>% 
  
  add_header_row(values = c("RECEITAS", "LOA 2024", "Cenário atual", " ",
                            glue::glue(" "), " ",
                            " ", " ", " "), 
                 colwidths = c(1,1,2,1,2,1,2,1,2)) %>% 
  add_header_row(values = c("RECEITAS", "Previsões", " ",
                            glue::glue("Realizado até {format(mes_atualizacao, '%B')}"), " ",
                            "Cenário x Realizado", " ", "Acum24/Acum23"), 
                 colwidths = c(1,3,1,2,1,2,1,2)) %>% 
  
  merge_at(i = 1:3, j = 1, part = "header") %>% 
  merge_at(i = 2:3, j = 2, part = "header") %>% 
  merge_at(i = 1:2, j = c(6:7), part = "header") |> 
  merge_at(i = 1:2, j = c(9:10), part = "header") |>  
  merge_at(i = 1:2, j = c(12:13), part = "header") |>  
  align(align = "center", part = "header") %>% 
  align(j = 1, align = "left", part = "header") %>% 
  width(j = 1, width = 8.3, unit = 'cm') %>%
  
  width(j = c(2:13), width = 2.4, unit = 'cm') |>
  width(j = c(5,8,11), width = .2, unit = 'cm') |> 
  hline(i = 1, j = c(2:4), border = fp_border(color = "white", width = 1), part = 'header') %>% 
  hline(i = 2, j = c(3:4,6:7,9:10,12:13), border = fp_border(color = "white", width = 1), part = 'header') %>% 
  hline(i = 2, j = c(3,6,12), border = fp_border(color = "white", width = 1), part = 'header') %>% 
  height(height = 0.4, part = "header", unit = 'cm') %>%
  height(height = 0.4, part = "body", unit = 'cm') %>% 
  padding(padding.left = 2, padding.right = 2, padding.top = 0, padding.bottom = 0, part = "all") %>% 
  padding( i=c(3:7, 14:17, 19:22, 30), j=1, padding.left=8) %>% 
  fontsize(j = c(1:13), size = 10, part = "body")
