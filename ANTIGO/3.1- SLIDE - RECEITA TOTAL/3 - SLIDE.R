g_rec <- realizado %>% 
  filter(RECEITAS == "1.1. IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA" |
           RECEITAS == "1.2. CONTRIBUIÇÕES" |
           RECEITAS == "1.3. RECEITA PATRIMONIAL" |
           RECEITAS == "1.4. RECEITA AGROPECUÁRIA" |
           RECEITAS == "1.5. RECEITA INDUSTRIAL" |
           RECEITAS == "1.6. RECEITA DE SERVIÇOS" |
           RECEITAS == "1.7. TRANSFERÊNCIAS CORRENTES" |
           RECEITAS == "1.9. OUTRAS RECEITAS CORRENTES" |
           RECEITAS == "2. RECEITAS DE CAPITAL" |
           RECEITAS == "Receitas Intraorçamentárias") %>% 
  select(1, starts_with(glue::glue("{year(Sys.Date())}"))) %>% 
  select(RECEITAS, `202406`) %>% 
  mutate(across(where(is.numeric), ~ round(. / 1000000, 2))) %>% 
  setNames(c('Receitas', 'mes_2024')) %>% 
  
  bind_cols(realizado %>% 
              filter(RECEITAS == "1.1. IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA" |
                       RECEITAS == "1.2. CONTRIBUIÇÕES" |
                       RECEITAS == "1.3. RECEITA PATRIMONIAL" |
                       RECEITAS == "1.4. RECEITA AGROPECUÁRIA" |
                       RECEITAS == "1.5. RECEITA INDUSTRIAL" |
                       RECEITAS == "1.6. RECEITA DE SERVIÇOS" |
                       RECEITAS == "1.7. TRANSFERÊNCIAS CORRENTES" |
                       RECEITAS == "1.9. OUTRAS RECEITAS CORRENTES" |
                       RECEITAS == "2. RECEITAS DE CAPITAL" |
                       RECEITAS == "Receitas Intraorçamentárias") %>% 
              select(1, starts_with(glue::glue("{year(Sys.Date())-1}"))) %>% 
              select(RECEITAS, `202306`) %>% 
              mutate(across(where(is.numeric), ~ round(. / 1000000, 2))) %>% 
              setNames(c('Receitas', 'mes_2023')) %>% 
              select(mes_2023)) %>% 
  
  bind_cols(projeção1 %>% 
              filter(RECEITAS == "1.1. IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA" |
                       RECEITAS == "1.2. CONTRIBUIÇÕES" |
                       RECEITAS == "1.3. RECEITA PATRIMONIAL" |
                       RECEITAS == "1.4. RECEITA AGROPECUÁRIA" |
                       RECEITAS == "1.5. RECEITA INDUSTRIAL" |
                       RECEITAS == "1.6. RECEITA DE SERVIÇOS" |
                       RECEITAS == "1.7. TRANSFERÊNCIAS CORRENTES" |
                       RECEITAS == "1.9. OUTRAS RECEITAS CORRENTES" |
                       RECEITAS == "2. RECEITAS DE CAPITAL" |
                       RECEITAS == "Receitas Intraorçamentárias") %>% 
              select(1, starts_with(glue::glue("{year(Sys.Date())}"))) %>% 
              select(RECEITAS, `202406`) %>% 
              mutate(across(where(is.numeric), ~ round(. / 1000000, 2))) %>% 
              setNames(c('Receitas', 'proj_24')) %>% 
              select(proj_24)) %>% 
  
  adorn_totals() %>% 
  mutate(perc_participação = round((mes_2024/tail(mes_2024, n = 1))*100, 2))

# %>% 
#   
#   bind_cols(realizado %>% 
#               filter(RECEITAS == "1.1. IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA" |
#                        RECEITAS == "1.2. CONTRIBUIÇÕES" |
#                        RECEITAS == "1.3. RECEITA PATRIMONIAL" |
#                        RECEITAS == "1.4. RECEITA AGROPECUÁRIA" |
#                        RECEITAS == "1.5. RECEITA INDUSTRIAL" |
#                        RECEITAS == "1.6. RECEITA DE SERVIÇOS" |
#                        RECEITAS == "1.7. TRANSFERÊNCIAS CORRENTES" |
#                        RECEITAS == "1.9. OUTRAS RECEITAS CORRENTES" |
#                        RECEITAS == "2. RECEITAS DE CAPITAL" |
#                        RECEITAS == "Receitas Intraorçamentárias") %>% 
#               select(1, starts_with(glue::glue("{year(Sys.Date())}"))) %>%
#               pivot_longer(cols = 2:13) %>% 
#               group_by(RECEITAS) %>% 
#               summarise(acum_24 = sum(value)/ 1000000) %>% 
#               select(acum_24)) %>% 
#   bind_cols(realizado %>% 
#               filter(RECEITAS == "1.1. IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA" |
#                        RECEITAS == "1.2. CONTRIBUIÇÕES" |
#                        RECEITAS == "1.3. RECEITA PATRIMONIAL" |
#                        RECEITAS == "1.4. RECEITA AGROPECUÁRIA" |
#                        RECEITAS == "1.5. RECEITA INDUSTRIAL" |
#                        RECEITAS == "1.6. RECEITA DE SERVIÇOS" |
#                        RECEITAS == "1.7. TRANSFERÊNCIAS CORRENTES" |
#                        RECEITAS == "1.9. OUTRAS RECEITAS CORRENTES" |
#                        RECEITAS == "2. RECEITAS DE CAPITAL" |
#                        RECEITAS == "Receitas Intraorçamentárias") %>% 
#               select(1, starts_with(glue::glue("{year(Sys.Date())-1}"))) %>%
#               pivot_longer(cols = 2:7) %>% 
#               group_by(RECEITAS) %>% 
#               summarise(acum_23 = sum(value)/ 1000000) %>% 
#               select(acum_23))
# 
# ### Arrumar pois o mes tá manual
# 
# 
# 
# 


