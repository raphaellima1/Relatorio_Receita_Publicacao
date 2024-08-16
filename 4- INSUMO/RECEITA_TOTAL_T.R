
# PUXANDO OS DADOS DAS RTLs DE REALIZADAS EM T-1 E T, E RTL PROJETADA DE T------
RLT <- realizado %>% 
  filter(RECEITAS == 'RECEITA TOTAL LÍQUIDA (EXCETO INTRAORÇAMENTÁRIA)') %>% 
  select(c(1, starts_with(glue('{year(Sys.Date())}')))) %>% 
  mutate(across(2:13, ~ na_if(as.numeric(.), 0.00)/1000000)) %>% 
  pivot_longer(cols =  2:13) %>% 
  mutate(data = ymd(paste0(name, '01'))) %>% 
  setNames(c('RCL', 'name', 'RCL_2024', 'data')) %>% 
  bind_cols(projeção1 %>% 
              filter(RECEITAS == 'RECEITA TOTAL LÍQUIDA (EXCETO INTRAORÇAMENTÁRIA)') %>% 
              select(c(1, starts_with(glue('{year(Sys.Date())}')))) %>% 
              mutate(across(2:13, as.numeric)/1000000) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01'))) %>% 
              select(value) %>% 
              setNames('Projeção_RCL')) %>% 
  bind_cols(realizado %>% 
              filter(RECEITAS == 'RECEITA TOTAL LÍQUIDA (EXCETO INTRAORÇAMENTÁRIA)') %>% 
              select(c(1, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
              mutate(across(2:13, as.numeric)/1000000) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01'))) %>% 
              select(value) %>% 
              setNames('RCL_2023'))%>%
# RTL ACUMULADO NO ANO
  mutate(acum_23 = cumsum(RCL_2023),
         acum_24 = cumsum(RCL_2024),
         proj_acum = cumsum(Projeção_RCL)) %>% 
  add_column(col_space = NA, .name_repair = "universal") %>%
  add_column(col_space2 = NA, .name_repair = "universal") %>%
  select(data, RCL_2023, RCL_2024, col_space,acum_23, acum_24, 
         col_space2, Projeção_RCL, proj_acum) %>% 
  add_column(col_space3 = NA, .name_repair = "universal") %>% 
  mutate(dif_mes = (RCL_2024/RCL_2023 - 1)*100,
         dif_acum = (acum_24/acum_23 - 1)*100,
         col_space4 = NA,
         dif_proj = RCL_2024 - Projeção_RCL,
         dif_proj_acum = acum_24 - proj_acum,
         data1 = tools::toTitleCase(format(as.Date(data), "%B")))

# CRIANDO OS VALORES PARA OS BLOCOS --------------------------------------------
bloco1 <- RLT |> 
  select(acum_24) |> 
  drop_na() |> 
  tail(1) |> 
  mutate(acum_24 = round(acum_24/1000,2)) |> 
  pull()

bloco2 <- RLT %>%
  select(acum_24, proj_acum) %>%
  drop_na() %>%  # Remove todas as linhas que contêm NA
  select(proj_acum) %>%  # Seleciona a coluna proj_acum
  tail(1) %>%  # Seleciona a última linha
  mutate(proj_acum = round(proj_acum / 1000, 2)) %>%  # Arredonda e converte a coluna proj_acum
  pull()

bloco3 <- round(bloco1 - bloco2, 2)

bloco4 <- RLT |> 
  select(proj_acum) |> 
  tail(1) |> 
  mutate(proj_acum = round(proj_acum/1000,2)) |> 
  pull()

# EDITANDO A TABELA DA RTL------------------------------------------------------
tabela_acumulado <- RLT %>%
  mutate(data = data1) %>% 
  select(-data1) %>% 
  flextable() %>% 
  border_remove() %>%
  colformat_double(j = c("RCL_2023", "RCL_2024", 'acum_23', 'acum_24',
                         'Projeção_RCL', 'proj_acum', 'dif_proj', 'dif_proj_acum'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 0, 
                   na_str = "--") %>% 
  colformat_double(j = c('dif_mes', 'dif_acum', 'dif_proj', 'dif_proj_acum'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>%
  
  colformat_double(j = c('dif_mes', 'dif_acum'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--",
                   suffix = " %") %>% 
  
  
  set_header_labels(values = c('Arrecadação',"2023", "2024",'', '2023', '2024','',
                               "Mensal", "Acumulado",' ', "Mensal", "Acumulado",
                               '   ',"Mensal", "Acumulado")) %>% 
  bg(., 
     part = "header", 
     bg = cor1[2]) %>% 
  style( pr_t = fp_text_default(
    bold = F,
    color = cor1[3]
  ),
  part = 'header') %>% 
  bg(., i= c(2,4,6,8,10,12), 
     part = "body", 
     bg = cor1[1]) %>% 
  
  color( ~ dif_mes < 0, ~ dif_mes,  color = cor1[4]) %>% 
  color( ~ dif_acum < 0, ~ dif_acum,  color = cor1[4]) %>% 
  color( ~ dif_proj < 0, ~ dif_proj,  color = cor1[4]) %>% 
  color( ~ dif_proj_acum < 0, ~ dif_proj_acum,  color = cor1[4]) %>% 
  
  add_header_row(values = c('Arrecadação', 'Mensal', '  ', "Acumulado (Ano)",
                            '   ', "Projeções", '    ', 'Diferença (%) - Igual periodo',
                            ' ', 'Diferença em R$ (Real./24) - (Proj./24)'), 
                 colwidths = c(1,2,1,2,1,2,1,2,1,2)) %>% 
  
  merge_at(i = 1:2, j = 1, part = "header") %>% 
  align(i = c(1,2), j = NULL, align = "center", part = "header") %>% 
  hline(i = 1, j = c(2,3,5,6,8,9,11,12,14,15), part = "header", 
        border =  std_border) %>% 
  width(j = c(4,7,10,13), width = .2, unit = 'cm') %>% 
  width(j = 1, width = 3.6, unit = 'cm') %>% 
  width(j = c(2,3,5,6,8,9,11,12,14,15), width = 2.6, unit = 'cm') 




#################################################################

