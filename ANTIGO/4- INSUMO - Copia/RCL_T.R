RCL <- realizado %>% 
  filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
  select(c(1, starts_with(glue('{year(Sys.Date())}')))) %>% 
  mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
  pivot_longer(cols =  2:13) %>% 
  mutate(data = ymd(paste0(name, '01'))) %>% 
  setNames(c('RCL', 'name', 'RCL_2024', 'data')) %>% 
  bind_cols(projeção %>% 
              filter(ESPECIFICAÇÃO == 'RECEITA CORRENTE LÍQUIDA (III) = (I-II)') %>% 
              select(c(1, starts_with(glue('{year(Sys.Date())}')))) %>% 
              mutate(across(2:13, as.numeric)) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01'))) %>% 
              select(value) %>% 
              setNames('Projeção_RCL')) %>% 
  bind_cols(realizado %>% 
              filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
              select(c(1, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
              mutate(across(2:13, as.numeric)) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01'))) %>% 
              select(value) %>% 
              setNames('RCL_2023')) %>% 
  bind_cols(realizado %>% 
              filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
              select(c(1, starts_with(glue('{year(Sys.Date())}')))) %>% 
              mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01'))) %>% 
              setNames(c('RCL', 'name', 'RCL_2024', 'data')) %>% 
              bind_rows(realizado %>% 
                          filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
                          select(c(1, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
                          mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
                          pivot_longer(cols =  2:13) %>% 
                          mutate(data = ymd(paste0(name, '01'))) %>% 
                          setNames(c('RCL', 'name', 'RCL_2024', 'data'))) %>% 
              bind_rows(realizado %>% 
                          filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
                          select(c(1, starts_with(glue('{year(Sys.Date())-2}')))) %>% 
                          mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
                          pivot_longer(cols =  2:13) %>% 
                          mutate(data = ymd(paste0(name, '01'))) %>% 
                          setNames(c('RCL', 'name', 'RCL_2024', 'data'))) %>% 
              arrange(data) %>% 
              mutate(RCL12 = rollsum(RCL_2024, 12, fill = NA, align = "right"),
                     ano = year(data)) %>% 
              filter(ano == 2024) %>% 
              select(data, RCL12) %>% 
              bind_cols(realizado %>% 
                          filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
                          select(c(1, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
                          mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
                          pivot_longer(cols =  2:13) %>% 
                          mutate(data = ymd(paste0(name, '01'))) %>% 
                          setNames(c('RCL', 'name', 'RCL_2024', 'data')) %>% 
                          bind_rows(projeção %>% 
                                      filter(ESPECIFICAÇÃO == 'RECEITA CORRENTE LÍQUIDA (III) = (I-II)') %>% 
                                      select(c(1, starts_with(glue('{year(Sys.Date())}')))) %>% 
                                      mutate(across(2:13, as.numeric)) %>% 
                                      pivot_longer(cols =  2:13) %>% 
                                      mutate(data = ymd(paste0(name, '01')),
                                             CAMPO = as.character(CAMPO)) %>% 
                                      setNames(c('RCL', 'name', 'RCL_2024', 'data'))) %>% 
                          arrange(data) %>% 
                          mutate(RCL12 = rollsum(RCL_2024, 12, fill = NA, align = "right"),
                                 ano = year(data)) %>% 
                          filter(ano == 2024) %>% 
                          select(RCL12)) %>% 
              setNames(c('data', 'RCL_24', 'PROJ_24')) %>% 
              bind_cols(realizado %>% 
                          filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
                          select(c(1, starts_with(glue('{year(Sys.Date())}')))) %>% 
                          mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
                          pivot_longer(cols =  2:13) %>% 
                          mutate(data = ymd(paste0(name, '01'))) %>% 
                          setNames(c('RCL', 'name', 'RCL_2024', 'data')) %>% 
                          bind_rows(realizado %>% 
                                      filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
                                      select(c(1, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
                                      mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
                                      pivot_longer(cols =  2:13) %>% 
                                      mutate(data = ymd(paste0(name, '01'))) %>% 
                                      setNames(c('RCL', 'name', 'RCL_2024', 'data'))) %>% 
                          bind_rows(realizado %>% 
                                      filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
                                      select(c(1, starts_with(glue('{year(Sys.Date())-2}')))) %>% 
                                      mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
                                      pivot_longer(cols =  2:13) %>% 
                                      mutate(data = ymd(paste0(name, '01'))) %>% 
                                      setNames(c('RCL', 'name', 'RCL_2024', 'data'))) %>% 
                          arrange(data) %>% 
                          mutate(RCL_23 = rollsum(RCL_2024, 12, fill = NA, align = "right"),
                                 ano = year(data)) %>% 
                          filter(ano == 2023) %>% 
                          select(RCL_23)) %>% 
              select(RCL_23, RCL_24, PROJ_24) %>% 
              setNames(c('acum_23', 'acum_24', 'proj_acum'))) %>% 
  add_column(col_space = NA, .name_repair = "universal") %>%
  add_column(col_space2 = NA, .name_repair = "universal") %>%
  select(data, RCL_2023, RCL_2024, col_space,acum_23, acum_24, 
         col_space2, Projeção_RCL, proj_acum) %>% 
  mutate(across(where(is.numeric), ~ . / 1000000)) %>% 
  add_column(col_space3 = NA, .name_repair = "universal") %>% 
  mutate(dif_mes = (RCL_2024/RCL_2023 - 1)*100,
         dif_acum = (acum_24/acum_23 - 1)*100,
         col_space4 = NA,
         dif_proj = RCL_2024 - Projeção_RCL,
         dif_proj_acum = acum_24 - proj_acum,
         data1 = format(as.Date(data), "%B"))
  
set_flextable_defaults(
  font.size = 11, font.family = "Calibri",
  font.color = "#333333",
  table.layout = "fixed",
  border.color = "gray",
  padding.top = 3, padding.bottom = 3,
  padding.left = 4, padding.right = 4)

std_border <- fp_border(color = "black", width = 1.1)

tabela_acumulado <- RCL %>%
  mutate(data = data1) %>% 
  select(-data1) %>% 
  flextable() %>% 
  border_remove() %>%
  colformat_double(j = c("RCL_2023", "RCL_2024", 'acum_23', 'acum_24',
                         'Projeção_RCL', 'proj_acum'),
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
                               "Mensal", "Acumulado 12M",' ', "Mensal", "Acumulado",
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
  
  color( ~ dif_mes < 0, ~ dif_mes,  color = cor1[4] ) %>% 
  color( ~ dif_acum < 0, ~ dif_acum,  color = cor1[4] ) %>% 
  color( ~ dif_proj < 0, ~ dif_proj,  color = cor1[4] ) %>% 
  color( ~ dif_proj_acum < 0, ~ dif_proj_acum,  color = cor1[4]) %>% 
  
  add_header_row(values = c('Arrecadação', 'Mensal', '  ', "Acumulado (12 meses)",
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

