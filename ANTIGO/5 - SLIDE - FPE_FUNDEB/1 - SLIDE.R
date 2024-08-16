FPE <- realizado %>% 
  filter(Column1 == 'Cota-Parte do FPE') %>% 
  select(c(2, starts_with(glue('{year(Sys.Date())}')))) %>% 
  mutate(across(2:13, ~ na_if(as.numeric(.), 0.00)/1000000)) %>% 
  pivot_longer(cols =  2:13) %>% 
  mutate(data = ymd(paste0(name, '01'))) %>% 
  setNames(c('FPE', 'name', 'RCL_2024', 'data')) %>% 
  bind_cols(projeção1 %>% 
              filter(Colunas1 == 'Cota-Parte do FPE') %>% 
              select(c(3, starts_with(glue('{year(Sys.Date())}')))) %>% 
              mutate(across(2:13, as.numeric)/1000000) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01'))) %>% 
              select(value) %>% 
              setNames('Projeção_RCL')) %>% 
  bind_cols(realizado %>% 
              filter(Column1 == 'Cota-Parte do FPE') %>% 
              select(c(2, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
              mutate(across(2:13, as.numeric)/1000000) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01'))) %>% 
              select(value) %>% 
              setNames('RCL_2023')) %>% 
  mutate(acum_23 = cumsum(RCL_2023),
         acum_24 = cumsum(RCL_2024),
         proj_acum = cumsum(Projeção_RCL)) %>% 
  add_column(col_space = NA, .name_repair = "universal") %>%
  add_column(col_space2 = NA, .name_repair = "universal") %>%
  select(data, RCL_2023, RCL_2024, col_space,acum_23, acum_24, 
         col_space2, Projeção_RCL, proj_acum) %>% 
  add_column(col_space3 = NA, .name_repair = "universal") %>% 
  mutate(dif_mes = (RCL_2024 - Projeção_RCL),
         dif_acum = (acum_24 - proj_acum),
         data1 = format(as.Date(data), "%B"))
  
set_flextable_defaults(
  font.size = 10, font.family = "Calibri",
  font.color = "#333333",
  table.layout = "fixed",
  border.color = "gray",
  padding.top = 3, padding.bottom = 3,
  padding.left = 4, padding.right = 4)

std_border <- fp_border(color = "black", width = 1.1)

tabela_acumulado <- FPE %>%
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
  
  colformat_double(j = c('dif_mes', 'dif_acum'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>% 


  set_header_labels(values = c('Arrecadação',"2023", "2024",'', '2023', '2024','',
                               "Mensal", "Acumulado",'', "Mensal", "Acumulado")) %>% 
  bg(., 
     part = "header", 
     bg = "gray70") %>% 
  style( pr_t = fp_text_default(
    bold = T
    #,color = "black"
  ),
  part = 'header') %>% 
  bg(., i= c(2,4,6,8,10,12), 
     part = "body", 
     bg = "#E3E7E7") %>% 
  
  color( ~ Projeção_RCL < 0, ~ Projeção_RCL,  color = 'red' ) %>% 
  color( ~ proj_acum < 0, ~ proj_acum,  color = 'red' ) %>% 
  color( ~ dif_mes < 0, ~ dif_mes,  color = 'red' ) %>% 
  color( ~ dif_acum < 0, ~ dif_acum,  color = 'red' ) %>% 

  add_header_row(values = c('Arrecadação', 'Mensal', '  ', "Acumulado (Ano)",
                            '   ', "Projeções", '    ', 'Diferença em R$ (Real./24) - (Proj./24)'), 
                 colwidths = c(1,2,1,2,1,2,1,2)) %>% 

  merge_at(i = 1:2, j = 1, part = "header") %>% 
  align(i = 1, j = NULL, align = "center", part = "header") %>% 
  hline(i = 1, j = c(2,3,5,6,8,9,11,12), part = "header", 
        border =  std_border) %>% 
  width(j = c(4,7,10), width = .2, unit = 'cm') %>% 
  width(j = 1, width = 2.6, unit = 'cm') %>% 
  width(j = c(2,3,5,6,8,9,11,12), width = 2, unit = 'cm') 


