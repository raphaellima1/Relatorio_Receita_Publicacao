


# CRIANDO A TABELA DO FUNDEB ---------------------------------------------------
tabela_acumulado <- FUNDEB %>%
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
     bg = cor1[2]) %>% 
  style( pr_t = fp_text_default(
    bold = F,
    color = cor1[1]
  ),
  part = 'header') %>% 
  bg(., i= c(2,4,6,8,10,12), 
     part = "body", 
     bg = cor1[1]) %>% 
  
  color( ~ Projeção_RCL < 0, ~ Projeção_RCL,  color = cor1[4]) %>% 
  color( ~ proj_acum < 0, ~ proj_acum,  color = cor1[4]) %>% 
  color( ~ dif_mes < 0, ~ dif_mes,  color = cor1[4]) %>% 
  color( ~ dif_acum < 0, ~ dif_acum,  color = cor1[4]) %>% 
  
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


# CALCULANDO OS INTERVALOS DE CONFIANÇA PARA O GRÁFICO -------------------------
FUNDEB_band <- FUNDEB %>%
  select(RCL_2024, Projeção_RCL) %>%
  drop_na() %>%
  mutate(dif = round(abs(RCL_2024/Projeção_RCL - 1),3)) %>%
  summarise(valor = sum(dif) / month(mes_atualizacao)) %>%
  pull() 

FUNDEB <- FUNDEB %>%
  mutate(band_inf = Projeção_RCL * (1 - FUNDEB_band),
         band_sup = Projeção_RCL * (1 + FUNDEB_band),
         mes = month(data)) |> 
  mutate(band_inf = cumsum(band_inf),
         band_sup = cumsum(band_sup))


# CRIANDO O GRÁFICO DO FUNDEB --------------------------------------------------

