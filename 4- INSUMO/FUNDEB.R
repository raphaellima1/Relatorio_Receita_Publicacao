
# CARREGANDO OS DADOS DO FUNDEB
FUNDEB <- realizado %>% 
  filter(Column1 == 'Transferências do FUNDEB') %>% 
  select(c(2, starts_with(glue('{year(Sys.Date())}')))) %>% 
  mutate(across(2:13, ~ na_if(as.numeric(.), 0.00)/1000000)) %>% 
  pivot_longer(cols =  2:13) %>% 
  mutate(data = ymd(paste0(name, '01'))) %>% 
  setNames(c('FUNDEB', 'name', 'RCL_2024', 'data')) %>% 
  bind_cols(projeção1 %>% 
              filter(Colunas1 == 'Transferências do FUNDEB') %>% 
              select(c(4, starts_with(glue('{year(Sys.Date())}')))) %>% 
              mutate(across(2:13, as.numeric)/1000000) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01'))) %>% 
              select(value) %>% 
              setNames('Projeção_RCL')) %>% 
  bind_cols(realizado %>% 
              filter(Column1 == 'Transferências do FUNDEB') %>% 
              select(c(1, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
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
         data1 = tools::toTitleCase(format(as.Date(data), "%B")))

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
fig1 <- FUNDEB %>% 
  mutate(fant_24 = sub("\\.", ",", round(acum_24, digits = 0))) |>
  ggplot()+
  geom_ribbon(aes(x = data, ymin = band_inf * 1000000, 
                  ymax = band_sup * 1000000), 
              fill = "grey80", alpha = 0.5) +
  
  geom_line(aes(x = data, y = proj_acum*1000000, color = "Projeção 2024", 
                linetype = "Projeção 2024"), size=0.5) +
  
  geom_line(aes(x = data, y = acum_23*1000000, color = "Acumulado 2023", 
                linetype = "Acumulado 2023"), size=0.5) +
  
  geom_line(aes(x = data, y = acum_24*1000000, color = "Acumulado 2024", 
                linetype = "Acumulado 2024"), size=1) +
  
  geom_label(aes(x = data, y = acum_24*1000000, label = fant_24),vjust = -0.8,colour = cor2[1], size = 3)+
  
  labs(x = "  ", 
       y = "Valores em R$", 
       title = "Transf. do FUNDEB",
       linetype = "Variable",
       color = "Variable") +
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c('Acumulado 2023', "Acumulado 2024", 'Projeção 2024'),
                     values = c("Acumulado 2024"= cor2[1],
                                "Acumulado 2023"= cor2[2],
                                "Projeção 2024"= cor2[3]), 
                     name="Legenda:")+
  scale_linetype_manual(breaks = c('Acumulado 2023', "Acumulado 2024", 'Projeção 2024'),
                        values = c("Acumulado 2024"='solid',
                                   "Acumulado 2023"='solid',
                                   "Projeção 2024"='longdash'), 
                        name="Legenda:")+
  
  labs(fill = "Title") +
  theme_hc() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
