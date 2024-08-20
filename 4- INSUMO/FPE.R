
# CARREGANDO OS DADOS DO FPE ---------------------------------------------------
FPE <- realizado %>% 
  filter(Column1 == 'Cota-Parte do FPE') %>% 
  select(c(2, starts_with(glue('{year(Sys.Date())}')))) %>% 
  mutate(across(2:13, ~ na_if(as.numeric(.), 0.00)/1000000)) %>% 
  pivot_longer(cols =  2:13) %>% 
  mutate(data = ymd(paste0(name, '01'))) %>% 
  setNames(c('FPE', 'name', 'RCL_2024', 'data')) %>% 
    bind_cols(realizado %>% 
              filter(Column1 == 'Cota-Parte do FPE') %>% 
              select(c(2, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
              mutate(across(2:13, as.numeric)/1000000) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01'))) %>% 
              select(value) %>% 
              setNames('RCL_2023')) %>% 
  mutate(acum_23 = cumsum(RCL_2023),
         acum_24 = cumsum(RCL_2024)) %>% 
  add_column(col_space = NA, .name_repair = "universal") %>%
  add_column(col_space2 = NA, .name_repair = "universal") %>%
  select(data, RCL_2023, RCL_2024, col_space,acum_23, acum_24, 
         col_space2) 

# CARREGANDO OS DADOS DO FUNDEB
FUNDEB <- realizado %>% 
  filter(Column1 == 'Transferências do FUNDEB') %>% 
  select(c(2, starts_with(glue('{year(Sys.Date())}')))) %>% 
  mutate(across(2:13, ~ na_if(as.numeric(.), 0.00)/1000000)) %>% 
  pivot_longer(cols =  2:13) %>% 
  mutate(data = ymd(paste0(name, '01'))) %>% 
  setNames(c('FUNDEB', 'name', 'RCL_2024', 'data')) %>% 
  bind_cols(realizado %>% 
              filter(Column1 == 'Transferências do FUNDEB') %>% 
              select(c(1, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
              mutate(across(2:13, as.numeric)/1000000) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01'))) %>% 
              select(value) %>% 
              setNames('RCL_2023')) %>% 
  mutate(acum_23 = cumsum(RCL_2023),
         acum_24 = cumsum(RCL_2024)) %>% 
  add_column(col_space = NA, .name_repair = "universal") %>%
  select(data, RCL_2023, RCL_2024, col_space,acum_23, acum_24)
 


# CRIANDO A TABELA DO FPE ------------------------------------------------------
tabela_acumulado <- FPE %>%
  bind_cols(FUNDEB |> 
              select(-data) |> 
              setNames(c('fundeb_2023', 'fundeb_2024', 'space',
                         'acum_fundeb_23', 'acum_fundeb_24'))) |> 
  mutate(data = format(data, '%B')) %>% 

  flextable() %>% 
  border_remove() %>%
  colformat_double(j = c("RCL_2023", "RCL_2024", 'acum_23', 'acum_24',
                         'fundeb_2023', 'fundeb_2024', 
                         'acum_fundeb_23', 'acum_fundeb_24'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 0, 
                   na_str = "--") %>% 
  
  
  set_header_labels(values = c('Arrecadação',"2023", "2024",'', '2023', '2024','',
                               "2023", "2024",'', "2023", "2024")) %>% 
  bg(., 
     part = "header", 
     bg = cor1[2]) %>% 
  flextable::style( pr_t = fp_text_default(
    bold = F,
    color = cor1[1]
  ),
  part = 'header') %>% 
  bg(., i= c(2,4,6,8,10,12), 
     part = "body", 
     bg = cor1[1]) %>% 
  
  add_header_row(values = c('Arrecadação', 'Mensal', '  ', "Acumulado (Ano)",
                            '   ', "Mensal", '    ', 'Acumulado (Ano)'), 
                 colwidths = c(1,2,1,2,1,2,1,2)) |> 
  
  add_header_row(values = c('Arrecadação', 'FPE', '','FUNDEB'), 
                 colwidths = c(1,5,1,5)) |> 
  
  merge_at(i = 1:3, j = 1, part = "header") %>% 
  align(i = 1:3, j = NULL, align = "center", part = "header") %>% 
  hline(i = 1, j = c(2:6,8:12), part = "header", 
        border =  std_border) %>% 
  hline(i = 2, j = c(2,3,5,6,8,9,11,12), part = "header", 
        border =  std_border) |> 
  width(j = c(4,7,10), width = .2, unit = 'cm') %>% 
  width(j = 1, width = 2.6, unit = 'cm') %>% 
  width(j = c(2,3,5,6,8,9,11,12), width = 1.9, unit = 'cm')

# CRIANDO O GRÁFICO DO FPE -----------------------------------------------------
 fig1 <- FPE %>% 
  mutate(fant_24 = sub("\\.", ",", round(acum_24, digits = 0))) |>
  ggplot()+
  
  geom_line(aes(x = data, y = acum_24*1000000, color = "Acumulado 2024", 
                linetype = "Acumulado 2024"), size=0.5) +
  
  geom_line(aes(x = data, y = acum_23*1000000, color = "Acumulado 2023", 
                linetype = "Acumulado 2023"), size=0.5) +
  
  geom_label(aes(x = data, y = acum_24*1000000, label = fant_24),vjust = -0.8,colour = cor2[1], size = 3)+
  
  labs(x = "  ", 
       y = "Valores em R$", 
       title = "Cota-Parte do FPE",
       linetype = "Variable",
       color = "Variable") +
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c('Acumulado 2023', "Acumulado 2024"),
                     values = c("Acumulado 2024"= cor2[1],
                                "Acumulado 2023"= cor2[2]),
                     name="Legenda:")+
  scale_linetype_manual(breaks = c('Acumulado 2023', "Acumulado 2024"),
                        values = c("Acumulado 2024"='solid',
                                   "Acumulado 2023"='solid'), 
                        name="Legenda:")+
  
  labs(fill = "Title") +
  theme_hc() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

fig2 <- FUNDEB %>% 
  mutate(fant_24 = sub("\\.", ",", round(acum_24, digits = 0))) |>
  ggplot()+

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
  scale_color_manual(breaks = c('Acumulado 2023', "Acumulado 2024"),
                     values = c("Acumulado 2024"= cor2[1],
                                "Acumulado 2023"= cor2[2]), 
                     name="Legenda:")+
  scale_linetype_manual(breaks = c('Acumulado 2023', "Acumulado 2024"),
                        values = c("Acumulado 2024"='solid',
                                   "Acumulado 2023"='solid'), 
                        name="Legenda:")+
  
  labs(fill = "Title") +
  theme_hc() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

fig.allg <- ggarrange(fig1, fig2, ncol = 1, nrow = 2, 
                      common.legend = T,legend = 'bottom')
