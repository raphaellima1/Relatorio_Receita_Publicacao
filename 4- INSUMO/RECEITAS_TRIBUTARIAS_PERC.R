#------------- fig 1 - COMBUSTÍVEL ------------------
index <- c(1:7,10,8:9)

tabela_COMB <- ICMS_base %>% 
  mutate(mes = month(data)) %>% 
  filter(Ano >= 2023,
         mes <= month(data_fim)) %>% 
  group_by(Ano, Grupo) %>% 
  summarise(Valor = sum(Valor)/1000000) %>% 
  pivot_wider(names_from = Ano, values_from = Valor) %>% 
  setNames(c('Grupo', 'acum_23', 'acum_24')) %>% 
  
  bind_cols(ICMS_base %>% 
              mutate(mes = month(data)) %>% 
              filter(Ano == 2023,
                     mes == month(data_fim)) %>% 
              group_by(Grupo) %>% 
              summarise(Valor = sum(Valor)/1000000) %>% 
              select(Valor) %>% 
              setNames('mes_23')) %>% 
  
  bind_cols(ICMS_base %>% 
              mutate(mes = month(data)) %>% 
              filter(Ano == 2024,
                     mes == month(data_fim)) %>% 
              group_by(Grupo) %>% 
              summarise(Valor = sum(Valor)/1000000) %>% 
              select(Valor) %>% 
              setNames('mes_24')) %>% 
  
  bind_cols(projecao_ICMS %>%
              filter(ano == 2024,
                     mes <= month(data_fim)) %>% 
              group_by(Setor) %>% 
              summarise(valor = sum(valor)/1000000) %>% 
              mutate(n_Grupo = case_when(
                Setor == "ENERGIA ELÉTRICA (CONS. LIVRE)" ~ 'ENERGIA ELÉTRICA',
                Setor == "OUTRAS" ~ 'OUTROS',
                TRUE ~ Setor)) %>%
              group_by(n_Grupo) %>% 
              summarise(valor = sum(valor)) %>% 
              select(valor) %>% 
              setNames('proj_acum')) %>% 
  
  bind_cols(projecao_ICMS %>%
              filter(ano == 2024,
                     mes == month(data_fim)) %>% 
              group_by(Setor) %>% 
              summarise(valor = sum(valor)/1000000) %>% 
              mutate(n_Grupo = case_when(
                Setor == "ENERGIA ELÉTRICA (CONS. LIVRE)" ~ 'ENERGIA ELÉTRICA',
                Setor == "OUTRAS" ~ 'OUTROS',
                TRUE ~ Setor)) %>%
              group_by(n_Grupo) %>% 
              summarise(valor = sum(valor)) %>% 
              select(valor) %>% 
              setNames('proj_me')) %>% 
  add_column(col_space1 = NA, .name_repair = "universal") %>% 
  add_column(col_space2 = NA, .name_repair = "universal") %>% 
  select(Grupo, mes_23, mes_24, col_space1, acum_23, 
         acum_24,col_space2, proj_acum, proj_me) %>% 
  mutate(index_ordem = index) %>% 
  arrange(index) %>% 
  select(-index_ordem) |> 
  adorn_totals()  %>%   
  add_column(col_space = NA, .name_repair = "universal") %>%
  mutate(dif_mes = mes_24 - proj_me,
         dif_acum = acum_24 - proj_acum,
         crescimento = ((acum_24 - acum_23)/acum_23)*100)


tabela_COM_ICMS <- tabela_COMB %>%
  select(-crescimento) |> 
  flextable() %>% 
  border_remove() %>%
  colformat_double(j = c("mes_23", "mes_24", 'acum_23', 'acum_24',
                         "proj_acum", "proj_me"),
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
    color = cor1[3]
  ),
  part = 'header') %>% 
  bg(., i= c(1,3,5,7,9), 
     part = "body", 
     bg = cor1[1]) %>% 
  
  bg(., i= ~ Grupo == "Total", 
     part = "body", 
     bg = cor1[2]) %>%
  
  style(i =  ~ Grupo == "Total", 
        pr_t = fp_text_default(
          bold = F,
          color = cor1[3]
        )) %>% 
  color( ~ dif_mes < 0, ~ dif_mes, color = cor1[4]) %>% 
  color( ~ dif_acum < 0, ~ dif_acum, color = cor1[4]) %>% 
  hline(i = c(10,11), part = "body", 
        border =  std_border) %>% 
  add_header_row(values = c('Arrecadação', 'Mensal', '  ', "Acumulado (Ano)",
                            '   ', "Projeções", '    ', 
                            'Diferença em R$ (Real./24) - (Proj./24)'), 
                 colwidths = c(1,2,1,2,1,2,1,2)) %>% 
  #merge_at(i = 1:2, j = 1, part = "header") %>% 
  merge_at(i = 1:2, j = c(1), part = "header") %>% 
  align(i = 1, j = NULL, align = "center", part = "header") %>% 
  hline(i = 1, j = c(2,3,5,6,8,9,11,12), part = "header", 
        border =  std_border) %>% 
  width(j = c(4,7,10), width = .2, unit = 'cm') %>% 
  width(j = 1, width = 4.0, unit = 'cm') %>% 
  width(j = c(2,3,5,6,8,9,11,12), width = 1.9, unit = 'cm') |> 
  width(j = c(9,12), width = 2.5, unit = 'cm')

index <- c(1:11)
# Definir o tamanho do gráfico
#options(repr.plot.width = 3.4, repr.plot.height = 5)
fig6 <- tabela_COMB %>% 
  mutate(index_ordem = index) %>% 
  select(Grupo, crescimento, index_ordem) %>%
  ggplot(aes(x = crescimento, y = reorder(Grupo, desc(index)), fill = crescimento > 0)) +  # Reorder para ordenar os grupos
  geom_col() +
  geom_label(aes(label = sub("\\.", ",", sprintf("%.2f", crescimento)),
                 x = ifelse(crescimento > 0, crescimento - 2.5, crescimento + 2.5)),  # Ajusta a posição dos rótulos
             color = "black", fill = "white", size = 3,label.size = 0.1) +
  scale_fill_manual(values = c(cor2[3], cor2[2]), guide = FALSE) +
  theme_hc() + 
  labs(title = "Acumulado mês / Acumulado mês ano anterior",
       x = "Variação (%)", 
       y = "Grupo de ICMS") +
  theme(text=element_text(size=8.5),
        plot.title = element_text(hjust = 0.9),
        legend.title = element_blank(),
        legend.position = "bottom"
  )

