filtro <- c('ICMS', 'IPVA', 'Adicional de 2% ICMS', 'Contribuições ao PROTEGE',
            'Contribuição ao FUNDEINFRA', 'ITCD')


tabela_graf_total <- receitas_base %>% 
  mutate(mes = month(data)) %>%
  filter(Ano >= year(data_fim)-1) %>% 
  group_by(mes, Ano) %>% 
  summarise(Valor = sum(Valor)) %>% 
  pivot_wider(names_from = 'Ano', values_from = Valor) %>% 
  setNames(c('mes', 'ano_23', 'ano_24')) %>% 
  bind_cols(projecao_base %>% 
              filter(DESCRIÇÃO == 'TOTAL') %>%
              select(name, Valor) %>% 
              mutate(Valor = Valor)) %>% 
  mutate(data = as.Date(paste0('2024-', mes, '-', '01'))) 


tabela_graf_total$acum_23 <- cumsum(tabela_graf_total$ano_23)
tabela_graf_total$acum_24 <- cumsum(tabela_graf_total$ano_24)
tabela_graf_total$acum_proj <- cumsum(tabela_graf_total$Valor)

fig1 <- tabela_graf_total %>% 
  ggplot()+
  geom_line(aes(x = data, y = acum_23, color = "Acumulado 2023", 
                linetype = "Acumulado 2023"), size=0.5) +
  
  geom_line(aes(x = data, y = acum_24, color = "Acumulado 2024", 
                linetype = "Acumulado 2024"), size=1) +
  
  geom_line(aes(x = data, y = acum_proj, color = "Projeção 2024", 
                linetype = "Projeção 2024"), size=0.5) +
  
  labs(x = "  ", 
       y = "Valores em Reais (R$)", 
       title = "RECEITA TOTAL",
       linetype = "Variable",
       color = "Variable") +
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c('Acumulado 2023', "Acumulado 2024", 'Projeção 2024'),
    values = c("Acumulado 2024"="#3f3939",
               "Acumulado 2023"="#4a760b",
               "Projeção 2024"="#fc7768"), 
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
  
rm(filtro)

setwd("./../")
