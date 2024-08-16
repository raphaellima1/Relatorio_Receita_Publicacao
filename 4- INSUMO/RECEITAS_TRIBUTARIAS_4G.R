tabela_ICMS <- receitas_base %>% 
  mutate(mes = month(data)) %>% 
  filter(Ano >= 2023, 
         Tipo =='ICMS') %>% 
  group_by(Ano, Tipo, mes) %>% 
  summarise(Valor = sum(Valor)) %>% 
  pivot_wider(names_from = Ano, values_from = Valor) %>% 
  setNames(c('Tipo', 'mes', 'acum_23', 'acum_24')) %>% 
  bind_cols(projecao_base %>% 
              filter(DESCRIÇÃO == 'ICMS') %>%
              select(name, Valor, mes) %>% 
              mutate(Valor = Valor) %>% 
              mutate(data = as.Date(paste0('2024-', mes, '-', '01'))) %>% 
              select(Valor, data) %>% 
              setNames(c('acum_proj', 'data')))

tabela_ICMS$acum_23 <- cumsum(tabela_ICMS$acum_23)
tabela_ICMS$acum_24 <- cumsum(tabela_ICMS$acum_24)
tabela_ICMS$acum_proj <- cumsum(tabela_ICMS$acum_proj)

fig1 <- tabela_ICMS %>% 
  ggplot()+
  geom_line(aes(x = data, y = acum_23, color = "Acumulado 2023", 
                linetype = "Acumulado 2023"), size=0.5) +
  
  geom_line(aes(x = data, y = acum_24, color = "Acumulado 2024", 
                linetype = "Acumulado 2024"), size=1) +
  
  geom_line(aes(x = data, y = acum_proj, color = "Projeção 2024", 
                linetype = "Projeção 2024"), size=0.5) +
  
  labs(x = "  ", 
       y = "Valores em Reais (R$)", 
       title = ' ',
       linetype = "Variable",
       color = "Variable") +
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c('Acumulado 2023', "Acumulado 2024", 'Projeção 2024'),
                     values = c("Acumulado 2024"=cor2[1],
                                "Acumulado 2023"=cor2[2],
                                "Projeção 2024"=cor2[3]) 
                     # name="Legenda:"
  )+
  scale_linetype_manual(breaks = c('Acumulado 2023', "Acumulado 2024", 'Projeção 2024'),
                        values = c("Acumulado 2024"='solid',
                                   "Acumulado 2023"='solid',
                                   "Projeção 2024"='longdash') 
                        # name="Legenda:"
  )+
  
  labs(fill = "Title",
       title = "ICMS") +
  theme_hc() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position="none"
  )

fig1

######### fig 2 IPVA

tabela_IPVA <- receitas_base %>% 
  mutate(mes = month(data)) %>% 
  filter(Ano >= 2023, 
         Tipo =='IPVA') %>% 
  group_by(Ano, Tipo, mes) %>% 
  summarise(Valor = sum(Valor)) %>% 
  pivot_wider(names_from = Ano, values_from = Valor) %>% 
  setNames(c('Tipo', 'mes', 'acum_23', 'acum_24')) %>% 
  bind_cols(projecao_base %>% 
              filter(DESCRIÇÃO == 'IPVA') %>%
              select(name, Valor, mes) %>% 
              mutate(Valor = Valor) %>% 
              mutate(data = as.Date(paste0('2024-', mes, '-', '01'))) %>% 
              select(Valor, data) %>% 
              setNames(c('acum_proj', 'data')))

tabela_IPVA$acum_23 <- cumsum(tabela_IPVA$acum_23)
tabela_IPVA$acum_24 <- cumsum(tabela_IPVA$acum_24)
tabela_IPVA$acum_proj <- cumsum(tabela_IPVA$acum_proj)

fig2 <- tabela_IPVA %>% 
  ggplot()+
  geom_line(aes(x = data, y = acum_23, color = "Acumulado IPVA 2023", 
                linetype = "Acumulado IPVA 2023"), size=0.5) +
  
  geom_line(aes(x = data, y = acum_24, color = "Acumulado IPVA 2024", 
                linetype = "Acumulado IPVA 2024"), size=1) +
  
  geom_line(aes(x = data, y = acum_proj, color = "Projeção IPVA 2024", 
                linetype = "Projeção IPVA 2024"), size=0.5) +
  
  labs(x = "  ", 
       y = NULL, 
       title = ' ',
       linetype = "Variable",
       color = "Variable") +
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c('Acumulado IPVA 2023', "Acumulado IPVA 2024", 'Projeção IPVA 2024'),
                     values = c("Acumulado IPVA 2024"=cor2[1],
                                "Acumulado IPVA 2023"=cor2[2],
                                "Projeção IPVA 2024"=cor2[3]), 
                     name="Legenda:")+
  scale_linetype_manual(breaks = c('Acumulado IPVA 2023', "Acumulado IPVA 2024", 'Projeção IPVA 2024'),
                        values = c("Acumulado IPVA 2024"='solid',
                                   "Acumulado IPVA 2023"='solid',
                                   "Projeção IPVA 2024"='longdash'), 
                        name="Legenda:")+
  
  labs(fill = "Title",
       title = "IPVA") +
  theme_hc() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position="none"
  )

fig2

######### fig 3 ITCD

tabela_ITCD <- receitas_base %>% 
  mutate(mes = month(data)) %>% 
  filter(Ano >= 2023, 
         Tipo =='ITCD') %>% 
  group_by(Ano, Tipo, mes) %>% 
  summarise(Valor = sum(Valor)) %>% 
  pivot_wider(names_from = Ano, values_from = Valor) %>% 
  setNames(c('Tipo', 'mes', 'acum_23', 'acum_24')) %>% 
  bind_cols(projecao_base %>% 
              filter(DESCRIÇÃO == 'ITCD') %>%
              select(name, Valor, mes) %>% 
              mutate(Valor = Valor) %>% 
              mutate(data = as.Date(paste0('2024-', mes, '-', '01'))) %>% 
              select(Valor, data) %>% 
              setNames(c('acum_proj', 'data')))

tabela_ITCD$acum_23 <- cumsum(tabela_ITCD$acum_23)
tabela_ITCD$acum_24 <- cumsum(tabela_ITCD$acum_24)
tabela_ITCD$acum_proj <- cumsum(tabela_ITCD$acum_proj)

fig3 <- tabela_ITCD %>% 
  ggplot()+
  geom_line(aes(x = data, y = acum_23, color = "Acumulado ITCD 2023", 
                linetype = "Acumulado ITCD 2023"), size=0.5) +
  
  geom_line(aes(x = data, y = acum_24, color = "Acumulado ITCD 2024", 
                linetype = "Acumulado ITCD 2024"), size=1) +
  
  geom_line(aes(x = data, y = acum_proj, color = "Projeção ITCD 2024", 
                linetype = "Projeção ITCD 2024"), size=0.5) +
  
  labs(x = "  ", 
       y = "Valores em Reais (R$)", 
       title = ' ',
       linetype = "Variable",
       color = "Variable") +
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c('Acumulado ITCD 2023', "Acumulado ITCD 2024", 'Projeção ITCD 2024'),
                     values = c("Acumulado ITCD 2024"=cor2[1],
                                "Acumulado ITCD 2023"=cor2[2],
                                "Projeção ITCD 2024"=cor2[3]), 
                     name="Legenda:")+
  scale_linetype_manual(breaks = c('Acumulado ITCD 2023', "Acumulado ITCD 2024", 'Projeção ITCD 2024'),
                        values = c("Acumulado ITCD 2024"='solid',
                                   "Acumulado ITCD 2023"='solid',
                                   "Projeção ITCD 2024"='longdash'), 
                        name="Legenda:")+
  
  labs(fill = "Title",
       title = "ITCD") +
  theme_hc() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position="none"
  )

fig3

######### fig 4 FUNDEINFRA

tabela_FUNDEINFRA <- receitas_base %>% 
  mutate(mes = month(data)) %>% 
  filter(Ano >= 2023, 
         Tipo =='FUNDEINFRA') %>% 
  group_by(Ano, Tipo, mes) %>% 
  summarise(Valor = sum(Valor)) %>% 
  pivot_wider(names_from = Ano, values_from = Valor) %>% 
  setNames(c('Tipo', 'mes', 'acum_23', 'acum_24')) %>% 
  bind_cols(projecao_base %>% 
              filter(DESCRIÇÃO == 'Contribuição ao FUNDEINFRA') %>%
              select(name, Valor, mes) %>% 
              mutate(Valor = Valor) %>% 
              mutate(data = as.Date(paste0('2024-', mes, '-', '01'))) %>% 
              select(Valor, data) %>% 
              setNames(c('acum_proj', 'data')))

tabela_FUNDEINFRA$acum_23 <- cumsum(tabela_FUNDEINFRA$acum_23)
tabela_FUNDEINFRA$acum_24 <- cumsum(tabela_FUNDEINFRA$acum_24)
tabela_FUNDEINFRA$acum_proj <- cumsum(tabela_FUNDEINFRA$acum_proj)

fig4 <- tabela_FUNDEINFRA %>% 
  ggplot()+
  geom_line(aes(x = data, y = acum_23, color = "Acumulado FUNDEINFRA 2023", 
                linetype = "Acumulado FUNDEINFRA 2023"), size=0.5) +
  
  geom_line(aes(x = data, y = acum_24, color = "Acumulado FUNDEINFRA 2024", 
                linetype = "Acumulado FUNDEINFRA 2024"), size=1) +
  
  geom_line(aes(x = data, y = acum_proj, color = "Projeção FUNDEINFRA 2024", 
                linetype = "Projeção FUNDEINFRA 2024"), size=0.5) +
  
  labs(x = "  ", 
       y = NULL, 
       title = ' ',
       linetype = "Variable",
       color = "Variable") +
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c('Acumulado FUNDEINFRA 2023', "Acumulado FUNDEINFRA 2024", 'Projeção FUNDEINFRA 2024'),
                     values = c("Acumulado FUNDEINFRA 2024"=cor2[1],
                                "Acumulado FUNDEINFRA 2023"=cor2[2],
                                "Projeção FUNDEINFRA 2024"=cor2[3]), 
                     name="Legenda:")+
  scale_linetype_manual(breaks = c('Acumulado FUNDEINFRA 2023', "Acumulado FUNDEINFRA 2024", 'Projeção FUNDEINFRA 2024'),
                        values = c("Acumulado FUNDEINFRA 2024"='solid',
                                   "Acumulado FUNDEINFRA 2023"='solid',
                                   "Projeção FUNDEINFRA 2024"='longdash'), 
                        name="Legenda:")+
  
  labs(fill = "Title",
       title = "FUNDEINFRA") +
  theme_hc() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position="none"
  )

fig4

# matrix_1 = rbind(c(1,1,1,2,2,2),
#                  c(1,1,1,2,2,2),
#                  c(3,3,3,4,4,4),
#                  c(3,3,3,4,4,4),
#                  c(0,5,5,5,5,0))
#-------------------------------------------
# fig.allg <- grid.arrange(fig1, fig2, fig3, fig4,
#                          layout_matrix = rbind(c(1,2),
#                                                c(3,4),
#                                                c(5,5)
#                                                )
#                          )


fig.allg <- ggarrange(fig1, fig2, fig3, fig4, ncol = 2, nrow = 2, widths = c(1, 1, 1, 1, 0.05, 1), common.legend = T, legend = "bottom")