# # variavies para cada categoria
variaveis <- c("COMBUSTÍVEL", "COMÉRCIO ATACADISTA E DISTRIBUIDOR",
               "COMÉRCIO VAREJISTA", 'COMUNICAÇÃO','ENERGIA ELÉTRICA',
               'EXTRATOR MINERAL OU FÓSSIL', 'INDÚSTRIA', 'OUTROS', 
               'PRESTAÇÃO DE SERVIÇO','PRODUÇÃO AGROPECUÁRIA')

funcao_graf <- function(filtro_base = NULL, filtro_projecao = NULL) {
  tabela_teste <- ICMS_base %>% 
    mutate(mes = month(data)) %>% 
    filter(Ano >= 2023,
           Grupo == filtro_base) %>% 
    group_by(Ano, Grupo, mes) %>% 
    summarise(Valor = sum(Valor)/1000000) %>% 
    pivot_wider(names_from = Ano, values_from = Valor) %>% 
    setNames(c('Grupo', 'mes', 'acum_23', 'acum_24')) %>% 
    bind_cols(projecao_ICMS %>%
                filter(ano == 2024,
                       grepl(filtro_projecao, Setor)) %>% 
                group_by(mes, Mes) %>% 
                summarise(valor = sum(valor)) %>% 
                mutate(valor = valor/1000000))
  
  tabela_teste$acum_23 <- cumsum(tabela_teste$acum_23)
  tabela_teste$acum_24 <- cumsum(tabela_teste$acum_24)
  tabela_teste$acum_proj <- cumsum(tabela_teste$valor)
  
  
  
  fig1 <- tabela_teste %>% 
    ggplot()+
    geom_line(aes(x = Mes, y = acum_23*1000000, color = "Acumulado 2023", 
                  linetype = "Acumulado 2023"), size=0.5) +
    
    geom_line(aes(x = Mes, y = acum_24*1000000, color = "Acumulado 2024", 
                  linetype = "Acumulado 2024"), size=1) +
    
    geom_line(aes(x = Mes, y = acum_proj*1000000, color = "Projeção 2024", 
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
      legend.title = element_blank(),
      legend.position = "bottom"
    )
  return(fig1)
}

fig1 <- funcao_graf(variaveis[1], variaveis[1]) +
  labs(title = "COMBUSTÍVEL") +
  theme(plot.title = element_text(hjust = 0.5))


fig2 <- funcao_graf(variaveis[2], variaveis[2]) +
  labs(title = "ATAC. E DISTRIB.",
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


fig3 <- funcao_graf(variaveis[3], variaveis[3]) +
  labs(title = "VAREJISTA",
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


fig4 <- funcao_graf(variaveis[4], variaveis[4]) +
  labs(title = "COMUNICAÇÃO",
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


fig5 <- funcao_graf(variaveis[5], variaveis[5]) +
  labs(title = "ENERGIA ELÉTRICA",
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


fig6 <- funcao_graf(variaveis[6], variaveis[6]) +
  labs(title = "EXT. MIN. OU FÓSSIL") +
  theme(plot.title = element_text(hjust = 0.5))


fig7 <- funcao_graf(variaveis[7], variaveis[7]) +
  labs(title = "INDÚSTRIA",
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


fig8 <- funcao_graf(variaveis[8], 'OUTRAS') +
  labs(title = "OUTRAS",
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


fig9 <- funcao_graf(variaveis[9], variaveis[9]) +
  labs(title = "SERVIÇO",
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


fig10 <- funcao_graf(variaveis[10], variaveis[10]) +
  labs(title = "AGROPECUÁRIA",
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))

fig.allg <- ggarrange(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, fig9, fig10, 
                      ncol = 5, nrow = 2, common.legend = T, legend = "bottom")




setwd("./../")