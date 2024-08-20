
# CONFIGURANDO OS DADOS IMPORTADOS -------------------------------
# RCLs REALIZADA EM T
RCL12_m <- realizado %>% 
  filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
  select(c(1, starts_with(glue('{year(Sys.Date())}')))) %>% 
  mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
  pivot_longer(cols =  2:13) %>% 
  mutate(data = ymd(paste0(name, '01'))) %>% 
  setNames(c('RCL', 'name', 'RCL_2024', 'data')) %>% 
# ANEXANDO A RCL REALIZADA EM T-1 NA MESMA TABELA
    bind_rows(realizado %>% 
              filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
              select(c(1, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
              mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01')))%>%
# SETNAMES RENOMEIA COLUNAS, MAS NESSE CASO TAMBÉM PODE SER USADO PARA JUNTAR VALORES DE DIFERENTES COLUNAS (VALORES DE 'RECEITAS' FORAM PARA RCL_2024)
              setNames(c('RCL', 'name', 'RCL_2024', 'data'))) %>%
# ANEXANDO A RCL DE T-2  
  bind_rows(realizado %>% 
              filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
              select(c(1, starts_with(glue('{year(Sys.Date())-2}')))) %>% 
              mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01'))) %>% 
              setNames(c('RCL', 'name', 'RCL_2024', 'data'))) %>% 
  arrange(data) %>% 
# PEGANDO AS RCLs MENSAIS E CALCULANDO O Realizado MÓVEL DE 12 MESES
  mutate(RCL12 = rollsum(RCL_2024, 12, fill = NA, align = "right"),
         ano = year(data)) %>% 
  filter(ano == 2024) %>% 
  select(data, RCL12) %>%

# PEGANDO OS DADOS DA RCL REALIZADA EM T-1 E DA RCL PROJETADA PARA T
# RCL DE T-1
  bind_cols(realizado %>% 
              filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
              select(c(1, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
              mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
              pivot_longer(cols =  2:13) %>% 
              mutate(data = ymd(paste0(name, '01')),
                     band_sup = value,
                     band_inf = value) %>% 
              setNames(c('RCL', 'name', 'RCL_2024', 'data', 'band_sup', 'band_inf')) %>% 
# RCL PROJETADA PARA T              
              bind_rows(projeção %>% 
                          filter(ESPECIFICAÇÃO == 'RECEITA CORRENTE LÍQUIDA (III) = (I-II)') %>% 
                          select(c(1, starts_with(glue('{year(Sys.Date())}')))) %>% 
                          mutate(across(2:13, as.numeric)) %>% 
                          pivot_longer(cols =  2:13) %>% 
                          mutate(data = ymd(paste0(name, '01')),
                                 CAMPO = as.character(CAMPO),
                                 band_sup = value*(1+0.0434106013926729),
                                 band_inf = value*(1-0.0434106013926729)) %>% 
                          setNames(c('RCL', 'name', 'RCL_2024', 'data', 'band_sup', 'band_inf'))) %>% 
              arrange(data) %>% 
              mutate(RCL12 = rollsum(RCL_2024, 12, fill = NA, align = "right"),
                     band_sup = rollsum(band_sup, 12, fill = NA, align = "right"),
                     band_inf = rollsum(band_inf, 12, fill = NA, align = "right"),
                     ano = year(data)) %>% 
              filter(ano == 2024) %>% 
              select(RCL12, band_sup, band_inf)) %>% 
  setNames(c('data', 'RCL_24', 'PROJ_24', 'band_sup', 'band_inf')) %>% 
# PEGANDO DADOS E CALCULANDO A RCL DE 2023 ACUMULADA EM 12 MESES
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
  select(-PROJ_24,-band_sup,-band_inf)

# a <- realizado %>% 
#   filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
#   select(c(1, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
#   mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
#   pivot_longer(cols =  2:13) %>% 
#   mutate(data = ymd(paste0(name, '01')),
#          band_sup = value,
#          band_inf = value) %>% 
#   setNames(c('RCL', 'name', 'RCL_2024', 'data', 'band_sup', 'band_inf')) %>% 
#   bind_rows(projeção %>% 
#               filter(ESPECIFICAÇÃO == 'RECEITA CORRENTE LÍQUIDA (III) = (I-II)') %>% 
#               select(c(1, starts_with(glue('{year(Sys.Date())}')))) %>% 
#               mutate(across(2:13, as.numeric)) %>% 
#               pivot_longer(cols =  2:13) %>% 
#               mutate(data = ymd(paste0(name, '01')),
#                      CAMPO = as.character(CAMPO),
#                      band_sup = value*(1+0.0434106013926729),
#                      band_inf = value*(1-0.0434106013926729)) %>% 
#               setNames(c('RCL', 'name', 'RCL_2024', 'data', 'band_sup', 'band_inf'))) %>% 
#   arrange(data) %>% 
#   mutate(RCL12 = rollsum(RCL_2024, 12, fill = NA, align = "right"),
#          band_sup = rollsum(band_sup, 12, fill = NA, align = "right"),
#          band_inf = rollsum(band_inf, 12, fill = NA, align = "right"),
#          ano = year(data)) %>% 
#   filter(ano == 2024)



#########################################################
bloco1 <- RCL12_m |> 
  select(RCL_24) |> 
  drop_na() |> 
  tail(1) |> 
  mutate(RCL_24 = round(RCL_24/1000000000,2)) |> 
  pull()

bloco2 <- RCL |>
  select(RCL_2024) |>
  drop_na() |>
  tail(1) |>
  mutate(RCL_2024 = round(RCL_2024/1000,2)) |>
  pull()

# bloco3 <- round(bloco1 - bloco2, 2)

# bloco4 <- RCL12_m |> 
#   select(PROJ_24) |> 
#   tail(1) |> 
#   mutate(PROJ_24 = round(PROJ_24/1000000000,2)) |> 
#   pull()
# CRIAÇÃO DAS FIGURAS -------------------------------

fig1 <- 
  RCL12_m %>%
  mutate(fant_24 = sub("\\.", ",", round(RCL_24 / 1000000000, digits = 2)),
         fant_23 = sub("\\.", ",", round(RCL_23 / 1000000000, digits = 2))
         ) %>%
  ggplot() +
  # geom_ribbon(aes(x = data, ymin = band_inf, ymax = band_sup), fill = "grey80", alpha = 0.5) +
  geom_line(aes(x = data, y = RCL_23, color = "Realizado 2023",
                linetype = "Realizado 2023"), size=1) +
  geom_line(aes(x = data, y = RCL_24, color = "Realizado 2024", 
                linetype = "Realizado 2024"), size=1) +
  # geom_label(aes(x = data, y = PROJ_24, label = fantp_24),vjust = 1.1,colour = cor2[3])+
  # geom_label(aes(x = data, y = RCL_23, label = fant_23),vjust = 0.5,colour = cor2[2]) +
  geom_label(aes(x = data, y = RCL_24, label = fant_24),vjust = 0.5,colour = cor2[1], size = 3) +
  labs(x = "",y = "",title = "RCL acumulada em 12 meses",
       linetype = "Variable",color = "Variable") +
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale(),decimal.mark = ',',
                                                 prefix='R$ ')) +
  scale_x_date(limits = c(min(RCL12_m$data), max(RCL12_m$data) %m+% months(1)),
               date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c(#'Projeção 2024', 
                                'Realizado 2023',
                                'Realizado 2024'),
                     values = c("Realizado 2023"= cor2[2],
                                'Realizado 2024'= cor2[1]
                                #"Projeção 2024"=cor2[3]
                                ), 
                     name="Legenda:") +
  scale_linetype_manual(breaks = c(#'Projeção 2024',
                                   'Realizado 2023', 'Realizado 2024'),
                        values = c('Realizado 2023' = 'solid',
                                   'Realizado 2024' = 'solid'
                                   #"Projeção 2024"='longdash'
                                   ), 
                        name="Legenda:") +
  labs(fill = "Title") +
  theme_hc() + 
  theme(
    plot.title = element_text(hjust = 0.5,face='bold'),
    legend.title = element_blank(),
    legend.position = "bottom",plot.margin = margin(0, 0, 0, 0)
  )


fig2 <- 
  RCL %>% 
  mutate(fant_24 = sub("\\.", ",", round(RCL_2024/1000, digits = 2)),
         fant_23 = sub("\\.", ",", round(RCL_2023/1000, digits = 2))) |> 
  # mutate(band_sup= Projeção_RCL*(1+0.0434106013926729),
  #        band_inf = Projeção_RCL*(1-0.0434106013926729)) |> 
  ggplot()+
  # geom_ribbon(aes(x = data, ymin = band_inf*1000000, ymax = band_sup*1000000), fill = "grey80", alpha = 0.5) +
  geom_line(aes(x = data, y = RCL_2023*1000000, color = "Realizado 2023", 
                linetype = "Realizado 2023"), size=0.5) +
  geom_line(aes(x = data, y = RCL_2024*1000000, color = "Realizado 2024", 
                linetype = "Realizado 2024"), size=1) +
  geom_label(aes(x = data, y = RCL_2024*1000000, label = fant_24),vjust = 0.5,colour = cor2[1], size = 3) +
  # geom_label(aes(x = data, y = RCL_2023*1000000, label = fant_23),vjust = 0.5,colour = cor2[2])+
  # geom_line(aes(x = data, y = Projeção_RCL*1000000, color = "Projeção 2024", 
                # linetype = "Projeção 2024"), size=0.5) +
  labs(x = "", 
       y = "", 
       title = "RCL mensal",
       linetype = "Variable",
       color = "Variable") +
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale(),
                                                 decimal.mark=',',prefix='R$ '), limits = c(0, 5000000000)) +
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c('Realizado 2023', "Realizado 2024"#, 'Projeção 2024'
                                ),
                     values = c("Realizado 2024"= cor2[1],
                                "Realizado 2023"= cor2[2]
                                #"Projeção 2024"=  cor2[3]
                                ), 
                     name="Legenda:")+
  scale_linetype_manual(breaks = c('Realizado 2023', "Realizado 2024"#, 'Projeção 2024'
                                   ),
                        values = c("Realizado 2024"='solid',
                                   "Realizado 2023"='solid'
                                   #,"Projeção 2024"='longdash'
                                   ), 
                        name="Legenda:")+
  labs(fill = "Title") +
  theme_hc() + 
  theme(plot.title = element_text(hjust = 0.5,face='bold'),
    legend.title = element_blank(),
    legend.position = "bottom",plot.margin = margin(0, 0, 0, 0)
  )

fig.allg <- 
  ggarrange(fig1, fig2, ncol = 1, nrow = 2, common.legend = T,legend = 'bottom')

  