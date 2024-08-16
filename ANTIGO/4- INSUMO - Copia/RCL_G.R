RCL12_m <- realizado %>% 
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
              mutate(data = ymd(paste0(name, '01')),
                     band_sup = value,
                     band_inf = value) %>% 
              setNames(c('RCL', 'name', 'RCL_2024', 'data', 'band_sup', 'band_inf')) %>% 
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
              select(RCL_23))

a <- realizado %>% 
  filter(RECEITAS == 'RECEITA CORRENTE LÍQUIDA') %>% 
  select(c(1, starts_with(glue('{year(Sys.Date())-1}')))) %>% 
  mutate(across(2:13, ~ na_if(as.numeric(.), 0.00))) %>% 
  pivot_longer(cols =  2:13) %>% 
  mutate(data = ymd(paste0(name, '01')),
         band_sup = value,
         band_inf = value) %>% 
  setNames(c('RCL', 'name', 'RCL_2024', 'data', 'band_sup', 'band_inf')) %>% 
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
  filter(ano == 2024)


#########################################################
bloco1 <- RCL12_m |> 
  select(RCL_24) |> 
  drop_na() |> 
  tail(1) |> 
  mutate(RCL_24 = round(RCL_24/1000000000,2)) |> 
  pull()

bloco2 <- RCL12_m |> 
  
  drop_na() |>
  select(PROJ_24) |> 
  tail(1) |> 
  mutate(PROJ_24 = round(PROJ_24/1000000000,2)) |> 
  pull() 

bloco3 <- round(bloco1 - bloco2, 2)

bloco4 <- RCL12_m |> 
  select(PROJ_24) |> 
  tail(1) |> 
  mutate(PROJ_24 = round(PROJ_24/1000000000,2)) |> 
  pull()
########################################################

fig1 <- RCL12_m %>%
  mutate(fant_24 = sub("\\.", ",", round(RCL_24 / 1000000000, digits = 2)),
         fantp_24 = sub("\\.", ",", round(PROJ_24 / 1000000000, digits = 2))) %>%
  ggplot()+
  geom_ribbon(aes(x = data, ymin = band_inf, ymax = band_sup), fill = "grey80", alpha = 0.5) +
  geom_line(aes(x = data, y = PROJ_24, color = "Projeção 2024", 
                linetype = "Projeção 2024"), size=0.5) +
  
  geom_line(aes(x = data, y = RCL_24, color = "Realizado 2024", 
                linetype = "Realizado 2024"), size=1) +
  geom_label(aes(x = data, y = PROJ_24, label = fantp_24),vjust = 1.1,colour = cor2[3])+
  geom_label(aes(x = data, y = RCL_24, label = fant_24),vjust = -0.7,colour = cor2[1])+

  
  labs(x = "  ", 
       y = "Valores em Reais (R$)", 
       title = "RCL 12 MESES",
       linetype = "Variable",
       color = "Variable")+
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale(),decimal.mark = ',')) +
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c('Projeção 2024', 'Realizado 2024'),
                     values = c("Realizado 2024"= cor2[1],
                                "Projeção 2024"=cor2[3]), 
                     name="Legenda:")+
  scale_linetype_manual(breaks = c('Projeção 2024', 'Realizado 2023', 'Realizado 2024'),
                        values = c('Realizado 2024' = 'solid',
                                   "Projeção 2024"='longdash'), 
                        name="Legenda:")+
  
  labs(fill = "Title") +
  theme_hc() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 

fig1

fig2 <- RCL %>% 
  mutate(band_sup= Projeção_RCL*(1+0.0434106013926729),
         band_inf = Projeção_RCL*(1-0.0434106013926729)) |> 
  ggplot()+
  geom_ribbon(aes(x = data, ymin = band_inf*1000000, ymax = band_sup*1000000), fill = "grey80", alpha = 0.5) +
  geom_line(aes(x = data, y = RCL_2023*1000000, color = "Acumulado 2023", 
                linetype = "Acumulado 2023"), size=0.5) +
  
  geom_line(aes(x = data, y = RCL_2024*1000000, color = "Acumulado 2024", 
                linetype = "Acumulado 2024"), size=1) +
  
  geom_line(aes(x = data, y = Projeção_RCL*1000000, color = "Projeção 2024", 
                linetype = "Projeção 2024"), size=0.5) +
  
  labs(x = "  ", 
       y = "Valores em Reais (R$)", 
       title = "RCL MENSAL",
       linetype = "Variable",
       color = "Variable") +
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c('Acumulado 2023', "Acumulado 2024", 'Projeção 2024'),
                     values = c("Acumulado 2024"= cor2[1],
                                "Acumulado 2023"= cor2[2],
                                "Projeção 2024"=  cor2[3]), 
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

fig.allg <- ggarrange(fig1, fig2, ncol = 2, nrow = 1, common.legend = F)
