
desp_liquidada <- e_orca |> 
  filter(Poder == 'EXECUTIVO') |> 
  select(Exercício, GND_Cod, AnoMes,  Liquidação) |> 
  filter(Exercício >= 2023) |> 
  group_by(AnoMes, Exercício) |> 
  summarise(Liquidação = sum(Liquidação, na.rm = T)) |> 
  ungroup() |> 
  mutate(AnoMes = format(ym(AnoMes), '%m')) |> 
  pivot_wider(names_from = Exercício, values_from = Liquidação) |> 
mutate(acum_23 = cumsum(`2023`),
       acum_24 = cumsum(`2024`),
       data = make_date(2024, as.integer(AnoMes), 1))

ultimo_ponto_23 <- desp_liquidada %>%
  filter(!is.na(acum_23)) %>%
  slice_tail(n = 1)

ultimo_ponto_24 <- desp_liquidada %>%
  filter(!is.na(acum_24)) %>%
  slice_tail(n = 1)


# Crie o gráfico com geom_label no último ponto de cada série
fig1 <- desp_liquidada |> 
  ggplot(aes(x = data)) +
  geom_line(aes(y = acum_23, color = "2023", linetype = "2023"), size = 1) +
  geom_line(aes(y = acum_24, color = "2024", linetype = "2024"), size = 1) +
  geom_label(data = ultimo_ponto_23, aes(y = acum_23, label = round(acum_23/1000000000, 2), color = "2023"), vjust = 1, show.legend = FALSE) +
  geom_label(data = ultimo_ponto_24, aes(y = acum_24, label = round(acum_24/1000000000, 2), color = "2024"), vjust = -1, show.legend = FALSE) +
  
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  
  scale_color_manual(breaks = c('2023', '2024'),
                     values = c("2023" = cor2[1], "2024" = cor2[2]), 
                     name = "Legenda:") + 
  
  scale_linetype_manual(breaks = c('2023', '2024'),
                        values = c('2023' = 'solid', '2024' = 'solid'), 
                        name = "Legenda:") +
  theme_classic2() +
  labs(title = 'Despesa Total Liquidada \n (Valores Acumulados)',
       x = "  ", 
       y = "Valores em Reais (R$)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom")

desp_liquidada_Grupo <- e_orca |> 
  filter(Poder == 'EXECUTIVO') |> 
  drop_na(Liquidação) |> 
  select(Exercício, GND_Nome, AnoMes,  Liquidação)  |> 
  filter(Exercício >= 2023) |> 
  group_by(AnoMes, Exercício, GND_Nome) |> 
  summarise(Liquidação = sum(Liquidação, na.rm = T)) |> 
  ungroup() |> 
  mutate(AnoMes = format(ym(AnoMes), '%m')) |> 
  pivot_wider(names_from = Exercício, values_from = Liquidação) |> 
  mutate(AnoMes = as.numeric(AnoMes)) |> 
filter(AnoMes <= as.numeric(month(Sys.Date() - 1))) |> 
  group_by(GND_Nome) |> 
  summarise(acum_23 = sum(`2023`, na.rm = T),
            acum_24 = sum(`2024`, na.rm = T))




fig2 <- desp_liquidada_Grupo |> 
  pivot_longer(cols = acum_23:acum_24, names_to = "variable", values_to = "value") |> 
  ggplot(aes(x = GND_Nome, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 1) +
  geom_text(aes(label = format(round(value / 1000000, 0), nsmall = 0, big.mark = ".", decimal.mark = ","),
                color =  "black"),position = position_dodge(width = 0.9), hjust = 0) + 
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()),limits = c(0,15000000000)) +

  coord_flip() +
  scale_fill_manual(name = "Legenda:", values = c("acum_23" = cor2[1]
                                                    , "acum_24" = cor2[2]), 
                    labels = c( "2023", "2024")) +
  theme_classic2() +
  labs(title = 'Despesa Total Liquidada \n (Valores Acumulados)',
       x = "  ", 
       y = "Valores em Reais (R$)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(size = 8),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) +
  scale_color_identity()

fig.allg <- ggarrange(fig1, fig2, ncol = 2, nrow = 1, common.legend = F)


bloco1 <-  desp_liquidada_Grupo |> 
  select(acum_24) |>
  mutate(acum_24 = round(acum_24/1000000000,2)) |> 
  sum() 


bloco2 <-  desp_liquidada_Grupo |> 
  select(acum_23) |>
  mutate(acum_23 = round(acum_23/1000000000,2)) |> 
  sum() 

bloco3 <- round(((bloco1-bloco2)/bloco2)*100,2)

bloco4 <- desp_liquidada_Grupo |> 
  filter(GND_Nome =='PESSOAL E ENCARGOS SOCIAIS') |> 
  mutate(variação = round(((acum_24 - acum_23)/acum_23)*100,2)) |> 
  select(variação) |> 
  pull()
