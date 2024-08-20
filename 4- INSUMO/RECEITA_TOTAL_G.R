
a <- RLT %>%
  select(RCL_2024, Projeção_RCL) |> 
  drop_na() %>%
  summarize(count = n()) |> 
  pull()

RLT_band <- RLT %>%
  select(RCL_2024, Projeção_RCL) %>%
  drop_na() %>%
  mutate(dif = round(abs(RCL_2024/Projeção_RCL - 1),3)) %>%
  summarise(valor = sum(dif) / a) %>%
  pull() 

RLT <- RLT %>%
  mutate(band_inf = Projeção_RCL * (1 - RLT_band),
         band_sup = Projeção_RCL * (1 + RLT_band),
         mes = month(data))




fig1 <- RLT %>%
  mutate(fant_24 = sub("\\.", ",", round(acum_24 / 1000, digits = 2))) |> 
  ggplot()+
 
  geom_line(aes(x = data, y = acum_23*1000000, color = "Acumulado 2023", 
                linetype = "Acumulado 2023"), size=0.5) +
  
  geom_line(aes(x = data, y = acum_24*1000000, color = "Acumulado 2024", 
                linetype = "Acumulado 2024"), size=1) +
  geom_label(aes(x = data, 
                 y = acum_24*1000000, 
                 label = fant_24),
             vjust = -0.7,
             colour = cor2[1])+
  

  labs(x = "  ", 
       y = "Valores em R$", 
       title = "RT acumulada",
       linetype = "Variable",
       color = "Variable") +
  
  scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b")+
  scale_color_manual(breaks = c('Acumulado 2023', "Acumulado 2024"),
                     values = c("Acumulado 2024"= cor2[1],
                                "Acumulado 2023"=cor2[2]), 
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
   # plot.background = element_rect(colour = "gray", linewidth = 0.2),
    legend.position = "bottom"
  )




fig2 <- RLT |> 
  mutate(fant_24 = sub("\\.", ",", round(RCL_2024 / 1000, digits = 2))) |> 
  ggplot() +

  geom_line(data = RLT, 
            aes(x = data, 
                y = RCL_2023 * 1000000, 
                color = "Acumulado 2023", 
                linetype = "Acumulado 2023"), 
            size = 0.5) +
  
  geom_label(aes(x = data, 
                 y = RCL_2024*1000000, 
                 label = fant_24),
             vjust = -0.7,
             colour = cor2[1])+
  
  geom_line(data = RLT, aes(x = data, 
                            y = RCL_2024 * 1000000, 
                            color = "Acumulado 2024", 
                            linetype = "Acumulado 2024"), 
            size = 1) +
  
  labs(x = "Meses", y = "Valores em R$", title = "RT mensal", 
       linetype = "Variable", color = "Variable") +
  
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()), limits = c(0, 5000000000)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  
  scale_color_manual(breaks = c('Acumulado 2023', 
                                "Acumulado 2024"),
                     values = c("Acumulado 2024" = cor2[1], 
                                "Acumulado 2023" = cor2[2]), 
                     name = "Legenda:") +
  
  scale_linetype_manual(breaks = c('Acumulado 2023', 
                                   "Acumulado 2024"),
                        values = c("Acumulado 2024" = 'solid', 
                                   "Acumulado 2023" = 'solid'), 
                        name = "Legenda:") +
  theme_hc() + 
  
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

# Juntando gráficos
fig.allg <- ggarrange(
  fig1, fig2,
  ncol = 1, nrow = 2, 
  widths = c(1, 0.04, 1),  # Ajustar proporção das colunas, onde 0.2 é o espaçador
  common.legend = TRUE, 
  legend = "bottom"
)

