excu_orcamentaria_G <- excu_orcamentaria |> 
  group_by(data) |> 
  summarise(dotação = sum(`Dotação (LOA + Créditos)`, na.rm = T),
            Empenho = sum(Empenho, na.rm = T),
            Liquidação = sum(Liquidação, na.rm = T),
            Pagamento = sum(Pagamento, na.rm = T)) |> 
  mutate(Dot = cumsum(dotação),
         Emp = cumsum(Empenho),
         Liq = cumsum(Liquidação),
         Pag = cumsum(Pagamento))

bloco1 <- excu_orcamentaria_G |> 
  select(Dot) |> 
  tail(n = 1) |>
  mutate(Dot = round(Dot/1000000000, 2)) |> 
  pull()

bloco2 <- excu_orcamentaria_G |> 
  select(Emp) |> 
  tail(n = 1) |>
  mutate(Emp = round(Emp/1000000000, 2)) |> 
  pull()

bloco3 <- excu_orcamentaria_G |> 
  select(Liq) |> 
  tail(n = 1) |>
  mutate(Liq = round(Liq/1000000000, 2)) |> 
  pull()

bloco4 <- excu_orcamentaria_G |> 
  select(Pag) |> 
  tail(n = 1) |>
  mutate(Pag = round(Pag/1000000000, 2)) |> 
  pull()

start_date <- as.Date('2024-08-01')
end_date <- as.Date("2024-12-01")
seq_dates <- seq(start_date, end_date, by = "month")



fig1 <- excu_orcamentaria_G |>
  bind_rows(data.frame(
    data = seq_dates,
    dotação = NA,
    Empenho = NA,
    Liquidação = NA,
    Pagamento = NA,
    Dot = NA,
    Emp = NA,
    Liq = NA,
    Pag = NA
  )) |> 
  fill(Dot, .direction = "down") |> 
  ggplot()+
  labs(x = " ", y = "Valores em Reais (R$)") +
  geom_step(aes(x = data, y = Dot, color = "Dotação",
                linetype = "Dotação"), size=0.8, )+
  geom_line(aes(x = data, y = Emp, color = "Empenho",
                linetype = "Empenho"), size=0.8)+
  geom_line(aes(x = data, y = Liq, color = "Liquidação",
                linetype = "Liquidação"), size=0.8)+
  geom_line(aes(x = data, y = Pag, color = "Pagamento",
                linetype = "Pagamento"), size=0.8)+
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  
  scale_color_manual(breaks = c('Dotação', "Empenho", 'Liquidação', 'Pagamento'),
                     values = c("Dotação" = 'gray20', "Empenho" = cor2[3], 
                                "Liquidação" = cor2[2],"Pagamento" = cor2[1]), 
                     name = "Legenda:") + 
  
  scale_linetype_manual(breaks = c('Dotação', "Empenho", 'Liquidação', 'Pagamento'),
                        values = c("Dotação" = 'solid', "Empenho" = 'solid', 
                                   "Liquidação" = 'solid',"Pagamento" = 'solid'), 
                        name = "Legenda:") +
  theme_classic2()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

