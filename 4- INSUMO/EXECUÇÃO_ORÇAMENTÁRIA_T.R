excu_orcamentaria <- e_orca |> 
  filter(Exercício == year(Sys.Date())) |> 
  mutate(data = ymd(paste0(AnoMes,'01')))
  


excu_orcamentaria_T <- excu_orcamentaria |> 
  group_by(GND_Cod) |> 
  summarise(dotação = sum(`Dotação (LOA + Créditos)`, na.rm = T)/1000000,
            Empenho = sum(Empenho, na.rm = T)/1000000,
            Liquidação = sum(Liquidação, na.rm = T)/1000000,
            Pagamento = sum(Pagamento, na.rm = T)/1000000) |> 
  adorn_totals() |> 
  mutate(dot_emp = (Empenho/dotação)*100,
         liq_emp = (Liquidação/Empenho)*100,
         pgto_liqui = (Pagamento/Liquidação)*100)



# Supondo que excu_orcamentaria_T seja seu dataframe
ft <- excu_orcamentaria_T |> 
  flextable()  |>  
  border_remove()  |> 
  colformat_double(j = c("dotação", "Empenho", 'Liquidação', 'Pagamento'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 0, 
                   na_str = "--") |> 
  colformat_double(j = c("dot_emp", "liq_emp", 'pgto_liqui'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 1, 
                   na_str = "--",
                   suffix = ' %') |> 
  set_header_labels(values = c('Grupo de despesa',"Dotação (Autorizada)", "Empenho",
                               'Liquidação', "Pagamento", "Emp/Dot",
                               "Liq/Emp", "Pgto./Liq")) |> 
  bg(part = "header", bg = cor1[2]) |> 
  flextable::style(pr_t = fp_text_default(bold = FALSE, color = cor1[3]), part = 'header') |> 
  bg(i= ~ GND_Cod == "Total", 
     part = "body", 
     bg = cor1[2]) %>% 
  
  style(i =  ~ GND_Cod == "Total", 
        pr_t = fp_text_default(
          bold = F,
          color = cor1[1]
        )) %>% 
  bg(i = c(2, 4, 6), part = "body", bg = cor1[1]) |> 
  align(i = c(1), j = NULL, align = "center", part = "header") |> 
  width(j = 1, width = 2, unit = 'cm') |> 
  width(j = c(2:5), width = 2.5, unit = 'cm') |> 
  width(j = c(6:8), width = 2, unit = 'cm') 

# Aplicar barras de progresso nas colunas específicas
for (col in c("dot_emp", "liq_emp", "pgto_liqui")) {
  ft <- compose(ft, j = col, value = as_paragraph(
    minibar(value = as.numeric(.data[[col]]), 
            max = max(as.numeric(excu_orcamentaria_T[[col]]), na.rm = TRUE), 
            height = 0.5,,width = 1.2, barcol = cor1[2], unit = 'cm'),
    ' ',
    as_chunk(sprintf("%.1f %%", as.numeric(.data[[col]])))
  ))
}
for (col in c("dot_emp", "liq_emp", "pgto_liqui")) {

  ft <- compose(ft, j = col, i = 8, value = as_paragraph(
    minibar(value = as.numeric(.data[[col]]), 
            max = max(as.numeric(excu_orcamentaria_T[[col]]), na.rm = TRUE), 
            height = 0.5, width = 1.2, barcol = cor1[3], unit = 'cm'),
    ' ',
    as_chunk(sprintf("%.1f %%", as.numeric(.data[[col]])))
  )) |> 
    align( j = c(6:8), align = "right", part = "body")
}
ft
save_as_image(ft, path = 'imagem.png')
