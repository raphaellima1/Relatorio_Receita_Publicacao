# Despesa Liquidada -------------------------------------------------------

source(encoding = 'UTF-8', file = './4- INSUMO/DESPESA_LIQUIDA_G.R')


# Receita TOtAL - Tabela
###############################################


my <- my %>%
  add_slide(layout = "título_conteúdo", master = "RRF_template_01") %>%
  
  ph_with(value = "Despesa Liquidada", 
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = fpar(
    ftext("Comparativo anual - ", prop = fp_text(font.size = 0, color = '#00579E')),
    ftext("Somente o Executivo", prop = fp_text(font.size = 0, shading.color ="yellow", color = '#00579E'))
  ),
  location = ph_location_type(type = "subTitle")) %>%
  
  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>% 
  
  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia", 
          location = ph_location_type(type = "ftr")) %>% 
  
  ph_with(value = empty_content(), 
          location = ph_location_type(type = "sldNum")) %>% 
  #  
  ph_with(dml(code = plot(fig.allg)), 
          location = ph_location(left = 0.6,
                                 top = 2.3,
                                 width = 12,8,
                                 height = 4.9)) |> 
  
  ph_with(block_list(
    fpar(
      ftext(glue('R$ {format(bloco1, decimal.mark = ",", scientific = FALSE)} bi'), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Despesa Liquidada \n acumulada até {format(Sys.Date() %m-% months(1), "%b/%y")}'), 
            prop = fp_text(font.size = 14, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 0.2, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2])) |> 
  
  # segundo bloco
  ph_with(block_list(
    fpar(
      ftext(glue('R$ {format(bloco2, decimal.mark = ",", scientific = FALSE)} bi'), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Despesa Liquidada \n acumulada até {format(Sys.Date() %m-% months(1), "%b")}/23'), 
            prop = fp_text(font.size = 14, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 3.44, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2])) |> 
  
  # terceiro bloco
  ph_with(block_list(
    fpar(
      ftext(ifelse(bloco3 > 0, 
                   glue('+ {format(bloco3, decimal.mark = ",", scientific = FALSE)} %'), 
                   glue('- {format(bloco3, decimal.mark = ",", scientific = FALSE)} %i')), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Variação (%) \n Despesa Liquidada Total'), 
            prop = fp_text(font.size = 14, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 6.68, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2])) |> 
  
  # quarto bloco
  ph_with(block_list(
    fpar(
      ftext(glue('{format(bloco4, decimal.mark = ",", scientific = FALSE)} %'), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Variação (%) \n liquidação Pessoal e Encargos'), 
            prop = fp_text(font.size = 14, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 9.92, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2]))
  
  
  


print('DESPESA LIQUIDADA <- OK')