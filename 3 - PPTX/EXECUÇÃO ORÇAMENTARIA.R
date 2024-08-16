# Receitas tributárias ----------------------------------------------------
source(encoding = 'UTF-8', file = './4- INSUMO/EXECUÇÃO_ORÇAMENTÁRIA_T.R')
source(encoding = 'UTF-8', file = './4- INSUMO/EXECUÇÃO_ORÇAMENTARIA_G.R')

my <- my %>%
  add_slide(layout = "título_conteúdo", master = "RRF_template_01") %>%



  
  ph_with(value = "Execução Orçamentária",
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue("Incluindo os Poderes"),
          location = ph_location_type(type = "subTitle")) %>%
  
  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>%
  
  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia",
          location = ph_location_type(type = "ftr")) %>%
  
  ph_with(value = empty_content(),
          location = ph_location_type(type = "sldNum")) %>%

  ph_with(ft, 
          location = ph_location(left = 0.4, top = 3.0)) %>% 
  
  ph_with(dml(code = plot(fig1)), 
          location = ph_location(left = 8.,
                                 top = 2.5,
                                 width = 5,
                                 height = 4)) %>% 
  
  ph_with(block_list(fpar(ftext(glue('(Em R$ milhões)'), 
                                prop = fp_text(font.size = 12, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 1.1, top = 2.7, 
                                 width = 6.5,
                                 height = 0.3)) |>

# Primeiro Bloco ----------------------------------------------------------

  
  ph_with(block_list(
    fpar(
      ftext(glue('R$ {format(bloco1, decimal.mark = ",", scientific = FALSE)} bi'), 
            prop = fp_text(font.size = 18, color = "white", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Dotação Autorizada'), 
            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Até {format(Sys.Date() %m-% months(1), "%b/%y")}'), 
            prop = fp_text(font.size = 10.5, color = "#ffffff")),
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
      ftext(glue('Valor Empenhado'), 
            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Até {format(Sys.Date() %m-% months(1), "%b/%y")}'), 
            prop = fp_text(font.size = 10.5, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 3.44, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2])) |> 
  
  # terceiro bloco
  ph_with(block_list(
    fpar(
      ftext(glue('R$ {format(bloco3, decimal.mark = ",", scientific = FALSE)} bi'), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Valor Liquidado'), 
            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Até {format(Sys.Date() %m-% months(1), "%b/%y")}'), 
            prop = fp_text(font.size = 10.5, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 6.68, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2])) |> 
  
  # quarto bloco
  ph_with(block_list(
    fpar(
      ftext(glue('R$ {format(bloco4, decimal.mark = ",", scientific = FALSE)} bi'), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Valor Pago'), 
            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Até {format(Sys.Date() %m-% months(1), "%b/%y")}'), 
            prop = fp_text(font.size = 10.5, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 9.92, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2]))

print('EXECUÇÃO ORÇAMENTÁRIA <- OK1')