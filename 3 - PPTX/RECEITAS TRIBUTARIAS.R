###############################################
# RECEITAS TRIBUTÁRIAS
###############################################
source(encoding = 'UTF-8', file = './4- INSUMO/RECEITAS_TRIBUTARIAS_T.R')
source(encoding = 'UTF-8', file = './4- INSUMO/RECEITAS_TRIBUTARIAS_G.R')



my <- my %>%
  add_slide(layout = "título_conteúdo", master = "RRF_template_01") %>%
  
  ph_with(value = "Receitas Tributárias",
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue("Arrecadação Bruta"),
          location = ph_location_type(type = "subTitle")) %>%
  
  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>%
  
  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia",
          location = ph_location_type(type = "ftr")) %>%
  
  ph_with(value = empty_content(),
          location = ph_location_type(type = "sldNum")) %>%
  #
  ph_with(tabela_acumulado,
          location = ph_location(left = 0.6, top = 2.8)) %>%
  
  ph_with(dml(code = plot(fig1)),
          location = ph_location(left = 6.6 , top = 2.4,
                                 width = 6.1, height = 4.7))%>%
  
  ph_with(block_list(fpar(ftext(glue('(Em milhões de R$)'),
                                prop = fp_text(font.size = 12,
                                               color = "#292929")),
                          fp_p = border1)),
          
          location = ph_location(left = 2, top = 2.5,
                                 width = 3,
                                 height = 0.3)) |> 
 
  ph_with(block_list(
    fpar(
      ftext(glue('R$ {format(bloco1, decimal.mark = ",", scientific = FALSE,digits = 4)} bi'), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('ICMS'), 
            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Acum. jan/24 a {format(mes_atualizacao, "%b/%y")}'), 
            prop = fp_text(font.size = 10.5, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 0.2, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2])) |> 
  
  # segundo bloco
  ph_with(block_list(
    fpar(
      ftext(glue('R$ {format(bloco2, decimal.mark = ",", scientific = FALSE,digits = 3)} bi'), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('IPVA'), 
            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Acum. jan/24 a {format(mes_atualizacao, "%b/%y")}'), 
            prop = fp_text(font.size = 10.5, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 3.44, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2])) |> 
  
  # terceiro bloco
  ph_with(block_list(
    fpar(
      ftext(glue('R$ {format(bloco3, decimal.mark = ",", scientific = FALSE, digits = 5)} mi'), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('ITCD'), 
            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Acum. jan/24 a {format(mes_atualizacao, "%b/%y")}'), 
            prop = fp_text(font.size = 10.5, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 6.68, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2])) |> 
  
  # quarto bloco
  ph_with(block_list(
    fpar(
      ftext(glue('R$ {format(bloco4, decimal.mark = ",", scientific = FALSE, digits = 5)} mi'), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('FUNDEINFRA'), 
            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Acum. jan/24 a {format(mes_atualizacao, "%b/%y")}'), 
            prop = fp_text(font.size = 10.5, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 9.92, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2]))



print('RECEITAS TRIBUTÁRIAS <- OK')



#####################################################################
source(encoding = 'UTF-8', file = './4- INSUMO/RECEITAS_TRIBUTARIAS_PERC.R')



my <- my %>%
  add_slide(layout = "título_conteúdo", master = "RRF_template_01") %>%

  ph_with(value = "Detalhamento das Receitas com ICMS",
          location = ph_location_type(type = "title")) %>%

  ph_with(value = glue("Macro-setores estratégicos"),
          location = ph_location_type(type = "subTitle")) %>%

  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>%

  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia",
          location = ph_location_type(type = "ftr")) %>%

  ph_with(value = empty_content(),
          location = ph_location_type(type = "sldNum")) %>%
  #
  ph_with(tabela_COM_ICMS,
          location = ph_location(left = 0.4, top = 1.7)) %>%

  ph_with(dml(code = plot(fig6)),
          location = ph_location(left = 7,
                                 top = 1.5,
                                 width = 6,
                                 height = 5)) %>%

  ph_with(block_list(fpar(ftext(glue('(Em R$ milhões)'),
                                prop = fp_text(font.size = 12,
                                               color = "#292929")),
                          fp_p = border1)),

          location = ph_location(left = 3.3, top = 1.4,
                                 width = 3,
                                 height = 0.3))



print('RECEITAS TRIBUTÁRIAS - PERCENTUAL <- OK')