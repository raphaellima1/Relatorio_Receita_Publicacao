###############################################
# RTL
###############################################

# EXECUTANDO OS INSUMOS
source(encoding = 'UTF-8', file = './4- INSUMO/RECEITA_TOTAL_T.R')
source(encoding = 'UTF-8', file = './4- INSUMO/RECEITA_TOTAL_G.R')

# Receita Total - Gráfico
###############################################
my <- my %>%
  add_slide(layout = "título_conteúdo", master = "RRF_template_01") %>%
  
  ph_with(value = "Receita Total", 
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue('Resultados preliminares em {format(Sys.Date(), "%d/%m/%y")}'),
          location = ph_location_type(type = "subTitle")) %>%
  
  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>% 
  
  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia", 
          location = ph_location_type(type = "ftr")) %>% 
  
  ph_with(value = empty_content(), 
          location = ph_location_type(type = "sldNum")) %>% 
  
  ph_with(dml(code = plot(fig.allg)), 
          location = ph_location(left = 0.6,
                                 top = 2.3,
                                 width = 12,8,
                                 height = 4.9)) %>%
  
  
# primeiro bloco
  ph_with(block_list(
    fpar(
      ftext(glue('R$ {format(bloco1, decimal.mark = ",", scientific = FALSE)} bi'), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(

      ftext(glue('Realizado até {format(mes_atualizacao, "%b")}/24'), 

            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    ),
    fpar(

      ftext(glue('(Acum. jan/24 a {format(mes_atualizacao, "%b")}/24)'), 

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

      ftext(glue('Projetado até {format(mes_atualizacao, "%b")}/24'), 

            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    ),
    fpar(
      ftext(glue('(Acum. jan/24 a {format(mes_atualizacao, "%b")}/24)'), 

            prop = fp_text(font.size = 10.5, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 3.44, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2])) |> 
  
  # terceiro bloco
  ph_with(block_list(
    fpar(
      ftext(ifelse(bloco3 > 0, 
                   glue('+ R$ {format(bloco3, decimal.mark = ",", scientific = FALSE)} bi'), 
                   glue('- R$ {format(bloco3, decimal.mark = ",", scientific = FALSE)} bi')), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Realizado x Projetado'), 
            prop = fp_text(font.size = 16, color = "#ffffff")),
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
      ftext(glue('Previsão acum. para Dez/24'), 
            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 9.92, top = 1.1, width = 3.14, 
                         height = 0.90, bg = cor1[2]))
  
  
  
  
  

# Receita Total Tabela
###############################################
# CRIAÇÃO DO SLIDE
my <- my %>%
  add_slide(layout = "título_conteúdo", master = "RRF_template_01") %>%
  
  ph_with(value = "Receita Total", 
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue('Resultados preliminares em {format(Sys.Date(), "%d/%m/%y")}'),
          location = ph_location_type(type = "subTitle")) %>%
  
  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>% 
  
  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia", 
          location = ph_location_type(type = "ftr")) %>% 
  
  ph_with(value = empty_content(), 
          location = ph_location_type(type = "sldNum")) %>% 
  #  
  ph_with(tabela_acumulado, 
          location = ph_location(left = 0.5, top = 1.2))  %>% 
  
  ph_with(block_list(fpar(ftext(glue('(Valores em milhões de R$)'), 
                                prop = fp_text(font.size = 12, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 5.8, top = 0.9, 
                                 width = 6.5,
                                 height = 0.3)) 

rm(bloco1, bloco2, bloco3, bloco4, a,fig.allg, fig1, fig2,
   RLT_band, RLT, spacer)

print('RTL <- OK')