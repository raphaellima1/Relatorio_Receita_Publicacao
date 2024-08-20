###############################################
# RCL
###############################################

# executando os insumos (gráficos e tabela)
source(encoding = 'UTF-8', file = './4- INSUMO/RCL_T.R')

source(encoding = 'UTF-8', file = './4- INSUMO/RCL_G.R')

# Receita total - Gráfico
###############################################
my <- my %>%
  add_slide(layout = "título_conteúdo", master = "RRF_template_01") %>%
  
  ph_with(value = "Receita Corrente Líquida (RCL)", 
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
          location = ph_location(left = 7.8,
                                 top = 0.8,
                                 width = 5,
                                 height = 6.3)) %>%

  ph_with(tabela_acumulado,
          location = ph_location(left = 0.39, top = 2.7,
                                 width=9,height=4))  %>%
  
  ph_with(block_list(fpar(ftext(glue('(Valores em milhões de R$)'),
                                prop = fp_text(font.size = 12,
                                               color = "#292929")),
                          fp_p = border1)),

          location = ph_location(left = 3.1, top = 2.4,
                                 width = 4,
                                 height = 0.3)) %>%
  
# primeiro bloco  
  ph_with(block_list(
    fpar(
      ftext(glue('R$ {format(bloco1, decimal.mark=",", scientific = FALSE)} bi'), 
            prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
      fp_p = border2
    ),
    fpar(
      ftext(glue('Realizado até {format(Sys.Date(), "%b/%y")}'), 
            prop = fp_text(font.size = 16, color = "#ffffff")),
      fp_p = border2
    ),
    fpar(
      ftext(glue('(Acumulado em 12 meses)'), 
            prop = fp_text(font.size = 10.5, color = "#ffffff")),
      fp_p = border2
    )
  ),
  location = ph_location(left = 0.39, top = 1.2, width = 3.14, 
                         height = 0.90, bg = cor1[2])) |>
  
# segundo bloco
ph_with(block_list(
  fpar(
    ftext(glue('R$ {format(bloco2, decimal.mark=",", scientific = FALSE)} bi'), 
          prop = fp_text(font.size = 18, color = "#ffffff", bold = T)),
    fp_p = border2
  ),
  fpar(
    ftext(glue('Realizado em {format(Sys.Date(), "%b/%y")}'), 
          prop = fp_text(font.size = 16, color = "#ffffff")),
    fp_p = border2
  )
),
location = ph_location(left = 4, top = 1.2, width = 3.14, 
                       height = 0.90, bg = cor1[2]))


print('RCL <- OK')


