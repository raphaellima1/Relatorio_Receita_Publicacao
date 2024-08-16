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
          location = ph_location(left = 0.3, top = 1.9)) %>%
  
  ph_with(dml(code = plot(fig1)),
          location = ph_location(left =8.1 , top = 1.7,
                                 width = 5, height = 4))%>%
  
  ph_with(block_list(fpar(ftext(glue('(Em milhões de R$)'),
                                prop = fp_text(font.size = 12,
                                               color = "#292929")),
                          fp_p = border1)),
          
          location = ph_location(left = 1.4, top = 1.6,
                                 width = 6.5,
                                 height = 0.3)) 



print('RECEITAS TRIBUTÁRIAS <- OK')



######################################################################
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
          location = ph_location(left = 0.4, top = 1.3)) %>% 
  
  ph_with(dml(code = plot(fig6)), 
          location = ph_location(left = 9,
                                 top = 1.5,
                                 width = 4,
                                 height = 5)) %>% 
  
  ph_with(block_list(fpar(ftext(glue('(Em R$ milhões)'), 
                                prop = fp_text(font.size = 12, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 1.7, top = 1, 
                                 width = 6.5,
                                 height = 0.3)) 
# %>% 
#   
#   ph_with(block_list(fpar(ftext(glue('* Resultados preliminares'),
#                                 prop = fp_text(font.size = 11,
#                                                color = "#292929")))),
#           
#           location = ph_location(left = 0.18, top = 0.96,
#                                  width = 2,
#                                  height = 0.201))


print('RECEITAS TRIBUTÁRIAS - PERCENTUAL <- OK')