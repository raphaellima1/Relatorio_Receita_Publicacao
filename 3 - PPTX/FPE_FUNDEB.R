
# TRASNFERÊNCIAS CORRENTES
# FPE --------------------------------------------------------------------------
# EXECUTANDO O INSUMO DO FPE
source(encoding = 'UTF-8', file = './4- INSUMO/FPE.R')

###############################################
# CARREGANDO O SLIDE DO FPE ----------------------------------------------------

my <- my %>%
  add_slide(layout = "título_conteúdo", master = "RRF_template_01") %>%
  
  ph_with(value = "Transferências Correntes", 
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue("Cota-Parte do FPE"),
          location = ph_location_type(type = "subTitle")) %>%
  
  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>% 
  
  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia", 
          location = ph_location_type(type = "ftr")) %>% 
  
  ph_with(value = empty_content(), 
          location = ph_location_type(type = "sldNum")) %>% 
  #  
  ph_with(tabela_acumulado, 
          location = ph_location(left = 0.3, top = 1.2)) %>% 
  
  ph_with(dml(code = plot(fig1)), 
          location = ph_location(left =8.1 , top = 1.7, 
                                 width = 5, height = 4)) %>% 
  
  ph_with(block_list(fpar(ftext(glue('(Valores em milhões de R$)'), 
                                prop = fp_text(font.size = 12, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 1.5, top = 0.9, 
                                 width = 6.5,
                                 height = 0.3)) 


rm(fig1, tabela_acumulado)
print('FPE <- OK')


# FUNDEB -----------------------------------------------------------------------
# EXECUTANDO O INSUMO DO FUNDEB
source(encoding = 'UTF-8', file = './4- INSUMO/FUNDEB.R')


my <- my %>%
  add_slide(layout = "título_conteúdo", master = "RRF_template_01") %>%

  ph_with(value = "Transferências Correntes",
          location = ph_location_type(type = "title")) %>%

  ph_with(value = glue("Transferências do FUNDEB"),
          location = ph_location_type(type = "subTitle")) %>%

  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>%

  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia",
          location = ph_location_type(type = "ftr")) %>%

  ph_with(value = empty_content(),
          location = ph_location_type(type = "sldNum")) %>%
  #
  ph_with(tabela_acumulado,
          location = ph_location(left = 0.3, top = 1.2)) %>%

  ph_with(dml(code = plot(fig1)),
          location = ph_location(left =8.1 , top = 1.7,
                                 width = 5, height = 4)) %>%

  ph_with(block_list(fpar(ftext(glue('(Valores em milhões de R$)'),
                                prop = fp_text(font.size = 12,
                                               color = "#292929")),
                          fp_p = border1)),

          location = ph_location(left = 1.5, top = 0.9,
                                 width = 6.5,
                                 height = 0.3))

rm(FPE_band, FUNDEB_band, fig1, FUNDEB, FPE)

print('FUNDEB <- OK')
