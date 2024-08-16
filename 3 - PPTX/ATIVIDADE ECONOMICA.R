
###############################################
# ATIVIDADE ECONÔMICA
###############################################
source(encoding = 'UTF-8', file = './4- INSUMO/ATIV_ECONOMICA_G.R')

my <- my %>%
  add_slide(layout = "título_conteúdo", master = "RRF_template_01") %>%
  
  ph_with(value = "Conjuntura econômica",
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue("Indicadores"),
          location = ph_location_type(type = "subTitle")) %>%
  
  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>%
  
  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia",
          location = ph_location_type(type = "ftr")) %>%
  
  ph_with(value = empty_content(),
          location = ph_location_type(type = "sldNum"))%>%
  
  # ph_with(dml(code = plot(fig1)),
  #         location = ph_location(left =8.1 , top = 1.7,
  #                                width = 5, height = 4))%>%
  
  ph_with(dml(code = plot(fig.allg)), 
          location = ph_location(left = 1.5,
                                 top = 1.03,
                                 width = 10,
                                 height = 6.2))


print('ATIVIDADE ECONÔMICA <- OK')