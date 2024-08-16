source( encoding = 'UTF-8', file = './1 - IMPORTAÇÃO/1 - importação Receita Total.R')

# Capa --------------------------------------------------------------------
my <- read_pptx('template - v2.pptx') %>% 
  
  ph_with(value = "Boletim Econômico", 
          location = ph_location_type(type = "ctrTitle")) %>% 
  
  ph_with(value = glue("Edição de {format(Sys.Date(), '%d/%m/%Y')}"), 
          location = ph_location_type(type = "subTitle"))


# Sumário -----------------------------------------------------------------
my <- my %>%
  add_slide(layout = "capa_seção", 
            master = "RRF_template_01") %>% 
  
  ph_with(value = sumario, 
          location = ph_location_type(type = "title")) %>% 
  
  ph_with(value = n_sumario, 
          location = ph_location_type(type = "subTitle")) |> 
  ph_with(value = 'SUMÁRIO', 
          location = ph_location(left = 7.5, top = 1., 
                                 width = 3, height = 1))

# Receita Corrente Líquida RCL --------------------------------------------

my <- my %>%
  add_slide(layout = "capa_seção", 
            master = "RRF_template_01") %>% 
  
  ph_with(value = espaco('\n',0,dados[1]), 
          location = ph_location_type(type = "title")) %>% 
  
  ph_with(value = espaco('\n',0,'1.'), 
          location = ph_location_type(type = "subTitle"))

# Slide PPTX da RCL
source( encoding = 'UTF-8', file = './3 - pptx/RCL.R')


# Receita Total -----------------------------------------------------------
my <- my %>%
  add_slide(layout = "capa_seção", master = "RRF_template_01") %>%
  ph_with(value = espaco('\n',1,dados[2]), location = ph_location_type(type = "title")) %>%
  ph_with(value = espaco('\n',vezes = 1,'2.'), location = ph_location_type(type = "subTitle"))

source( encoding = 'UTF-8', file = './3 - pptx/RECEITA TOTAL.R')


# Cenário da Receita ------------------------------------------------------
my <- my %>%
  add_slide(layout = "capa_seção", master = "RRF_template_01") %>%
  ph_with(value = espaco('\n',2,dados[3]), location = ph_location_type(type = "title")) %>%
  ph_with(value = espaco('\n',vezes = 2,'3.'), location = ph_location_type(type = "subTitle"))

source( encoding = 'UTF-8', file = './3 - pptx/CENARIO_RECEITA.R')


# TRANSFERÊNCIAS CORRENTES -----------------------------------------------

my <- my %>%
  add_slide(layout = "capa_seção", master = "RRF_template_01") %>%
  ph_with(value = espaco('\n',3,dados[4]), location = ph_location_type(type = "title")) %>%
  ph_with(value = espaco('\n',vezes = 3,'4.'), location = ph_location_type(type = "subTitle"))

source( encoding = 'UTF-8', file = './3 - pptx/FPE_FUNDEB.R')

rm(projeção, projeção1, realizado, tabela_acumulado)
# Receitas Tributárias ----------------------------------------------------

# Rodar os dados
source( encoding = 'UTF-8', file = './1 - IMPORTAÇÃO/0 - importação ICMS.R')

my <- my %>%
  add_slide(layout = "capa_seção", master = "RRF_template_01") %>%
  ph_with(value = espaco('\n',4,dados[5]), location = ph_location_type(type = "title")) %>%
  ph_with(value = espaco('\n',vezes = 4,'5.'), location = ph_location_type(type = "subTitle"))

source( encoding = 'UTF-8', file = './3 - pptx/RECEITAS TRIBUTARIAS.R')

source( encoding = 'UTF-8', file = './3 - pptx/RECEITAS TRIBUTARIAS G.R')

source( encoding = 'UTF-8', file = './3 - pptx/MACRO SETORES.R')

# # add capa_seção - Seção 1
# ###############################################
# my <- my %>%
#   add_slide(layout = "capa_seção", master = "RRF_template_01") %>% 
#   ph_with(value = "EXECUÇÃO ORÇAMENTÁRIA", location = ph_location_type(type = "title")) %>% 
#   ph_with(value = "1.", location = ph_location_type(type = "subTitle"))
# 
# 
# # Execução orçamentária ---------------------------------------------------
# source( encoding = 'UTF-8', file = './3 - pptx/EXECUÇÃO ORÇAMENTARIA.R')
# 
# source( encoding = 'UTF-8', file = './3 - pptx/EXECUÇÃO ORÇAMENTARIA_E.R')
# # Despesa Liquidada -------------------------------------------------------
# source( encoding = 'UTF-8', file = './3 - pptx/DESPESA LIQUIDADA.R')
# # Finalização da apresentação
# ##############################################
# 
#   source( encoding = 'UTF-8', file = './1 - IMPORTAÇÃO/2 - Importação Atividade economica.R')
# # Adicionar capa de seção - Atividade econômica --------------------------------
# my <- my %>%
#   add_slide(layout = "capa_seção", master = "RRF_template_01") %>% 
#   ph_with(value = "CONJUNTURA ECONÔMICA", location = ph_location_type(type = "title")) %>% 
#   ph_with(value = "1.", location = ph_location_type(type = "subTitle"))
# 
# source( encoding = 'UTF-8', file = './3 - pptx/ATIVIDADE ECONOMICA.R')
# 
# # add slide equipe_imagem
# my <- my %>% 
#   add_slide(layout = "equipe", master = "RRF_template_01")

# Tranferências Constitucionais -------------------------------------------


#rm(tabela_receita, p, g, )
my %>%
  print(target = glue("{Sys.Date()}V2.pptx"))%>% 
  browseURL()

