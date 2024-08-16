setwd("./../")

border1 <- fp_par(
  text.align = "right",
  shading.color = "#2d2e2d")
# numero de seções
s <- 1
# Nº de graficos
g <- 1
# Nº de tabelas
t <- 1

###############################################
# Capa Slide
###############################################

my <- read_pptx('template - v2.pptx') %>% 
  
  ph_with(value = "Boletim Econômico", 
          location = ph_location_type(type = "ctrTitle")) %>% 
  
  ph_with(value = glue("Edição de {format(Sys.Date(), '%d/%m/%Y')}"), 
          location = ph_location_type(type = "subTitle"))


###############################################
# add capa_seção - Seção 1
###############################################
my <- my %>%
  add_slide(layout = "capa_seção", master = "RRF_template_01") %>% 
  ph_with(value = "RECEITA TOTAL", location = ph_location_type(type = "title")) %>% 
  ph_with(value = "1.", location = ph_location_type(type = "subTitle"))







###############################################
# add capa_seção - Seção 2
###############################################
my <- my %>%
  add_slide(layout = "capa_seção", master = "RRF_template_01") %>% 
  ph_with(value = "TRANSFERÊNCIAS CORRENTES", location = ph_location_type(type = "title")) %>% 
  ph_with(value = "2.", location = ph_location_type(type = "subTitle"))


###############################################
# 1.1 - FPE
setwd("./5 - SLIDE - FPE_FUNDEB/")
source( encoding = 'UTF-8', file = '1 - SLIDE.R')
source( encoding = 'UTF-8', file = '1.1 SLIDE.R')

###############################################
# carregar a tabela total chamada df

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
  
  ph_with(block_list(fpar(ftext(glue('(Em R$ milhões)'), 
                                prop = fp_text(font.size = 12, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 1.5, top = 0.9, 
                                 width = 6.5,
                                 height = 0.3)) 


rm(fig1, tabela_acumulado)

###############################################
# 1.1 - FUNDEB
setwd("./5 - SLIDE - FPE_FUNDEB/")
source( encoding = 'UTF-8', file = '2 - SLIDE.R')
source( encoding = 'UTF-8', file = '2.1 SLIDE.R')

###############################################
# carregar a tabela total chamada df


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
  
  ph_with(block_list(fpar(ftext(glue('(Em R$ milhões)'), 
                                prop = fp_text(font.size = 12, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 1.5, top = 0.9, 
                                 width = 6.5,
                                 height = 0.3)) 



###############################################
# RCL
###############################################
setwd("./3.1- SLIDE - RECEITA TOTAL/")
source( encoding = 'UTF-8', file = '1 - SLIDE.R')
source( encoding = 'UTF-8', file = '1.1 SLIDE.R')

my <- my %>%
  add_slide(layout = "título_conteúdo_nota", master = "RRF_template_01") %>%
  
  ph_with(value = "Receita Corrente Líquida (RCL)", 
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue("Comparativo mensal*"),
          location = ph_location_type(type = "subTitle")) %>%
  
  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>% 
  
  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia", 
          location = ph_location_type(type = "ftr")) %>% 
  
  ph_with(value = empty_content(), 
          location = ph_location_type(type = "sldNum")) %>% 
  #  
  ph_with(tabela_acumulado, 
          location = ph_location(left = 0.5, top = 1.3)) %>% 
  
  ph_with(block_list(fpar(ftext(glue('(Em R$ milhões)'), 
                                prop = fp_text(font.size = 12, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 5, top = 1.0, 
                                 width = 6.5,
                                 height = 0.3)) %>% 
  
  ph_with(block_list(fpar(ftext(glue('* Resultados preliminares'),
                                prop = fp_text(font.size = 11,
                                               color = "#292929")))),
          
          location = ph_location(left = 0, top = 0.88,
                                 width = 2,
                                 height = 0.201))


# Receita TOtAL - Gráfico
###############################################
my <- my %>%
  add_slide(layout = "título_conteúdo_nota", master = "RRF_template_01") %>%
  
  ph_with(value = "Receita Corrente Líquida (RCL)", 
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue("Comparativo mensal*"),
          location = ph_location_type(type = "subTitle")) %>%
  
  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>% 
  
  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia", 
          location = ph_location_type(type = "ftr")) %>% 
  
  ph_with(value = empty_content(), 
          location = ph_location_type(type = "sldNum")) %>% 
  
  ph_with(dml(code = plot(fig.allg)), 
          location = ph_location(left = 0.6,
                                 top = 2.1,
                                 width = 12,8,
                                 height = 4.9)) %>% 
  
  ph_with(block_list(fpar(ftext(glue('* Resultados preliminares'),
                                prop = fp_text(font.size = 11,
                                               color = "#292929")))),
          
          location = ph_location(left = 0, top = 0.88,
                                 width = 2,
                                 height = 0.201))


###############################################
# add capa_seção - Seção 3
###############################################
my <- my %>%
  add_slide(layout = "capa_seção", master = "RRF_template_01") %>% 
  ph_with(value = "RECEITAS TRIBUTÁRIAS", location = ph_location_type(type = "title")) %>% 
  ph_with(value = "3.", location = ph_location_type(type = "subTitle"))


###############################################
# 1.1 - Participação percentual por atividade econômica
setwd("./4- SLIDE/")
source( encoding = 'UTF-8', file = '1 - SLIDE.R')
source( encoding = 'UTF-8', file = '1.1 - SLIDE.R')

###############################################
# carregar a tabela total chamada df


my <- my %>%
  add_slide(layout = "título_conteúdo_nota", master = "RRF_template_01") %>%
  
  ph_with(value = "Receitas Tributárias", 
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue("Arrecadação Bruta*"),
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
  
  ph_with(block_list(fpar(ftext(glue('(Em R$ milhões)'), 
                                prop = fp_text(font.size = 12, 
                                               color = "#292929")), 
                          fp_p = border1)),
          
          location = ph_location(left = 1.4, top = 1.6, 
                                 width = 6.5,
                                 height = 0.3)) %>% 
  
  ph_with(block_list(fpar(ftext(glue('* Resultados preliminares'),
                                prop = fp_text(font.size = 11,
                                               color = "#292929")))),
          
          location = ph_location(left = 0, top = 0.88,
                                 width = 2,
                                 height = 0.201))


rm(fig1)

###############################################
# 1.1 - Participação percentual por atividade econômica
setwd("./4- SLIDE/")
source( encoding = 'UTF-8', file = '2 - SLIDE.R')
###############################################
# carregar a tabela total chamada df

my <- my %>%
  add_slide(layout = "título_conteúdo_nota", master = "RRF_template_01") %>%
  
  ph_with(value = "Receitas Tributárias", 
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue("Comparativo em relação ao ano anterior e o projetado*"),
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
                                 top = 1.16,
                                 width = 12,8,
                                 height = 5.9)) %>% 
  
  ph_with(block_list(fpar(ftext(glue('* Resultados preliminares'),
                                prop = fp_text(font.size = 11,
                                               color = "#292929")))),
          
          location = ph_location(left = 0, top = 0.88,
                                 width = 2,
                                 height = 0.201))

rm(fig1, fig2, fig3, fig4, fig.allg)

###############################################
# 1.1 - Participação percentual por atividade econômica
setwd("./4- SLIDE/")
source( encoding = 'UTF-8', file = '3 - SLIDE.R')


###############################################
# carregar a tabela total chamada df



my <- my %>%
  add_slide(layout = "título_conteúdo_nota", master = "RRF_template_01") %>%
  
  ph_with(value = "Detalhamento das Receitas com ICMS", 
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue("Macro-setores estratégicos*"),
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
                                 height = 0.3)) %>% 
  
  ph_with(block_list(fpar(ftext(glue('* Resultados preliminares'),
                                prop = fp_text(font.size = 11,
                                               color = "#292929")))),
          
          location = ph_location(left = 0, top = 0.88,
                                 width = 2,
                                 height = 0.201))

###############################################
# 1.1 - Participação percentual por atividade econômica
setwd("./4- SLIDE/")
source( encoding = 'UTF-8', file = '4 - SLIDE.R')
###############################################
# carregar a tabela total chamada df

my <- my %>%
  add_slide(layout = "título_conteúdo_nota", master = "RRF_template_01") %>%
  
  ph_with(value = "Receitas com ICMS", 
          location = ph_location_type(type = "title")) %>%
  
  ph_with(value = glue("Comparativo em relação ao ano anterior e o projetado*"),
          location = ph_location_type(type = "subTitle")) %>%
  
  ph_with(value = format(Sys.Date(), "%d/%m/%Y"),
          location = ph_location_type(type = "dt")) %>% 
  
  ph_with(value = "Boletim Econômico | Secretaria de Estado da Economia", 
          location = ph_location_type(type = "ftr")) %>% 
  
  ph_with(value = empty_content(), 
          location = ph_location_type(type = "sldNum")) %>% 
  #  
  
  ph_with(dml(code = plot(fig.allg)), 
          location = ph_location(left = 0.32,
                                 top = 1.16,
                                 width = 12.75,
                                 height = 5.9)) %>% 
  
  ph_with(block_list(fpar(ftext(glue('* Resultados preliminares'),
                                prop = fp_text(font.size = 11,
                                               color = "#292929")))),
          
          location = ph_location(left = 0, top = 0.88,
                                 width = 2,
                                 height = 0.201))




rm(fig1)


##############################################
# Finalização da apresentação
##############################################


# add slide equipe_imagem
my <- my %>% 
  add_slide(layout = "equipe", master = "RRF_template_01")

#rm(tabela_receita, p, g, )
my %>%
  print(target = glue("{Sys.Date()}.pptx"))%>% 
  browseURL()

