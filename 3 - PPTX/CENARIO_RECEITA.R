###############################################
# Resumo Gerencial 323 (monitoramento da receita)
###############################################

#executando o insumo
source(encoding = 'UTF-8', file = './4- INSUMO/CENARIO_RECEITA_T.R')


# criação e configuração do slide
my <- my %>%
  add_slide(layout = "título_conteúdo",  master = "RRF_template_01") %>%
  
  ph_with(value = "Cenários da Receita Orçamentária",
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
          location = ph_location(left = 0.5, top = 1.)) %>%
  
  ph_with(block_list(fpar(ftext(glue('(Valores em milhões de R$)'),
                                prop = fp_text(font.size = 12,
                                               color = "#292929")),
                          fp_p = border1)),
          
          location = ph_location(left = 6.2, top = 0.75,
                                 width = 6.501,
                                 height = 0.201)) 

rm(df, df_acum23, df_acum24,df_G323, df_G323_T, df_PLOA2024,tabela_acumulado,
   TAB_GER, original_col_names, new_col_names,mes_dados)

print('CENARIO RECEITA <- OK')