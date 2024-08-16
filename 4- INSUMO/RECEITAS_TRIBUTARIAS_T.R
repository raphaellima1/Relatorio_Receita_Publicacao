##############################################
# Criação base diaria
##############################################
index = c(2,4,1,5,6,3)

filtro <- c('ICMS','IPVA','Adicional de 2% ICMS','Contribuições ao PROTEGE',
            'Contribuição ao FUNDEINFRA','ITCD')

data_fim <- max(receitas_base$data)

tabela_total <- receitas_base %>% 
  mutate(mes = month(data)) %>%
# FILTRA AS RECEITAS PARA (MÊS ATUAL-1) DE 2023 E 2024
  filter(mes == month(data_fim) & Ano >= 2023) %>%
  group_by(Ano, Tipo) %>% 
# CONVERTE OS VALORES PARA MILHÕES E SOMA O QUE FOI ARRECADADO AO LONGO DO MÊS PARA CADA UMA DAS RECEITAS
  summarise(Valor = sum(Valor)/1000000) %>% 
# TRANSPOSTA: OS ANOS VIRAM COLUNAS
  pivot_wider(names_from = Ano, values_from = Valor) %>% 
  arrange(Tipo) %>%
  mutate(index_ordem = index) %>% 
  arrange(index) %>% 
  setNames(c('Tipo', 'mes_23', 'mes_24')) %>% 
  select(1:3) %>% 
  
  add_column(col_space = NA, .name_repair = "universal") %>% 
  
# PUXANDO DADOS DAS RECEITAS PARA CALCULAR O ACUMULADO ATÉ O MÊS ANTERIOR AO ATUAL EM 2023 E 2024
  bind_cols(receitas_base %>% 
              mutate(mes = month(data)) %>% 
              filter(mes <= month(data_fim), Ano >= 2023) %>% 
              group_by(Ano, Tipo) %>% 
              summarise(Valor = sum(Valor)/1000000) %>% 
              pivot_wider(names_from = Ano, values_from = Valor) %>% 
              arrange(Tipo) %>% 
              mutate(index_ordem = index) %>% 
              arrange(index) %>%
              setNames(c('Tipo', 'acum_23', 'acum_24')) %>% 
              select(2:3)
  ) %>% 
  
  add_column(col_space = NA, .name_repair = "universal") %>% 
# ANEXANDO O VALOR ARRECADADO PROJETADO PRO MÊS ANTERIOR AO ATUAL 
  bind_cols(projecao_base %>% 
              filter(mes == month(data_fim)) %>%
              group_by(DESCRIÇÃO) %>% 
              summarise(valor = sum(Valor)/1000000) %>% 
              filter(DESCRIÇÃO %in% filtro) %>% 
              mutate(DESCRIÇÃO = case_when(
                DESCRIÇÃO == "Adicional de 2% ICMS" ~ 'Adicional 2%',
                DESCRIÇÃO == "Contribuições ao PROTEGE" ~ 'PROTEGE',
                DESCRIÇÃO == "Contribuição ao FUNDEINFRA" ~ 'FUNDEINFRA',
                TRUE ~ DESCRIÇÃO)) %>% 
              arrange(DESCRIÇÃO) %>% 
              mutate(index_ordem = index) %>% 
              arrange(index) %>% 
              rename('projecao_mes' = 'valor') %>% 
              select(2)
  ) %>% 
# ANEXANDO O VALOR ARRECADADO PROJETADO ACUMULADO ATÉ O MÊS ANTERIOR AO ATUAL
  bind_cols(projecao_base %>% 
              filter(mes <= month(data_fim)) %>%
              group_by(DESCRIÇÃO) %>% 
              summarise(valor = sum(Valor)/1000000) %>% 
              filter(DESCRIÇÃO %in% filtro) %>% 
              mutate(DESCRIÇÃO = case_when(
                DESCRIÇÃO == "Adicional de 2% ICMS" ~ 'Adicional 2%',
                DESCRIÇÃO == "Contribuições ao PROTEGE" ~ 'PROTEGE',
                DESCRIÇÃO == "Contribuição ao FUNDEINFRA" ~ 'FUNDEINFRA',
                TRUE ~ DESCRIÇÃO)) %>% 
              arrange(DESCRIÇÃO) %>% 
              mutate(index_ordem = index) %>% 
              arrange(index) %>% 
              rename('projecao_acum' = 'valor') %>% 
              select(2)
  ) %>% 
  
  add_column(col_space = NA, .name_repair = "universal") %>%
# CALCULAR AS DIFERENÇAS
  mutate(dif_mes = mes_24 - projecao_mes,
         dif_acum = acum_24 - projecao_acum) %>% 
  adorn_totals(na.rm = TRUE, fill = " ") %>% 
  setNames(c("Arrecadação", "2023", "2024", " ", ' 2023', ' 2024', "  ",
             " Mensal", " Acumulado", "   ", 'Mensal', 'Acumulado'))



# EDITANDO A TABELA A SER PLOTADA NO SLIDE -------------------------------------
# CONVERTENDO O DATA FRAME EM IMAGEM, E RENOMEANDO AS COLUNAS
tabela_acumulado <- tabela_total %>%
  flextable() %>% 
  border_remove() %>%
  padding( i=c(2,3), j=1, padding.left=15) %>% 
  colformat_double(j = c("2023", "2024", ' 2023', ' 2024',
                         " Mensal", " Acumulado"),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 0, 
                   na_str = "--") %>%
# CONVERTENDO "." PARA "," E COLOCANDO VALOR NEGATIVO PRA FICAR VERMELHO
  colformat_double(j = c('Mensal', 'Acumulado'),
                   big.mark=".",
                   decimal.mark = ',', 
                   digits = 2, 
                   na_str = "--") %>% 
  color( ~ Mensal < 0, ~ Mensal,  color = cor1[4]) %>% 
  color( ~ Acumulado < 0, ~ Acumulado,  color = cor1[4]) %>% 
  set_header_labels(values = c('Arrecadação',"2023", "2024",'', '2023', '2024','',
                               "Mensal", "Acumulado",'', "Mensal", "Acumulado")) %>%
# DEFININDO AS CORES DAS LINHAS PRINCIPAL E SECUNDÁRIAS, E ADICIONANDO NEGRITO NA LINHA PRINCIPAL
  bg(., 
     part = "header", 
     bg = cor1[2]) %>% 
  style( pr_t = fp_text_default(
    bold = T,
    color = cor1[1]
  ),
  part = 'header') %>% 
  bg(., i= c(1,4,6), 
     part = "body", 
     bg = cor1[1]) %>%
# ALTERANDO A COR DA LINHA DO TOTAL
  bg(., i= ~ Arrecadação == "Total", 
     part = "body", 
     bg = cor1[2]) %>%
# COLOCANDO A COR BRANCA E ADICIONANDO NEGRITO NA LINHA DO TOTAL
  style(i =  ~ Arrecadação == "Total", 
        pr_t = fp_text_default(
          bold = T,
          color = cor1[1]
        )) %>%
  hline(i = c(6,7), part = "body", 
        border =  std_border) %>% 
# ADICIONANDO UMA LINHA ACIMA DA LINHA PRINCIPAL
  add_header_row(values = c('Arrecadação', 'Mensal', '  ', "Acumulado (Ano)",
                            '   ', "Projeções", '    ', 'Diferença em R$ (Real./24) - (Proj./24)'), 
                 colwidths = c(1,2,1,2,1,2,1,2)) %>%
# MESCLANDO AS LINHAS 1 E 2 DA COLUNA 1
  merge_at(i = 1:2, j = 1, part = "header") %>% 
# CENTRALIZANDO 
  align(i = 1, j = NULL, align = "center", part = "header") %>% 
  hline(i = 1, j = c(2,3,5,6,8,9,11,12), part = "header", 
        border =  std_border) %>% 
  width(j = c(4,7,10), width = .2, unit = 'cm') %>% 
  width(j = 1, width = 2.8, unit = 'cm') %>% 
  width(j = c(2,3,5,6,8,9,11,12), width = 1.8, unit = 'cm') |> 
  width(j = c(9,12), width = 2.4, unit = 'cm') 

tabela_acumulado
