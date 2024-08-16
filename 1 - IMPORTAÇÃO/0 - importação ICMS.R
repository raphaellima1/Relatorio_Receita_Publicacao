# Tabela do ICMS ----------------------------------------------------------


ICMS_base <- read_excel("./0 - DADOS/ICMS_TOTAL_2024.xlsx", 
                        sheet = "ICMS", 
                        col_types = c("text", "text", "numeric", "text", "numeric")) %>% 
  setNames(c('Tipo', 'Grupo', 'data','Ano', 'Valor')) %>% 
  mutate(data = ymd(data), Ano = year(data), Tipo = 'ICMS')


# Adicional de 2% ---------------------------------------------------------

ADD2_base <- read_excel("./0 - DADOS/ICMS_TOTAL_2024.xlsx", 
                        sheet = "Adicional_2%", 
                        col_types = c("text","text", "numeric", "text", "numeric")) %>% 
  setNames(c('Tipo', 'Grupo', 'data','Ano', 'Valor')) %>% 
  select(Tipo, data,Ano, Valor) %>% 
  mutate(Tipo = 'Adicional 2%') %>% 
  mutate(data = ymd(data), Ano = year(data))


# Tabela PROTEGE ----------------------------------------------------------

PROTEGE_base <- read_excel("./0 - DADOS/ICMS_TOTAL_2024.xlsx", 
                           sheet = "PROTEGE", 
                           col_types = c("text", "text", "numeric", "text", "numeric")) %>% 
  setNames(c('Tipo', 'Grupo', 'data','Ano', 'Valor')) %>% 
  select(Tipo, data, Ano, Valor) %>% 
  mutate(data = ymd(data), Ano = year(data), Tipo = 'PROTEGE') 


# Tabela IPVA -------------------------------------------------------------

IPVA_base <- read_excel("./0 - DADOS/ICMS_TOTAL_2024.xlsx", 
                        sheet = "IPVA", 
                        col_types = c("text", "text", "numeric", "text", "numeric")) %>% 
  setNames(c( 'Tipo', 'Grupo', 'data','Ano', 'Valor')) %>%  
  drop_na() %>% 
  select(Tipo, data, Ano, Valor) %>% 
  mutate(data = ymd(data), Ano = year(data), Tipo = 'IPVA')


# Tabela ITCD -------------------------------------------------------------

ITCD_base <- read_excel("./0 - DADOS/ICMS_TOTAL_2024.xlsx", 
                        sheet = "ITCD", 
                        col_types = c("text", "text", "numeric", "text", "numeric")) %>% 
  setNames(c( 'Tipo', 'Grupo', 'data','Ano', 'Valor')) %>% 
  drop_na() %>% 
  select(Tipo, data, Ano, Valor) %>% 
  mutate(data = ymd(data), Ano = year(data), Tipo = 'ITCD')


# Tabela FUNDEINFRA -------------------------------------------------------

FUNDEINFRA_base <- read_excel("./0 - DADOS/ICMS_TOTAL_2024.xlsx", 
                        sheet = "FUNDEINFRA", 
                        col_types = c("text", "text", "numeric", "text", "numeric")) %>% 
  setNames(c( 'Tipo', 'Grupo', 'data','Ano', 'Valor')) %>% 
  drop_na() %>% 
  select(Tipo, data, Ano, Valor) %>% 
  mutate(data = ymd(data), Ano = year(data), Tipo = 'FUNDEINFRA')


# Tabela total ------------------------------------------------------------

receitas_base <- ICMS_base %>% 
  select(Tipo, data,Ano, Valor) %>% 
  rbind(ADD2_base) %>% 
  rbind(PROTEGE_base) %>% 
  rbind(IPVA_base) %>% 
  rbind(ITCD_base) %>% 
  rbind(FUNDEINFRA_base)

# Tabela projeção ---------------------------------------------------------

projecao_base <- read_excel("./0 - DADOS/Previsão_receitas_tributarias.xlsx", sheet = 2) %>% 
  pivot_longer(cols = Janeiro:Dezembro) %>% 
  mutate(Valor = as.numeric(gsub( ",", ".",value)),
         mes = case_when(
           name == "Janeiro" ~ 1,
           name == "Fevereiro" ~ 2,
           name == "Março" ~ 3,
           name == "Abril" ~ 4,
           name == "Maio" ~ 5,
           name == "Junho" ~ 6,
           name == "Julho" ~ 7,
           name == "Agosto" ~ 8,
           name == "Setembro" ~ 9,
           name == "Outubro" ~ 10,
           name == "Novembro" ~ 11,
           name == "Dezembro" ~ 12,
           TRUE ~ NA_real_
         ),
         ano = 2024) 

# Tabela projeções do ICMS ------------------------------------------------

projecao_ICMS <- read_csv2("./0 - DADOS/previsoes_icms_setor_mar_2024.csv") %>% 
  setNames(c('Setor', 'Mes', 'Valor', 'pred') ) %>% 
  mutate(ano = year(Mes),
         mes = month(Mes)) %>%
  filter(ano > 2022) %>%
  mutate(valor = rowSums(select(., Valor, pred), na.rm = TRUE)) %>% 
  select(Setor, Mes, valor, ano, mes)
  
new_projecoes <- read_excel("./0 - DADOS/Projeções_receita.xlsx", sheet = 1) |> 
  mutate(data = ym(`ANOMES REFERENCIA`))

new_projecoes_base <- read_excel("./0 - DADOS/Projeções_receita.xlsx", sheet = 2)
