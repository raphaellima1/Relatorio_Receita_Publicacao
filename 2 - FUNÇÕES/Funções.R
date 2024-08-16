ajust_base_diaria<- function(base = NULL, ano = NULL) {
  df_data <- DATE_base %>% 
    filter(year(data) == ano) %>% 
    arrange(data)
  
  #filtrar a tabela para 2019
  tabela <- base %>% 
    filter(Ano == ano) %>%
    mutate(dm = paste(dia = day(data), 
                      mes = month(data), 
                      sep ='/')) %>% 
    group_by(dm) %>% 
    summarise(Valor = sum(Valor))
  
  # cobinar as tabela
  df <- merge(df_data,tabela,  
              by = 'dm',
              all.x = T) %>% 
    arrange(data)
  
  df[is.na(df)] <-  0
  
  df <- df %>% 
    mutate(acumulado = cumsum(Valor))
  
  return(df)
  
}
