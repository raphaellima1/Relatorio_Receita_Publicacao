index = c(4,2,1,5,6,3)

tabela_resumo_trib <- receitas_base %>% 
  filter(Ano >= 2023) %>% 
  select(Tipo, Ano, Valor) %>% 
  group_by(Tipo,Ano) %>% 
  summarise(Valor = sum(Valor), .groups = 'drop') %>% 
  pivot_wider(names_from = Ano, values_from = Valor) %>% 
  arrange(Tipo) %>% 
  mutate(index_ordem = index) %>% 
  arrange(index) %>% 
  adorn_totals() %>% 
  select(1:3) %>% 
  mutate(perc = (`2024`/tail(`2024`,n = 1)*100))

