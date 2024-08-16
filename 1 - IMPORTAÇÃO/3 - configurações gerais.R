border1 <- fp_par(
  text.align = "right",
  shading.color = "#2d2e2d")

border2 <- fp_par(
  text.align = "center",
  shading.color = "#2d2e2d")


# configuração de paleta de cor -------------------------------------------

# tabela 
cor1 <- c('#E5F5EB', # linhas
          "#008E52", # cabeçalho
          '#ffffff', # Letra
          '#bf1e2e' # Numero negativo
          )
# Gráfico
cor2 <- c('#002E54', # Ano corrente
          '#009e3c', # Ano Anterior
          '#bf1e2e'  # Projeção
          )

# Configuração do Flextable

flex_tab1 <- set_flextable_defaults(
  font.size = 12, font.family = "Calibri",
  font.color = "#333333",
  table.layout = "fixed",
  border.color = "gray",
  padding.top = 3, padding.bottom = 3,
  padding.left = 4, padding.right = 6)


# configuração de borda
std_border <- fp_border(color = cor1[3], width = 0.5)

dados <- c('Receita Corrente Líquida','Receita Total','Cenário da Receita',
           'Transferências Correntes', 'Receitas tributárias')
sumario <- paste(dados, collapse = "\n")
n_sumario <- '1.\n2.\n3.\n4.\n5.'




# Esqueleto das apresentações ---------------------------------------------

espaco <- function(n_espacos, vezes, dado = NULL){
  a <- rep(n_espacos, vezes)
  b <- paste(a, collapse = "")
  c <- paste0(b, dado)
  return(c)
}
# sb <- espaco(n_espacos = '\n', 5, dados[1])


border1 <- fp_par(
  text.align = "right",
  shading.color = "#2d2e2d")
# numero de seções
s <- 1
# Nº de graficos
g <- 1
# Nº de tabelas
t <- 1
