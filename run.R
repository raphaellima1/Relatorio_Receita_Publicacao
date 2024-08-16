# mes_atualizacao <- Sys.Date() %m-% months(1)
mes_atualizacao <- Sys.Date()

tempo_execucao <- system.time({
suppressMessages({

  pacman::p_load(tidyverse, readxl, lubridate, glue, ggpubr,gridExtra,grid,
                 ggthemes,officer, gt, kableExtra, magrittr,rvg, 
                 mschart, janitor,sidrar, lemon,zoo, rlang,flextable, 
                 htmltools,GetBCBData,rbcb,dplyr,ggplot2,tidyverse,zoo,magrittr,
                 ggthemes,scales,parallel,profvis)
  
# setwd('C:/Users/rapha/OneDrive/Documentos/Trabalhos em R/Relatorio_receita')


# Configurações gerais ----------------------------------------------------
  source( encoding = 'UTF-8', file = './1 - IMPORTAÇÃO/3 - configurações gerais.R')
  
# Rodar o script ----------------------------------------------------------
  source( encoding = 'UTF-8', file = './3 - pptx/0 - geral.R')

  })
})


print(tempo_execucao)
