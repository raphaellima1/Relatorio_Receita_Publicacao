########################################
# tabela do Realizado
#########################################

realizado <- read_excel('./0 - DADOS/RECEITA_LIQUIDA_MONITOR_PLDO25_20240417.xlsx', sheet = 'realizado', skip = 2)

projeção <- read_excel('./0 - DADOS/Previsão_da_Receita_PLDO_2025_cenario_320_realizado_202401_v3 (1).xlsx', sheet = 'PREVISÃO RCL (MENSAL)')

projeção1 <- read_excel('./0 - DADOS/RECEITA_LIQUIDA_MONITOR_PLDO25_20240417.xlsx', sheet = 'PLDO2025')

e_orca <- read_excel("0 - DADOS/DESPESA_-_Cenários_Fiscais.xlsx", sheet = 2)
