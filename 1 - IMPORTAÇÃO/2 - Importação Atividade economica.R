
#importando a série do PIB----------------------------------
PIB <- get_sidra(5932, variable=6562, period=c("last" = 16)) %>%
  select('Valor', 'Trimestre (Código)', 'Setores e subsetores (Código)') %>%
  setNames(c('pib', 'trimestre', 'setores')) %>%
  filter(setores == '90707') %>%
  mutate(trimestre = case_when(
    substr(trimestre, 5, 6) == "01" ~ paste0(substr(trimestre, 1, 4), "-03-01"),
    substr(trimestre, 5, 6) == "02" ~ paste0(substr(trimestre, 1, 4), "-06-01"),
    substr(trimestre, 5, 6) == "03" ~ paste0(substr(trimestre, 1, 4), "-09-01"),
    substr(trimestre, 5, 6) == "04" ~ paste0(substr(trimestre, 1, 4), "-12-01"))) %>%
  mutate(trimestre = ymd(trimestre))

start_data<-paste0(year(Sys.Date())-2,glue("-{month(Sys.Date())}-01"))
start_data1<-paste0(year(Sys.Date())-3,glue("-{month(Sys.Date())}-01"))

#importando a série do ipca---------------------------------
ipcabr<-gbcbd_get_series(433,first.date=start_data1) %>% 
  setNames(c('data','ValorBR'))%>%
  select(data,ValorBR) |> 
  mutate(ipcabr12 = round(rollapply(
    ValorBR,12,function(x)(prod(1+x/100)-1)*100,
    by.column=F,align='right',fill=NA),2)) |> 
  filter(!is.na(ipcabr12))


#importando o IPCA de Goiânia
ipcago<-get_series(13255,start_date=start_data1) %>%
  setNames(c('data','ValorGO')) %>%
mutate(ipcago12=round(rollapply(ValorGO,12,
                                 function(x)(prod(1+x/100)-1)*100,
                                 by.column=F,align='right',fill=NA),2))
ipcago<-ipcago %>%
  filter(!is.na(ipcago12))

#mesclando os dois IPCAs
ipca<-ipcabr %>%
  merge(ipcago,by='data') %>%
  select('data','ipcabr12','ipcago12')
last_ipca <- ipca %>%
  filter(data == max(data))


#taxa de juros Selic----------------
selic<-get_series(432,start_date=start_data) %>% setNames(c('data','selic'))

last_selic <- selic %>%
  filter(data == max(data))


#taxas de câmbio-------------------------
#importando os dados do Dólar dos EUA e do Euro--------------------------
dolar<-get_currency('USD',start_date = start_data,end_date = Sys.Date())%>%
  setNames(c('data','Venda_USD','Compra_USD'))
euro<-get_currency('EUR',start_date = start_data,end_date=Sys.Date())%>%
  setNames(c('data','Venda_EUR','Compra_EUR'))
cotacao<-dolar%>%
  merge(euro,by='data')%>%
  select('data','Venda_USD','Venda_EUR')%>%
  mutate(Venda_USD=round(Venda_USD,2),Venda_EUR=round(Venda_EUR,2))
last_cotacao <- cotacao %>% filter(data == max(data))

