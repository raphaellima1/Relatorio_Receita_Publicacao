#gráfico PIB------------------------------------------------

breaks_trimestre <- PIB %>% 
  filter(substr(trimestre, 6, 7) == "12") %>% 
  pull(trimestre)

labels <- format(breaks_trimestre, "%Y-%m")

# Criar o gráfico
fig1 <- PIB %>%
  ggplot(aes(x = trimestre, y = pib, fill = pib > 0)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  geom_text(aes(label = scales::label_number(decimal.mark=',',accuracy=0.1)(pib)), 
            vjust = ifelse(PIB$pib > 0, 1.5, -0.5), color = "white", 
            position = position_dodge(width = 0.7), size = 3.5, fontface = "bold") +
  scale_fill_manual(guide = 'none', breaks = c(TRUE, FALSE),
                    values = c(cor2[2], cor2[3])) +
  scale_x_date(breaks = breaks_trimestre, labels = labels) +
  labs(x = "", y = "% a.a.", 
       title = glue("PIB Brasil - Var. real últimos 4 trimestres ({year(Sys.Date())-4}-{year(Sys.Date())})"), linetype = "Variable",
       color = "Variable") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(0, 0, 0, 0))



#criando o gráfico do IPCA----------------------------------
fig2<-ipca %>% 
  ggplot()+
  geom_line(aes(x = data, y = ipcabr12, color="IPCA Brasil"), size = 1) +
  geom_line(aes(x = data, y = ipcago12, color="IPCA Goiânia"), size = 1) +
  scale_y_continuous(labels=scales::label_number(decimal.mark=','))+
  labs(x = " ", y = "% a.a.", title = glue("IPCA acum. 12 meses ({year(Sys.Date())-2}-{year(Sys.Date())})"), 
       linetype = "Variable", color = "Variable")+
  scale_color_manual(breaks = c('IPCA Brasil','IPCA Goiânia'), 
                     values = c('IPCA Brasil'=cor2[2],'IPCA Goiânia'=cor2[1]),name="") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5, face = "bold"),
        plot.margin = margin(0, 0, 0, 0)) +
  scale_x_date(limits = c(min(ipca$data), max(ipca$data) %m+% months(1))) +
  theme(legend.position=c(0.99,0.9),
        legend.key.size = unit(0.3,'cm'),
        legend.justification = c("right", "bottom"),
        legend.margin = margin(0,0,0,0)) +
  geom_label(aes(x = last_ipca$data, y = last_ipca$ipcago12, 
                 label = paste0(format(last_ipca$ipcabr12,decimal.mark=','),'%')),vjust=0,hjust=0.4, colour = "black") +
  geom_label(aes(x = last_ipca$data, y = last_ipca$ipcabr12, 
                 label = paste0(format(last_ipca$ipcago12,decimal.mark=','),'%')),vjust=0,hjust=0.4, colour = "black")



#gráfico da selic----------------------
fig3<-selic %>% 
  ggplot() +
  geom_line(aes(x = data, y = selic, color="Taxa Selic"), size = 1)+
  scale_y_continuous(labels=scales::label_number(decimal.mark=','))+
  labs(x = " ", y = "% a.a.", title = glue("Selic definida pelo Copom ({year(Sys.Date())-2}-{year(Sys.Date())})"))+
  scale_color_manual(breaks = ('Taxa Selic'), values = ('Taxa Selic'=cor2[1]), name=" ")+
  theme_classic()+theme(plot.title = element_text(hjust=0.5, face = "bold"),
                        legend.title=element_blank(),legend.position='none',
                        plot.margin = margin(0, 0, 0, 0)) +
  scale_x_date(limits = c(min(selic$data), max(selic$data) %m+% months(1))) +
  geom_label(aes(x = last_selic$data, y = last_selic$selic, 
                 label = paste0(format(last_selic$selic,decimal.mark=','),'%')),vjust =0.5, colour = "black")


#criando o gráfico das cotações---------------------------
fig4<-cotacao %>% 
  ggplot() +
  geom_line(aes(x = data, y = Venda_EUR, color="EUR"), size = 1) +
  geom_line(aes(x = data, y = Venda_USD, color="USD"), size = 1) +
  scale_y_continuous(labels=scales::label_number(decimal.mark=','))+
  labs(x = "", y = "R$", title = glue("Cotações diárias do Euro e do Dólar dos EUA ({year(Sys.Date())-2}-{year(Sys.Date())})"),
       linetype = "Variable", color = "Variable")+
  scale_color_manual(breaks = (c('EUR','USD')), values = c('EUR'= cor2[2],'USD'=cor2[1]), name=" ") +
  theme_classic() +
  scale_x_date(limits = c(min(cotacao$data), max(cotacao$data) %m+% months(1)))+
  theme(plot.title = element_text(hjust=0.5, face = "bold"),
        plot.margin = margin(0, 0, 0, 0)) +
  theme(legend.position=c(0.99,0.05),
        legend.key.size = unit(0.3,'cm'),
        legend.justification = c("right", "bottom"),
        legend.margin = margin(0,0,0,0)) +
  geom_label(aes(x = last_cotacao$data, y = last_cotacao$Venda_EUR, 
                 label = paste0("R$ ",format(last_cotacao$Venda_EUR,decimal.mark=','))),
             vjust =1,hjust=0.4, colour = "black") +
  geom_label(aes(x = last_cotacao$data, y = last_cotacao$Venda_USD, 
                 label = paste0("R$ ",format(last_cotacao$Venda_USD,decimal.mark=','))),
             vjust =1,hjust=0.4, colour = "black")


#figuras agrupadas---------------------------
fig.allg <- ggarrange(fig1, fig2, fig3, fig4, ncol = 2, nrow = 2, common.legend = F)

