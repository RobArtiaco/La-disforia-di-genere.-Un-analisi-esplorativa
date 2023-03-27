library(FactoMineR) 
library(factoextra) 
library(tidyverse)
library(rio)
library(corrplot)
library(tidyr)
library(ggplot2)


classifica_diritti = classifica_diritti%>%
  select(paese, diritti)

classifica_diritti=data.frame(classifica_diritti, row.names = 1)
colnames(classifica_diritti)[colnames(classifica_diritti) == "paese"] <- "CountryCode"


-------------------------------------------------------#Correspondence Analysis#----------------------------------------------------------------------------

# rimozione dei paesi dove il campione < 100 e la media europea 
# e creazione delle tabelle di contingenza per ciascuna delle 
# domande nella sezione specifica per le persone transgender


----------------------------------------------------------------------------------------------------------------------------------------------------------

# Change of legal gender
ac_change_legal_gender = change_legal_gender %>%
  filter(CountryCode != "Slovenia" & 
           CountryCode != "Cyprus" & 
           CountryCode != "EU-28" & 
           CountryCode != "Malta" & 
           CountryCode != "North Macedonia" & 
           CountryCode != "Luxembourg") %>%
  select(CountryCode, answer, percentage)


ac_change_legal_gender %>%
  ggplot(aes(x = Yes , y=(fct_reorder(X, Yes)))) + geom_col(fill = "#8AB7DD") + xlab("Paesi") + ylab("% di Si") + 
  labs(title = "Hai cambiato il tuo genere legale?") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(size = 20, face="bold"))+
  theme(panel.background = element_rect(fill = "#FFF8E9"),
        panel.grid.major = element_line(size = 0),
        panel.grid.minor = element_line(size = 0),
        plot.background= element_rect(fill= '#FFD2CC'),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.title.y = element_text("p",size = 20, face="bold"))
        

--------------------------------------------------------------------------------------------------------------------------------------------------------
summary(ac_reasons_not_change)
#Reason for not yet having legal gender changed
ac_reasons_not_change = reasons_not_change %>%
  filter(CountryCode != "Slovenia" & 
           CountryCode != "Cyprus" & 
           CountryCode != "EU-28" & 
           CountryCode != "Malta" & 
           CountryCode != "North Macedonia" & 
           CountryCode != "Luxembourg") %>%
  select(CountryCode, answer, percentage)

ac_reasons_not_change = spread(ac_reasons_not_change, answer, value = percentage)

colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "X"] <- "CountryCode"

colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "I.do.not.agree.with.the.provisions.of.the.law"] <- "disaccordo con le leggi"
colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "I.do.not.fulfil.the.requirements.of.the.law"] <- "requisiti di legge"
colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "I.don.t.know.if.I.can"] <- "non so se posso"
colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "I.don.t.think.it.s.necessary"] <- "non necessario"
colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "I.don.t.want.to"] <- "non voglio"
colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "There.is.no.legal.procedure.in.this.country.to.change.my.legal.gender"] <- "nessuna legge"
colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "I.think.it.s.too.difficult"] <- "difficoltà"
colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "I.think.it.s.too.expensive"] <- "costi"
colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "I.would.like.to.do.so.in.the.future"] <- "lo farò in futuro"
colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "My.application.was.rejected"] <- "richiesta negata"
colnames(ac_reasons_not_change)[colnames(ac_reasons_not_change) == "Other"] <- "altro"

unique(colnames(ac_reasons_not_change))


ac_reasons_not_change = merge(ac_reasons_not_change, classifica_diritti, by="CountryCode")
ac_reasons_not_change = data.frame(ac_reasons_not_change, row.names = 1)
CA_reasons_not_change = CA(ac_reasons_not_change, ncp=5)

CA_reasons_not_change = CA(ac_reasons_not_change, ncp=5,  col.sup = 12)


fviz_ca(CA_reasons_not_change, 
        repel = T, 
        col.row = "cos2", 
        col.col = "black", 
        labelsize=10, 
        gradient.cols = c("red", "darkgreen")) + 
  theme(panel.background = element_rect(fill = '#FFD2CC'),
        panel.grid.major = element_line(size = 0),
        panel.grid.minor = element_line(size = 0),
        plot.background= element_rect(fill= "#8AB7DD"),
        legend.title = element_text(size = 20, face="bold"),
        legend.text = element_text(size = 20, face="bold")) +
  labs(title = "Reason for not yet having legal gender changed") +
  theme(plot.title = element_text(size = 20, face="bold"),
        axis.title.x = element_text(size=20, face="bold", color = "black"),
        axis.title.y = element_text(size=20, face="bold", color = "black"))


res_reasons_not_change = chisq.test(ac_reasons_not_change)
row.contrib=CA_reasons_not_change$row$contrib
row.cos2=CA_reasons_not_change$row$cos2
col.contrib=CA_reasons_not_change$col$contrib
col.cos2=CA_reasons_not_change$col$cos2

write.csv(row.contrib, "row_contrib_reasons.csv")
write.csv(row.cos2, "row_cos2_reasons.csv")
write.csv(col.contrib, "col_contrib_reasons.csv")
write.csv(col.cos2, "col_cos2_reasons.csv")

eig_val_reasons = CA_reasons_not_change[["eig"]]
write.csv(eig_val_reasons, "eig_val_reasons.csv")

fviz_eig(CA_reasons_not_change, addlabels = T, ylim = c(0, 80)) + 
  theme(panel.background = element_rect(fill = '#FFD2CC'),
        panel.grid.major = element_line(size = 0),
        panel.grid.minor = element_line(size = 0),
        plot.background= element_rect(fill= "#8AB7DD"),
        legend.title = element_text(size = 20, face="bold"),
        legend.text = element_text(size = 20, face="bold")) +
  labs(title = "Screeplot degli autovalori") +
  theme(plot.title = element_text(size = 20, face="bold"),
        axis.title.x = element_text(size=20, face="bold", color = "black"),
        axis.title.y = element_text(size=20, face="bold", color = "black"))


-----------------------------------------------------------------------------------------------------------------------------------------------------

#Avoiding expression of gender through physical appearance for fear of being assaulted, threatened or harassed

always = mean(avoid_gender_ex$Always)
Dont.know = mean(avoid_gender_ex$Dont.know) 
Never = mean(avoid_gender_ex$Never) 
Often = mean(avoid_gender_ex$Often)
Rarely = mean(avoid_gender_ex$Rarely) 

profilo_medio_avoid = data.frame(always = always, Dont.know = Dont.know, Never=Never, often= Often, rarely = Rarely)

ac_avoiding_gender_expression = avoiding_gender_expression %>%
  filter(CountryCode != "Slovenia" & 
           CountryCode != "Cyprus" & 
           CountryCode != "EU-28" & 
           CountryCode != "Malta" & 
           CountryCode != "North Macedonia" & 
           CountryCode != "Luxembourg") %>%
  select(CountryCode, answer, percentage)

ac_avoiding_gender_expression %>%
  arrange(desc(Often))

ac_avoiding_gender_expression = spread(ac_avoiding_gender_expression, answer, value = percentage)
ac_avoiding_gender_expression = merge(ac_avoiding_gender_expression,classifica_diritti,by="CountryCode")
ac_avoiding_gender_expression = data.frame(avoid_gender_ex, row.names = 1)
res_avoiding_gender_expression = chisq.test(ac_avoiding_gender_expression) #test chiquadro


CA_avoiding_gender_expression = CA(ac_avoiding_gender_expression, ncp=5,  col.sup = 6)#avviamo l'analisi
summary(CA_avoiding_gender_expression)#chiamiamo i risultati in console

CA_avoiding_gender_expression = CA(ac_avoiding_gender_expression, ncp=5, row.sup=NULL)#avviamo l'analisi


res_CA_avoiding_gender_expression = chisq.test(ac_avoiding_gender_expression)
row.contrib=CA_avoiding_gender_expression$row$contrib
row.cos2=CA_avoiding_gender_expression$row$cos2
col.contrib=CA_avoiding_gender_expression$col$contrib
col.cos2=CA_avoiding_gender_expression$col$cos2


fviz_ca(CA_avoiding_gender_expression, 
        repel = T, 
        col.row = "cos2", 
        col.col = "black", 
        labelsize=10, 
        gradient.cols = c("red", "darkgreen")) + 
  theme(panel.background = element_rect(fill = '#FFD2CC'),
        panel.grid.major = element_line(size = 0),
        panel.grid.minor = element_line(size = 0),
        plot.background= element_rect(fill= "#8AB7DD"),
        legend.title = element_text(size = 20, face="bold"),
        legend.text = element_text(size = 20, face="bold")) +
  labs(title = "Avoiding expression of gender through physical appearance for fear of being assaulted, threatened or harassed") +
  theme(plot.title = element_text(size = 20, face="bold"),
        axis.title.x = element_text(size=20, face="bold", color = "black"),
        axis.title.y = element_text(size=20, face="bold", color = "black"))


fviz_eig(CA_avoiding_gender_expression, addlabels = TRUE, ylim = c(0, 80))


write.csv(row.contrib, "row_contrib_avoid.csv")
write.csv(row.cos2, "row_cos2_avoid.csv")
write.csv(col.contrib, "col_contrib_avoid.csv")
write.csv(col.cos2, "col_cos2_avoid.csv")

eig_val_avoid = CA_avoiding_gender_expression[["eig"]]
write.csv(eig_val_avoid, "eig_val_avoid.csv")

fviz_eig(CA_avoiding_gender_expression, addlabels = T, ylim = c(0, 80)) + 
  theme(panel.background = element_rect(fill = '#FFD2CC'),
        panel.grid.major = element_line(size = 0),
        panel.grid.minor = element_line(size = 0),
        plot.background= element_rect(fill= "#8AB7DD"),
        legend.title = element_text(size = 20, face="bold"),
        legend.text = element_text(size = 20, face="bold")) +
  labs(title = "Screeplot degli autovalori") +
  theme(plot.title = element_text(size = 20, face="bold"),
        axis.title.x = element_text(size=20, face="bold", color = "black"),
        axis.title.y = element_text(size=20, face="bold", color = "black"))



