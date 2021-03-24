#limpa variaveis da memoria
rm(list = ls())

#carrega o pacote de projecao
library(forecast)

#carrega o arquivo com a seria temporal de passageiros bruta
dados_passageiros <- read.csv("Dados/Amtrak data.csv")

#coverte a serie de passageiros bruta no formato de serie temporal
passageiros_ts <- ts(dados_passageiros$Ridership, start=c(1991,1), end=c(2004,3), frequency = 12)

#analise estatistica
summary(dados_passageiros)

summary(passageiros_ts)


#plota o grafico da serie temporal
plot(passageiros_ts, xlab="Tempo", ylab="Passageiros", ylim=c(1300, 2300), type="l")

plot(dados_passageiros$Ridership, xlab="Tempo", ylab="Passageiros", ylim=c(1300, 2300),type="p")
