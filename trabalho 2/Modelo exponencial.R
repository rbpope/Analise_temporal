rm(list = ls())
library(readxl)
db <- read_excel("Dados Telefonia Movel.xlsx")

library(forecast)
#Cria a série temporal
db_ts <- ts(db$`Telefonia Móvel`, start=c(2015,1), end=c(2021, 3), frequency = 12)
#Análise estatística
summary(db_ts)
boxplot(db_ts)
#Plota o gráfico da série temporal
plot(db_ts, xlab="Tempo", ylab= "Despesa", ylim=c(300000, 6100000), bty="l")
#Separar as amostras:
#1/3 da amostra = 25 - fica a crtério e do tamanho da amostra
#Amostra de Validação ou amostra de teste:
amostra_validacao <- 20
#Amostra de treino é o total - a amostra de teste:
amostra_treino <- length(db_ts) - amostra_validacao
#Criar a série temporal de treinamento:
treinamento_ts <-window(db_ts, start=c(2015, 1), end=c(2015, amostra_treino))
#Criar a série temporal de teste ou de Validação:
validacao_ts <- window(db_ts, start=c(2015, amostra_treino + 1), end=c(2015, amostra_treino + amostra_validacao))
#Avaliar as amostras
treinamento_ts
validacao_ts
#Plota gráfico da serie temporal de treinamento e teste
plot(treinamento_ts, xlab="Tempo", ylab="Despesa", xaxt="n", ylim=c(300000, 6100000), xlim=c(2015, 2021), bty="l")
axis(1,at=seq(2015, 2021, 1), labels=format(seq(2015,2021,1)))
lines(validacao_ts, bty="l", col="red")
#Estima o modelo de tendência exp
modelo_tendencia_exp <- tslm(treinamento_ts ~ trend, lambda=0)

#resumo do modelo
summary(modelo_tendencia_exp)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_tendencia_exp$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-0.5, 0.5), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_tendencia_exp$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_tendencia_exp, test="LB")

