rm(list = ls())
library(readxl)
db <- read_excel("Dados Telefonia Movel.xlsx")
library(lubridate)
library(forecast)
psych::describe(db)
#Cria a série temporal
db_ts <- ts(db$`Telefonia Móvel`, start=c(2015,1), end=c(2021, 3), frequency = 12)
#Análise estatística
psych::describe(db_ts)
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
#plota a série temporal de cada ano por mês
ggseasonplot(db_ts)

#cria dummies mensais
dummies_mensais <- seasonaldummy(db_ts)

#Estima o modelo de tendência linear
modelo_sazonalidade_linear <- tslm(treinamento_ts ~ season)

#resumo do modelo
summary(modelo_sazonalidade_linear)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_sazonalidade_linear$residuals, xlab="Tempo", ylab="Resíduos", main="Resíduos do Modelo de Sazonalidade Linear", bty="l")
#calcula a autocorrelação dos resíduos
Acf(modelo_sazonalidade_linear$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_sazonalidade_linear, test="LB")

#plot modelo com sazonalidade
plot(treinamento_ts, xlab="Tempo", ylab="Despesas", main="Resíduos do modelo de Sazonalidade linear", bty="l")
lines(modelo_sazonalidade_linear$fitted.values, lwd=2, col="blue")

#projeta o modelo durante o período de validação
modelo_sazonalidade_linear_proj <- forecast(modelo_sazonalidade_linear, h = amostra_validacao, level=0.95)
#plota o grafico da serie temporal de treinamento e teste
plot(modelo_sazonalidade_linear_proj, xlab="Tempo", ylab="Passageiros", xaxt="n" , main="Série Temporal Treinamento e Teste", xlim=c(2015, 2021.25), bty="l", flty=2)

axis(1, at=seq(2015, 2021, 1), labels=format(seq(2015, 2021,1)))

lines(validacao_ts)
lines(modelo_sazonalidade_linear_proj$fitted, lwd=2, col="blue")


#Verifica a acuracia do modelo
accuracy(modelo_sazonalidade_linear_proj, validacao_ts)

