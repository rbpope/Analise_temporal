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
#Estimar o modelo de tendencia linear:
modelo_tendencia_linear <- tslm(treinamento_ts ~ trend)
#resumo do modelo
summary(modelo_tendencia_linear)
#Plotando os resíduos:
plot(modelo_tendencia_linear$residuals, xlab="Tempo", ylab="Resíduos", bty="l", main="Resíduos do modelo de regreção linear")

#Calcula a autocorrelação dos resíduos
Acf(modelo_tendencia_linear$residuals, main="Modelo de Tendencia Linear")
#Plot da modelo com a tendencia
plot(treinamento_ts, xlab="Tempo", ylab="Despesa", bty="l", main="Modelo com tendência")
lines(modelo_tendencia_linear$fitted.values, lwd="2", col="orange")
#Teste de Ljung-Box
checkresiduals(modelo_tendencia_linear, test="LB")
#projeta o modelo durante o período de validação
modelo_tendencia_linear_proj <- forecast(modelo_tendencia_linear, h=amostra_validacao, level=0.95)
#plota o grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_linear_proj, xlab="Tempo", ylab="Despesas", xaxt="n" , xlim=c(2015, 2021.75), bty="l", flty=2)
axis(1, at=seq(2015, 2021, 1), labels=format(seq(2015,2021,1)))
lines(validacao_ts, col="red")
lines(modelo_tendencia_linear_proj$fitted, lwd="2", col="blue")
#Testa a acurácia
accuracy(modelo_tendencia_linear_proj, validacao_ts)
#Modelo Naive
modelo_naive <- naive(treinamento_ts, level=0, h=amostra_validacao)
accuracy(modelo_naive, validacao_ts)
#Plotar o modelo Naive
plot(modelo_naive, xlab="Tempo", ylab="Despesas", xaxt="s" , ylim=c(342000, 6013000), xlim=c(2015, 2021), bty="l", flty=2)
axis(1, at=seq(2015, 2021,1), labels=format(seq(2015, 2021,1)))
lines(validacao_ts, bty="l", col="red")
#Projetar os 36 meses futuros
#Reestimar o modelo com toda a base (treinamento + validação)
modelo_tendencia_linear_final <- tslm(db_ts ~ trend)
#Sumário do modelo:
summary(modelo_tendencia_linear_final)
#projeta os próximos 36 meses (o Número de meses é o h):
modelo_tendencia_linear_final_proj <- forecast(modelo_tendencia_linear_final, h=36, level=0.95)
#PLota o gráfico da projeção:
plot(modelo_tendencia_linear_final_proj, xlab="Tempo", ylab="Despesa", xaxt="s" , ylim=c(342000, 6013000), xlim=c(2015, 2025), bty="l", flty=2, main="Previsão do modelo de regressão linear")
axis(1, at=seq(2015, 2025,1), labels=format(seq(2015, 2025,1)))
lines(modelo_tendencia_linear_final_proj$fitted, bty="l", col="red")
