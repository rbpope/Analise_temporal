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

#Estima o modelo de tendencia Poli
modelo_tendencia_poli <- tslm(treinamento_ts ~ trend + I(trend^2))

#resumo do modelo
summary(modelo_tendencia_poli)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_tendencia_poli$residuals, xlab="Tempo", ylab="Resíduos", bty="l", main="Resíduos")

#calcula a autocorrelação dos resíduos
Acf(modelo_tendencia_poli$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_tendencia_poli, test="LB")

#plot modelo com tendencia
plot(treinamento_ts, xlab="Tempo", ylab="Despesas", bty="l", main="Modelo com tendência")
lines(modelo_tendencia_poli$fitted.values, lwd=2)

#projeta o modelo durante o período de validação
modelo_tendencia_poli_proj <- forecast(modelo_tendencia_poli, h = amostra_validacao, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_poli_proj, xlab="Tempo", ylab="Despesas", xaxt="n" , xlim=c(2015, 2021.25), bty="l", flty=2,main="Previsão do modelo Polinomial")

axis(1, at=seq(2015, 2021, 1), labels=format(seq(2015, 2021,1)))

lines(validacao_ts)
lines(modelo_tendencia_poli_proj$fitted, lwd=2, col="blue")


#Verifica a acuracia do modelo
accuracy(modelo_tendencia_poli_proj, validacao_ts)


#Calula o modelo naive
modelo_naive <- naive(treinamento_ts, level=0, h=amostra_validacao)

#Verifica a acuracia do modelo naive
accuracy(modelo_naive, validacao_ts)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_naive, xlab="Tempo", ylab="Despesas", xaxt="n", xlim=c(2016, 2021.25), bty="l", flty=2)

axis(1, at=seq(2015, 2021, 1), labels=format(seq(2015, 2021,1)))

lines(validacao_ts)

#projetar para os próximos 36 meses no futuro

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
modelo_tendencia_poli_final <- tslm(db_ts ~ trend + I(trend^2))


#sumario do modelo
summary(modelo_tendencia_poli_final)

#projeta os próximos 36 meses do futuro
modelo_tendencia_poli_final_proj <- forecast(modelo_tendencia_poli_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_poli_final_proj, xlab="Tempo", ylab="Despesas", , xlim=c(2015, 2025), bty="l", flty=2, main="Forecast from Polynomial regression model")
axis(1, at=seq(2015, 2025, 1), labels=format(seq(2015, 2025,1)))
lines(modelo_tendencia_poli_final_proj$fitted, lwd=2, col="blue")


