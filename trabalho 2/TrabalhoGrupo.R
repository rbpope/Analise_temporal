#limpa variaveis da memoria
rm(list = ls())

install.packages("forecast")
library(forecast)

#Importar Dados Telefonia Movel.xlsx
library(readxl)
db <- read_excel("Dados Telefonia Movel.xlsx")

#Transformar em série temporal
db_ts <- ts(db$`Telefonia Móvel`, start=c(2015, 1), end=c(2021, 3), frequency = 12)

#analise estatistica
summary(db_ts)

#grafico da serie temporal
plot(db_ts, xlab="Tempo", ylab="Despesas", ylim=c(342000, 6013000), type="l")

#Amostra Treinamento
treinamento_ts <- window(db_ts, start=c(2015, 1), end=c(2019,7))

#Amostra Validação
validacao_ts <- window(db_ts, start=c(2019, 8), end=c(2021,3))

#Grafico da série temporal de treinamento e validação
plot(treinamento_ts, xlab="Tempo", ylab="Despesas", xaxt="n" , ylim=c(342000, 6013000), xlim=c(2015, 2021), type="l")
axis(1, at=seq(2015, 2021), labels=format(seq(2015, 2021)))
lines(validacao_ts, bty="l", col="red")

#Modelo Naive
modelo_naive <- naive(treinamento_ts, level=0, h=20)
accuracy(modelo_naive, validacao_ts)

#Grafico da série temporal de treinamento, validação e modelo naive
plot(modelo_naive, xlab="Tempo", ylab="Despesas", xaxt="s" , ylim=c(342000, 6013000), xlim=c(2015, 2021), bty="l", flty=2)
axis(1, at=seq(2015, 2021), labels=format(seq(2015, 2021)))
lines(validacao_ts, bty="l", col="red")

#modelo de tendência linear com sazonalidade
modelo_tendencia_linear_sazonalidade <- tslm(treinamento_ts ~ season+trend)
summary(modelo_tendencia_linear_sazonalidade)

plot(modelo_tendencia_linear_sazonalidade$residuals, xlab="Tempo", ylab="Resíduos",ylim=c(-1500000, 4000000), type="l")
Acf(modelo_tendencia_linear_sazonalidade$residuals)
checkresiduals(modelo_tendencia_linear_sazonalidade, test="LB")

plot(db_ts, xlab="Tempo", ylab="Despesas", ylim=c(342000, 6013000), type="l")
lines(modelo_tendencia_linear_sazonalidade$fitted.values, lwd=2, col="Blue")
modelo_tendencia_linear_sazonalidade_proj <- forecast(modelo_tendencia_linear_sazonalidade, h = 55, level=0.95)        

plot(modelo_tendencia_linear_sazonalidade_proj, xlab="Tempo", ylab="Despesas", xaxt="s" , ylim=c(-1500000, 6013000), xlim=c(2015, 2021), type="l", flty=3)
axis(1, at=seq(2015, 2021), labels=format(seq(2015, 2021)))
lines(validacao_ts)
lines(modelo_tendencia_linear_sazonalidade_proj$fitted, lwd=2, col="blue")

accuracy(modelo_tendencia_linear_sazonalidade_proj, validacao_ts)

#modelo de Média Móvel
install.packages("zoo")
library(zoo)

ma_simples <- rollmean(db_ts, k=12, align="right")
ma_centrada <- ma(db_ts, order=12)

plot(db_ts, ylim=c(342000, 6013000), ylab="Despesas", xlab="Tempo", bty="l", xaxt="s", xlim=c(2015,2021))
axis(1, at=seq(2015, 2021, 1), labels=format(seq(2015, 2021, 1)))
lines(ma_centrada, lwd=2)
lines(ma_simples, lwd=2, lty=2)
legend(2019,7000000, c("Despesas", "MA Centrada", "MA Simples"), lty=c(1,1,2), lwd=c(1,2,2), bty="n")

ma_simples_treinamento <- rollmean(treinamento_ts, k=36, align="right")
ultima_ma <- tail(ma_simples_treinamento, 1)

ma_simples_proj <- ts(rep(ultima_ma, 55), start=c(2015, 56), end = c(2015, 75), freq=12)

plot(treinamento_ts, ylim=c(342000, 6013000), ylab="Despesas", xlab="Tempo", bty="l", xaxt="n", xlim=c(2015,2021))
axis(1, at=seq(2015, 2021), labels=format(seq(2015, 2021)))
lines(ma_simples_treinamento, lwd=2, col="blue")
lines(ma_simples_proj, lwd=2, lty=2, col="blue")
lines(validacao_ts)

plot(treinamento_ts-ma_simples_treinamento, xlab="Tempo", ylab="Resíduos", ylim=c(-1500000, 4000000), bty="l")
Acf(treinamento_ts-ma_simples_treinamento)
checkresiduals(treinamento_ts-ma_simples_treinamento, test="LB")

accuracy(ma_simples_treinamento, treinamento_ts)
accuracy(ma_simples_proj, validacao_ts)

#Modelo de Suavização Exponencial Model(ZZZ)
modelo_ses1 <- ets(treinamento_ts, model = "ZZZ")
summary(modelo_ses1)

modelo_ses1_proj <- forecast(modelo_ses1, h=20, level=0.95)

plot(modelo_ses1_proj, ylim=c(-300000, 6013000), ylab="Despesas", xlab="Tempo", bty="l", xaxt="n", xlim=c(2015,2021), flty=2)
axis(1, at=seq(2015, 2021), labels=format(seq(2015, 2021)))
lines(modelo_ses1$fitted, lwd=2, col="blue")
lines(validacao_ts)

accuracy(modelo_ses1_proj, validacao_ts)

Acf(modelo_ses1_proj$residuals)
checkresiduals(modelo_ses1_proj, test="LB")


