#limpa variaveis da memoria
rm(list = ls())

library(zoo)
library(forecast)
library(urca)

#carrega o arquivo com a seria temporal de passageiros bruta
dados_passageiros <- read.csv("Amtrak_data.csv")

#coverte a serie de passageiros bruta no formato de serie temporal
passageiros_ts <- ts(dados_passageiros$Ridership, start=c(1991,1), end=c(2004,3), frequency = 12)

#plota o grafico da serie temporal
plot(passageiros_ts, xlab="Tempo", ylab="Passageiros", ylim=c(1300, 2300), bty="l")

#separa as amostras em treinamento e teste

#define o tamanho da amostra de teste
tam_amostra_teste <- 36

#define o tamanho da amostra de treinamento
tam_amostra_treinamento <- length(passageiros_ts) - tam_amostra_teste

#cria a serie temporal de treinamento
treinamento_ts <- window(passageiros_ts, start=c(1991, 1), end=c(1991,tam_amostra_treinamento))

#cria a serie temporal de teste
validacao_ts <- window(passageiros_ts, start=c(1991, tam_amostra_treinamento + 1), end=c(1991,tam_amostra_treinamento+tam_amostra_teste))


#Modelo Arima
Modelo_ARIMA <- auto.arima(treinamento_ts, stepwise=FALSE, approximation = FALSE)


#resumo modelo
summary(Modelo_ARIMA)

#projeta os proximos 12 meses
modelo_ARIMA_proj <- forecast(Modelo_ARIMA, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ARIMA_proj, ylab="Passageiros", xlab="Tempo", bty="l", xaxt="n", xlim=c(1991,2006.25), flty=2)

axis(1, at=seq(1991, 2006, 1), labels=format(seq(1991, 2006, 1)))

lines(Modelo_ARIMA$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ARIMA_proj, validacao_ts)

checkresiduals(modelo_ARIMA_proj)


#Preparar projecao

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
Modelo_ARIMA_final <- auto.arima(passageiros_ts, stepwise=FALSE, approximation = FALSE)

#sumario do modelo
summary(Modelo_ARIMA_final)

#projeta os próximos 36 meses do futuro
Modelo_ARIMA_final_proj <- forecast(Modelo_ARIMA_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(Modelo_ARIMA_final_proj, xlab="Tempo", ylab="Passageiros", ylim=c(1300, 2800), xlim=c(1991, 2007), bty="l", flty=2, main="Forecast from Polynomial regression model")
axis(1, at=seq(1991, 2007, 1), labels=format(seq(1991, 2007,1)))
lines(Modelo_ARIMA_final_proj$fitted, lwd=2, col="orange")

