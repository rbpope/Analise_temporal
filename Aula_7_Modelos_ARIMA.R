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


#diferenciando series de dados
#plota o grafica da projecao
par(mfrow=c(2,2))
plot(passageiros_ts, ylab="Passageiros", xlab="Tempo", bty="l", xlim=c(1991,2004.25), main=("Serie sem diff"))
plot(diff(passageiros_ts, lag=1), ylab="Passageiros", xlab="Tempo", bty="l", xlim=c(1991,2004.25), main=("diff lag 1"))
plot(diff(passageiros_ts, lag=12), ylab="Passageiros", xlab="Tempo", bty="l", xlim=c(1991,2004.25), main=("diff lag 12"))
plot(diff(diff(passageiros_ts, lag=12), lag=1), ylab="Passageiros", xlab="Tempo", bty="l", xlim=c(1991,2004.25), main=("diff lag 12 e entao diff lag 1"))

#checar estacionariedade
checkresiduals(passageiros_ts)

checkresiduals(diff(passageiros_ts, lag=1))

checkresiduals(diff(passageiros_ts, lag=12))

checkresiduals(diff(diff(passageiros_ts, lag=12), lag=1))



#carrega o arquivo com a seria temporal Ibov
Ibov <- read.csv("Ibovespa.csv")

#transforma em série temporal
Ibov_ts <- ts(Ibov$Adj.Close, start=c(2006,1), frequency = 12)

#plot gráfico
par(mfrow=c(1,1))

plot(Ibov_ts)

#checa a autocorrelação
checkresiduals(Ibov_ts)
Box.test(Ibov_ts, type="Ljung")

#diferencia 1 vez
Ibov_ts_diff <- diff(Ibov_ts, lag=1)

#checa a autocorrelação
checkresiduals(Ibov_ts_diff)
Box.test(Ibov_ts_diff, type="Ljung")

#Carrega Biblioteca de testes estatísticos
library(urca)

#executa o teste de KPSS
summary(ur.kpss(Ibov_ts))


#executa o teste de KPSS
summary(ur.kpss(Ibov_ts_diff))


#executa o teste de ADF
summary(ur.df(Ibov_ts))


#executa o teste de ADF
summary(ur.df(Ibov_ts_diff))


#############################################################
# MODELO ARIMA
#############################################################

#remove a sazonalidade da base completa
passageiros_ts_diff <- diff(passageiros_ts, lag=12)

par(mfrow=c(1,2))
plot(passageiros_ts)
plot(passageiros_ts_diff)
par(mfrow=c(1,1))

ajuste_sazonal_passageiros <- passageiros_ts-passageiros_ts_diff

plot(ajuste_sazonal_passageiros)

#separa as amostras em treinamento e teste

#define o tamanho da amostra de teste
tam_amostra_teste <- 36

#define o tamanho da amostra de treinamento
tam_amostra_treinamento <- length(passageiros_ts_diff) - tam_amostra_teste

#cria a serie temporal de treinamento
treinamento_ts_diff <- window(passageiros_ts_diff, start=c(1992, 1), end=c(1992,tam_amostra_treinamento))

#cria a serie temporal de teste
validacao_ts_diff <- window(passageiros_ts_diff, start=c(1992, tam_amostra_treinamento + 1), end=c(1992,tam_amostra_treinamento+tam_amostra_teste))


#executa o teste de KPSS
summary(ur.kpss(treinamento_ts_diff))


#executa o teste de ADF
summary(ur.df(treinamento_ts_diff))

#calcula a ACF
Acf(treinamento_ts_diff)

#calcula a PCF
Pacf(treinamento_ts_diff)

#Modelo Arima
Modelo_ARIMA <- Arima(treinamento_ts_diff, order = c(2,1,1))

#resumo modelo
summary(Modelo_ARIMA)

#projeta os proximos 12 meses
modelo_ARIMA_proj <- forecast(Modelo_ARIMA, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ARIMA_proj, ylab="Passageiros", xlab="Tempo", bty="l", xaxt="n", xlim=c(1992,2006.25), flty=2)

axis(1, at=seq(1992, 2006, 1), labels=format(seq(1992, 2006, 1)))

lines(Modelo_ARIMA$fitted, lwd=2, col="blue")

lines(validacao_ts_diff)

#verifica precisao
accuracy(modelo_ARIMA_proj, validacao_ts_diff)

#função auto.arima
auto.arima(treinamento_ts_diff, seasonal = FALSE, stepwise=FALSE, approximation = FALSE)

