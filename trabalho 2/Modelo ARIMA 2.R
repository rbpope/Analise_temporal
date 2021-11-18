#limpa variaveis da memoria
rm(list = ls())

library(zoo)
library(forecast)
library(urca)
library(readxl)
#carrega o arquivo com a seria temporal de passageiros bruta
db <- read_excel("Dados Telefonia Movel.xlsx")
#coverte a serie de gastos bruta no formato de serie temporal
db_ts <- ts(db$`Telefonia Móvel`, start=c(2015,1), end=c(2021,3), frequency = 12)

#plota o grafico da serie temporal"
plot(db_ts, xlab="Tempo", ylab="Despesas", main="Gráfico da Série Temporal", bty="l")


#diferenciando series de dados
#plota o grafica da projecao
par(mfrow=c(2,2))
plot(db_ts, ylab="Despesas", xlab="Tempo", bty="l", xlim=c(2015,2021.25), main=("Serie sem diff"))
plot(diff(db_ts, lag=1), ylab="Despesas", xlab="Tempo", bty="l", xlim=c(2015,2021.25), main=("diff lag 1"))
plot(diff(db_ts, lag=12), ylab="Despesas", xlab="Tempo", bty="l", xlim=c(2015,2021.25), main=("diff lag 12"))
plot(diff(diff(db_ts, lag=12), lag=1), ylab="Despesas", xlab="Tempo", bty="l", xlim=c(2015,2021.25), main=("diff lag 12 e entao diff lag 1"))

#checar estacionariedade
checkresiduals(db_ts)

checkresiduals(diff(db_ts, lag=1))

checkresiduals(diff(db_ts, lag=12))

checkresiduals(diff(diff(db_ts, lag=12), lag=1))

#diferencia 1 vez
db_ts_diff <- diff(db_ts, lag=1)

#executa o teste de KPSS
summary(ur.kpss(db_ts))


#executa o teste de KPSS
summary(ur.kpss(db_ts_diff))


#executa o teste de ADF
summary(ur.df(db_ts))


#executa o teste de ADF
summary(ur.df(db_ts_diff))


#############################################################
# MODELO ARIMA
#############################################################

par(mfrow=c(1,2))
plot(db_ts)
plot(db_ts_diff)
par(mfrow=c(1,1))

ajuste_sazonal_db <- db_ts-db_ts_diff

plot(ajuste_sazonal_db)

#separa as amostras em treinamento e teste

#define o tamanho da amostra de teste
amostra_teste <- 20

#define o tamanho da amostra de treinamento
amostra_treino <- length(db_ts_diff) - amostra_teste



#cria a serie temporal de treinamento
treinamento_ts_diff <- window(db_ts_diff, start=c(2015, 1), end=c(2015, amostra_treino))

#cria a serie temporal de teste
validacao_ts_diff <- window(db_ts_diff, start=c(2015, amostra_treino + 1), end=c(2015,amostra_treino + amostra_teste))


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
modelo_ARIMA_proj <- forecast(Modelo_ARIMA, h=amostra_teste, level=0.95)
#plota o grafica da projecao
plot(modelo_ARIMA_proj, ylab="Despesa", xlab="Tempo", bty="l", xaxt="n", xlim=c(2015,2024.25), flty=2)

axis(1, at=seq(2015, 2024, 1), labels=format(seq(2015, 2024, 1)))

lines(Modelo_ARIMA$fitted, lwd=2, col="blue")

lines(validacao_ts_diff)

#verifica precisao
accuracy(modelo_ARIMA_proj, validacao_ts_diff)

#função auto.arima
auto.arima(treinamento_ts_diff, seasonal = FALSE, stepwise=FALSE, approximation = FALSE)

