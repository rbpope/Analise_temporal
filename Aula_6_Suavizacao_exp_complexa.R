#limpa variaveis da memoria
rm(list = ls())

library(zoo)
library(forecast)

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


#projeção série com tendência aditiva

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "AAN")

#resumo modelo
summary(modelo_ses)

#projeta os proximos 12 meses
modelo_ses_proj <- forecast(modelo_ses, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ses_proj, ylim=c(1300, 2600), ylab="Passageiros", xlab="Tempo", bty="l", xaxt="n", xlim=c(1991,2006.25), flty=2)

axis(1, at=seq(1991, 2006, 1), labels=format(seq(1991, 2006, 1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ses_proj, validacao_ts)

#calcula a autocorrelação dos resíduos
Acf(modelo_ses$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")

#Preparar projecao

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
modelo_ses_final <- ets(passageiros_ts, model = "AAN")

#sumario do modelo
summary(modelo_ses_final)

#projeta os próximos 36 meses do futuro
modelo_ses_final_proj <- forecast(modelo_ses_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_ses_final_proj, xlab="Tempo", ylab="Passageiros", ylim=c(1300, 2800), xlim=c(1991, 2007), bty="l", flty=2, main="Forecast from Polynomial regression model")
axis(1, at=seq(1991, 2007, 1), labels=format(seq(1991, 2007,1)))
lines(modelo_ses_final_proj$fitted, lwd=2, col="blue")



############################################################################
#projeção série com tendência multiplicativa
############################################################################

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "MMN")

#resumo modelo
summary(modelo_ses)

#projeta os proximos 12 meses
modelo_ses_proj <- forecast(modelo_ses, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ses_proj, ylim=c(1300, 2600), ylab="Passageiros", xlab="Tempo", bty="l", xaxt="n", xlim=c(1991,2006.25), flty=2)

axis(1, at=seq(1991, 2006, 1), labels=format(seq(1991, 2006, 1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ses_proj, validacao_ts)

#calcula a autocorrelação dos resíduos
Acf(modelo_ses$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")

#Preparar projecao

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
modelo_ses_final <- ets(passageiros_ts, model = "MMN")

#sumario do modelo
summary(modelo_ses_final)

#projeta os próximos 36 meses do futuro
modelo_ses_final_proj <- forecast(modelo_ses_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_ses_final_proj, xlab="Tempo", ylab="Passageiros", ylim=c(1300, 2800), xlim=c(1991, 2007), bty="l", flty=2, main="Forecast from Polynomial regression model")
axis(1, at=seq(1991, 2007, 1), labels=format(seq(1991, 2007,1)))
lines(modelo_ses_final_proj$fitted, lwd=2, col="blue")

###############################################################################################
#projeção série com tendência e sazonalidade aditiva
################################################################################################

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "AAA")

#resumo modelo
summary(modelo_ses)

#projeta os proximos 12 meses
modelo_ses_proj <- forecast(modelo_ses, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ses_proj, ylim=c(1300, 2600), ylab="Passageiros", xlab="Tempo", bty="l", xaxt="n", xlim=c(1991,2006.25), flty=2)

axis(1, at=seq(1991, 2006, 1), labels=format(seq(1991, 2006, 1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ses_proj, validacao_ts)

#calcula a autocorrelação dos resíduos
Acf(modelo_ses$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")

#Preparar projecao

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
modelo_ses_final <- ets(passageiros_ts, model = "AAA")

#sumario do modelo
summary(modelo_ses_final)

#projeta os próximos 36 meses do futuro
modelo_ses_final_proj <- forecast(modelo_ses_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_ses_final_proj, xlab="Tempo", ylab="Passageiros", ylim=c(1300, 2800), xlim=c(1991, 2007), bty="l", flty=2, main="Forecast from Polynomial regression model")
axis(1, at=seq(1991, 2007, 1), labels=format(seq(1991, 2007,1)))
lines(modelo_ses_final_proj$fitted, lwd=2, col="blue")

################################################################################################
#projeção série com tendência aditiva e sazonalidade multiplicativa
################################################################################################

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "MAM")

#resumo modelo
summary(modelo_ses)

#projeta os proximos 12 meses
modelo_ses_proj <- forecast(modelo_ses, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ses_proj, ylim=c(1300, 2600), ylab="Passageiros", xlab="Tempo", bty="l", xaxt="n", xlim=c(1991,2006.25), flty=2)

axis(1, at=seq(1991, 2006, 1), labels=format(seq(1991, 2006, 1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ses_proj, validacao_ts)

#calcula a autocorrelação dos resíduos
Acf(modelo_ses$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")

#Preparar projecao

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
modelo_ses_final <- ets(passageiros_ts, model = "MAM")

#sumario do modelo
summary(modelo_ses_final)

#projeta os próximos 36 meses do futuro
modelo_ses_final_proj <- forecast(modelo_ses_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_ses_final_proj, xlab="Tempo", ylab="Passageiros", ylim=c(1300, 2800), xlim=c(1991, 2007), bty="l", flty=2, main="Forecast from Polynomial regression model")
axis(1, at=seq(1991, 2007, 1), labels=format(seq(1991, 2007,1)))
lines(modelo_ses_final_proj$fitted, lwd=2, col="blue")


############################################################################################
#projeção série com algoritmo de busca de melhor modelo
############################################################################################

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "ZZZ", restrict = FALSE, allow.multiplicative.trend = TRUE)

#resumo modelo
summary(modelo_ses)

#projeta os proximos 12 meses
modelo_ses_proj <- forecast(modelo_ses, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ses_proj, ylim=c(1300, 2600), ylab="Passageiros", xlab="Tempo", bty="l", xaxt="n", xlim=c(1991,2006.25), flty=2)

axis(1, at=seq(1991, 2006, 1), labels=format(seq(1991, 2006, 1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ses_proj, validacao_ts)


#calcula a autocorrelação dos resíduos
Acf(modelo_ses$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")
