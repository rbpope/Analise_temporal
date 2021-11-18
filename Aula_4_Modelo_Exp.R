#limpa variaveis da memoria
rm(list = ls())

#carrega o pacote de projecao
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


#plota o grafico da serie temporal de treinamento e teste
plot(treinamento_ts, xlab="Tempo", ylab="Passageiros", xaxt="n" , ylim=c(1300, 2300), xlim=c(1991, 2006.25), bty="l")

axis(1, at=seq(1991, 2006, 1), labels=format(seq(1991, 2006,1)))

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


#plot modelo com tendencia
plot(treinamento_ts, xlab="Tempo", ylab="Passageiros", ylim=c(1300, 2300), bty="l")
lines(modelo_tendencia_exp$fitted.values, lwd=2)

#projeta o modelo durante o período de validação
modelo_tendencia_exp_proj <- forecast(modelo_tendencia_exp, h = tam_amostra_teste, level=0)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_exp_proj, xlab="Tempo", ylab="Passageiros", xaxt="n" , ylim=c(1300, 2300), xlim=c(1991, 2006.25), bty="l", flty=2 ,main="Forecast from Exp regression model")

axis(1, at=seq(1991, 2006, 1), labels=format(seq(1991, 2006,1)))

lines(validacao_ts)
lines(treinamento_ts)
lines(modelo_tendencia_exp_proj$fitted, lwd=2, col="blue")


#Verifica a acuracia do modelo
accuracy(modelo_tendencia_exp_proj, validacao_ts)


#Calula o modelo naive
modelo_naive <- naive(treinamento_ts, level=0, h=tam_amostra_teste)

#Verifica a acuracia do modelo naive
accuracy(modelo_naive, validacao_ts)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_naive, xlab="Tempo", ylab="Passageiros", xaxt="n" , ylim=c(1300, 2300), xlim=c(1991, 2006.25), bty="l", flty=2, main="Forecast from Exp regression model")

axis(1, at=seq(1991, 2006, 1), labels=format(seq(1991, 2006,1)))

lines(validacao_ts)

#projetar para os próximos 36 meses no futuro

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
modelo_tendencia_exp_final <- tslm(passageiros_ts ~ trend, lambda = 0)

#sumario do modelo
summary(modelo_tendencia_exp_final)

#projeta os próximos 12 meses do futuro
modelo_tendencia_exp_final_proj <- forecast(modelo_tendencia_exp_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_exp_final_proj, xlab="Tempo", ylab="Passageiros", ylim=c(1300, 2300), xlim=c(1991, 2007), bty="l", flty=2, main="Forecast from Exp regression model")
axis(1, at=seq(1991, 2007, 1), labels=format(seq(1991, 2007,1)))
lines(modelo_tendencia_exp_final_proj$fitted, lwd=2, col="blue")

lines(validacao_ts)
lines(treinamento_ts)
