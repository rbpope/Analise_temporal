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

#calcula a media movel simples
ma_simples <- rollmean(passageiros_ts, k=12, align="right")

#calcula a media centrada
ma_centrada <- ma(passageiros_ts, order=12)

#plota as medias 
plot(passageiros_ts, ylim=c(1300, 2400), ylab="Passageiros", xlab="Tempo", bty="l", xaxt="n", xlim=c(1991,2004.25))

axis(1, at=seq(1991, 2004.25, 1), labels=format(seq(1991, 2004.25, 1)))

lines(ma_centrada, lwd=2)

lines(ma_simples, lwd=2, lty=2)
 
legend(1994,2400, c("Passageiros", "MA Centrada", "MA Simples"), lty=c(1,1,2), lwd=c(1,2,2), bty="n")

#separa as amostras em treinamento e teste

#define o tamanho da amostra de teste
tam_amostra_teste <- 36

#define o tamanho da amostra de treinamento
tam_amostra_treinamento <- length(passageiros_ts) - tam_amostra_teste

#cria a serie temporal de treinamento
treinamento_ts <- window(passageiros_ts, start=c(1991, 1), end=c(1991,tam_amostra_treinamento))

#cria a serie temporal de teste
validacao_ts <- window(passageiros_ts, start=c(1991, tam_amostra_treinamento + 1), end=c(1991,tam_amostra_treinamento+tam_amostra_teste))

#estima o modelo de MA na base de treinamento
ma_simples <- rollmean(treinamento_ts, k=12, align="right")

#obtem a média da última janela movel de 12 meses para projeção
ultima_ma <- tail(ma_simples, 1)

#cria uma projeção que é a repeticao da ultima media da janela para o periodo de validacao
ma_simples_proj <- ts(rep(ultima_ma, tam_amostra_teste), start=c(1991, tam_amostra_treinamento+1), end = c(1991, tam_amostra_treinamento + tam_amostra_teste), freq=12)

#plota o grafica da projecao
plot(treinamento_ts, ylim=c(1300, 2600), ylab="Passageiros", xlab="Tempo", bty="l", xaxt="n", xlim=c(1991,2006.25))

axis(1, at=seq(1991, 2006, 1), labels=format(seq(1991, 2006, 1)))

lines(ma_simples, lwd=2, col="blue")

lines(ma_simples_proj, lwd=2, lty=2, col="blue")

lines(validacao_ts)

#valida a precisao da estimacao no periodo de treinamento
accuracy(ma_simples, treinamento_ts)

#valida a precisao da estimacao no periodo de validacao
accuracy(ma_simples_proj, validacao_ts)

#Plotando os resíduos
plot(treinamento_ts-ma_simples, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(treinamento_ts-ma_simples)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(treinamento_ts-ma_simples, test="LB")




