install.packages("zoo")
#limpa variaveis da memoria
rm(list = ls())

library(zoo)

#cadastra o nome dos meses
mes_nomes <- c("julho/15", "agosto/15", "setembro/15","outubro/15", "novembro/15", "dezembro/15","janeiro/16")

#define um indice numérico para os meses para estimacao e predicao
mes <- 1:7
dados_pred <- data.frame(mes)
mes <- 1:6

#cadastra o valor de vendas
vendas <- c(14, 16, 19, 22, 24, 26)

#estima um modelo linear para vendas
modelo_linear <- lm(vendas ~ mes)

#mostra o resumo do modelo
summary(modelo_linear)

#projeta linearmente janeiro de 2016
proj_linear <- predict(modelo_linear,newdata=dados_pred)

#estima um modelo exponencial para vendas
modelo_exponencial <- lm(vendas ~ log(mes))

#projeta exponencialemente janeiro de 2016
proj_exponencial <- predict(modelo_exponencial,newdata=dados_pred)

#estima um modelo autoregressivo para vendas
vendas_lag1 <- vendas[1:5]
vendas_norm <- vendas[2:6]

modelo_ar <- lm(vendas_norm ~ vendas_lag1)

#resumo modelo AR
summary(modelo_ar)

#ajusta dados_pred
dados_pred_ar <- data.frame(vendas_lag1 = vendas_norm)

#projeta ar janeiro de 2016
proj_ar <- predict(modelo_ar,newdata=dados_pred_ar)


#plot o grafico com as projecoes
plot(xaxt="n",vendas, main="Projecao Vendas", ylim=c(10,30), xlim=c(1,7) , xlab="mes")

lines(proj_linear, col="green", lty=2)
lines(modelo_linear$fitted.values, col="green", type="l", lwd=1)


lines(proj_exponencial, col="red",lty=2)
lines(modelo_exponencial$fitted.values, col="red",lwd=1)

lines(c(NA,NA,proj_ar), col="blue",lty=2)
lines(c(NA,modelo_ar$fitted.values), col="blue",lwd=1)


legend("topleft", legend=c("modelo linear", "modelo exponencial", "modelo autoregressivo"), col=c("green", "red", "blue"), lty=1:2, cex=0.8)

axis(1, at=1:7, labels=mes_nomes)

