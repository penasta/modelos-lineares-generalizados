#------------------------------------------------------------#
# Para rodar este programa  deixe no objeto fit.model a sa�da 
# do  ajuste  da  regress�o dupla com  erros gama.
# Deixe  os dados dispon�veis  atrav�s do comando 
# attach(...). Depois  use  o comando source(...) no 
# S-Plus ou R para executar o programa. 
# A sequ�ncia de comandos � a seguinte:
#
#        > fit.model <- ajuste
#        > attach(dados)
#        > source("diag_gama_dglm")
#
# A sa�da ter� quatro gr�ficos: de pontos de alacanca, 
# de pontos influentes  e  dois de res�duos. 
# Para identificar os pontos que  mais  se destacam 
# usar o comando identify(...). Se por
# exemplo se destacam tr�s pontos no 
# plot(fitted(fit.model),h,...), ap�s esse comando coloque
#     
#        > identify(fitted(fit.model),h,n=3)
#
# O mesmo pode ser feito nos demais gr�ficos. Nos gr�ficos de 
# res�duos foram colocados os limites ylim=c(a-1,b+1), 
# em que a � o menor valor e b o maior valor para o res�duo. 
# Este programa usa a library dglm para estimar os par�metros
# fi da gama que  estar� guardado no objeto fi.
#------------------------------------------------------------#
par(mfrow=c(2,2))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
library(dglm)
fi <- fitted(fit.model$dispersion)
fi <- 1/fi
w <- fit.model$weights
w <- w*fi
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ts <- resid(fit.model,type="pearson")*sqrt(fi/(1-h))
td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
di <- (h/(1-h))*(ts^2)
a <- max(td)
b <- min(td)
plot(fitted(fit.model),h,xlab="Media Ajustada", 
ylab="Medida h", pch=16)
#identify(fitted(fit.model), h, n=2)
#
plot(di,xlab="indice", ylab="Distancia de Cook", pch=16)
#identify(di, n=1)
#
plot(fitted(fit.model),td,xlab="Valor Ajustado", 
ylab="Residuo Componente do Desvio",
ylim=c(b-1,a+1),pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),td, n=1)
#
w <- fit.model$weights
eta <- predict(fit.model)
z <- eta + resid(fit.model, type="pearson")/sqrt(w)
plot(predict(fit.model),z,xlab="Preditor Linear", 
ylab="Variavel z", pch=16)
par(mfrow=c(1,1))
#------------------------------------------------------------#

