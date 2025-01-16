#----------------------------------------------------------#
# Para rodar este programa  deixe no objeto fit.model a sa�da 
# do  ajuste  da  regress�o com  erro normal heterosced�stico. 
# Deixe  os dados dispon�veis  atrav�s do comando attach(...). 
# Depois  use  o comando source(...) no S-Plus ou R para
# executar o programa. A sequ�ncia de comandos � a seguinte:
#
#        > fit.model <- ajuste
#        > attach(dados)
#        > source("diag_norm_dglm")
#
# A sa�da ter� quatro gr�ficos: de pontos de alavanca, 
# de pontos influentes  e  dois de res�duos. Para identificar
# os pontos que  mais  se destacam usar o comando 
# identify(...). Se por exemplo se destacam tr�s pontos no
# plot(fitted(fit.model),h,...), ap�s esse comando coloque
#     
#        > identify(fitted(fit.model),h,n=3)
#
# O mesmo pode ser feito nos demais gr�ficos. Nos gr�ficos de 
# res�duos foram tra�ados os limites ylim=c(a-1,b+1), em que a
# � o menor valor e b o maior valor para o res�duo..Mude esses 
# limites  se  necess�rio. 
#----------------------------------------------------------#
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
require(dglm)
fi <- fitted(fit.model$dispersion)
fi <- 1/fi
for(i in 1:n){
X[i,] = fi[i]*X[i,]
}
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
r <- resid(fit.model)
ts <- (sqrt(fi)*r)/sqrt(1-h)
di <- (h/(1-h))*(ts^2)
a <- max(ts)
b <- min(ts)
par(mfrow=c(2,2))
plot(h,xlab="indice", ylab="Medida h", pch=16)
cut <- 2*p/n
abline(cut,0,lty=2)
#identify(h, n=3)
#
plot(di,xlab="indice", ylab="Distancia de Cook", pch=16)
#identify(di, n=3)
#
plot(ts,xlab="indice", ylab="Residuo Studentizado",
ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(tsi, n=1)
#
plot(fitted(fit.model),ts,xlab="Valor Ajustado", 
ylab="Residuo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),tsi, n=1)
par(mfrow=c(1,1))
#-----------------------------------------------------------#
