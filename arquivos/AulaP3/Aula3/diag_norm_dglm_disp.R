#------------------------------------------------------------#
# Para rodar este programa  deixe no objeto fit.model a sa�da 
# do  ajuste  da  regress�o dupla com  erros normal e em 
# resp o vetor de respostas. Deixe  os dados dispon�veis  
# atrav�s do comando attach(...). Depois  use  
# o comando source(...) no S-Plus ou R para executar o
# programa. A sequ�ncia de comandos � a seguinte:
#
#        > fit.model <- ajuste
#        > attach(dados)
#        > source("diag_norm_dglm_disp")
#
# A sa�da ter� quatro gr�ficos: de pontos de alacanca, 
# de pontos influentes  e  dois de res�duos. Para identificar
# os pontos que  mais  se destacam usar o comando 
# identify(...). Se por exemplo se destacam tr�s pontos no
# plot(fittedfit.model),h,...), ap�s esse comando coloque
#     
#        > identify(fitted(fit.model),h,n=3)
#
# O mesmo pode ser feito nos demais gr�ficos. Nos gr�ficos de 
# res�duos foram colocados os limites ylim=c(a-1,b+1), em que
# a � o menor valor e b o maior valor para o res�duo. 
# Este programa usa a library dglm para estimar os par�metros
# fi da normal que  estar� guardado no objeto fi.
#------------------------------------------------------------#
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
Z=X
library(dglm)
resp <- fit.model$y
mu <- fitted(fit.model)
fi <- fitted(fit.model$dispersion)
fi <- 1/fi
R <- solve(t(Z)%*%Z)
R <- Z%*%R%*%t(Z)
r <- diag(R)
t <- resp*mu - 0.5*(mu*mu + resp*resp) 
t1 = t + 1/(2*fi)
tt = t1/(sqrt(1-r)*(1/(fi*sqrt(2))))
td <- resid(fit.model$dispersion,type="deviance")/sqrt(1-r)
#
par(mfrow=c(2,2))
a <- max(td)
b <- min(td)
di = (r/(1-r))*(tt^2)
plot(fitted(fit.model$dispersion),r,xlab="Dispersao Ajustada", ylab="Medida r", pch=16)
#identify(fitted(fit.model$dispersion), r, n=1)
#
plot(di,xlab="indice", ylab="Distancia de Cook", pch=16)
#identify(di, n=3)
#
plot(fitted(fit.model$dispersion),td,xlab="Valor Ajustado", ylab="Residuo Componente do Desvio",ylim=c(b-1,a+1),pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),tt, n=1)
#
eta <- predict(fit.model$dispersion)
z <- eta + 2*fi*t1  
plot(eta,z,xlab="Preditor Linear", 
ylab="Variavel z", pch=16)
lines(smooth.spline(predict(fit.model$dispersion), z, df=2))
par(mfrow=c(1,1))
#------------------------------------------------------------#

