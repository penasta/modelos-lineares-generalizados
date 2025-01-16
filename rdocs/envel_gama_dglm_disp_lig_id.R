#------------------------------------------------------------#
# Para rodar este programa deixe no objeto fit.model a sa�da      
# do ajuste da regress�o gama duplo com liga��es log.  Deixe     
# tamb�m os dados dispon�veis atrav�s do comando attach(...).    
# Depois use o comando source(...) no R ou S-Plus para
# executar o programa. A sequ�ncia de comandos � a seguinte:               
#
#       fit.model <- ajuste
#       attach(dados)
#       source("envel_gama_dglm_disp")                                      
#
# A sa�da ser� o gr�fico de envelope para o res�duo componente 
# do desvio padronizado para a dispers�o. Para colocar
# um t�tulo no gr�fico use o comando title("..."). Para
# usar  outras  liga��es para a m�dia mudar
# no programa abaixo o termo family=Gamma(link=log) 
# para family=Gamma no caso de liga��o  rec�proca ou 
# por family= Gamma(link=identity) no caso de
# liga��o identidade.
#------------------------------------------------------------#
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
Z=X
library(dglm)
mu <- fitted(fit.model)
fi <- fitted(fit.model$dispersion)
fi <- 1/fi
p = fi*(fi*trigamma(fi) - 1)
P <- diag(p)
R <- solve(t(Z)%*%P%*%Z)
R <- sqrt(P)%*%Z%*%R%*%t(Z)%*%sqrt(P)
r <- diag(R)
td <- resid(fit.model$dispersion,type="deviance")/sqrt(1-r)
#
e <- matrix(0,n,100)
#
for(i in 1:100){
resp1 <- rgamma(n,fi)
resp1 <- (fitted(fit.model)/fi)*resp1
fit <- dglm(resp1 ~ X, ~ Z, family=Gamma(link=identity))
mu <- fitted(fit)
fi1 <- fitted(fit$dispersion)
fi1 <- 1/fi1
p = fi1*(fi1*trigamma(fi1) - 1)
P <- diag(p)
R <- solve(t(Z)%*%P%*%Z)
R <- sqrt(P)%*%Z%*%R%*%t(Z)%*%sqrt(P)
r <- diag(R)
e[,i] <- sort(resid(fit$dispersion,type="deviance")/sqrt(1-r))}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:n){
  eo <- sort(e[i,])
e1[i] <- (eo[2]+eo[3])/2
e2[i] <- (eo[97]+eo[98])/2}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
par(pty="s")
qqnorm(td, xlab="Percentil da N(0,1)",
ylab="Residuo Componente do Desvio", ylim=faixa, pch=16, 
main="")
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")
#------------------------------------------------------------#                      
