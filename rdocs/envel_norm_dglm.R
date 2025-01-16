#-----------------------------------------------------------#
# Para  rodar este programa deixe no objeto fit.model a sa�da 
# do ajuste da regress�o do modelo normal 
# linear heterosced�stica. Deixe tamb�m os dados dispon�veis
# atrav�s do comando attach(...). Depois use o comando
# source(...) no R ou S-plus para executar o 
# programa. A sequ�ncia de comandos � a seguinte:
#
#       fit.model <- ajuste
#       attach(dados)
#       source("envel_norm_dglm")
#
# A sa�da ser� o gr�fico de envelope para o res�duo
# padronizado. Para colocar  um  t�tulo no gr�fico ap�s a
# sa�da use o comando title("...").
#------------------------------------------------------------#
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
Z=X
require(dglm)
mu <- fitted(fit.model)
fi <- fitted(fit.model$dispersion)
fi1 <- 1/fi
Fi = diag(fi1)
H <- sqrt(Fi)%*%X%*%solve(t(X)%*%Fi%*%X)%*%t(X)%*%sqrt(Fi)
h <- diag(H)
r <- resid(fit.model)
td <- (sqrt(fi1)*r)/sqrt(1-h)
#
e <- matrix(0,n,100)
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:100){
resp1 <- rnorm(n,0,1)
resp1 <- mu + sqrt(fi)*resp1
require(dglm)
fit <- dglm(resp1 ~ X, ~ Z, family=gaussian)
fi2 <- fitted(fit$dispersion)
fi3 <- 1/fi2
Fi = diag(fi3)
H <- sqrt(Fi)%*%X%*%solve(t(X)%*%Fi%*%X)%*%t(X)%*%sqrt(Fi)
h <- diag(H)
r <- resid(fit)
e[,i] <- sort((sqrt(fi3)*r)/sqrt(1-h)) 
}
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
ylab="Residuo Studentizado", ylim=faixa, pch=16, main="")
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")
#-----------------------------------------------------------#
