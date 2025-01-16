#------------------------------------------------------------#
# Para rodar este programa deixe no objeto fit.model a saida      
# do ajuste da regressao gama duplo com ligacoes log.  Deixe     
# tambem os dados disponiveis atraves do comando attach(...).    
# Depois use o comando source(...) no R ou S-Plus para
# executar o programa. A sequencia de comandos e a seguinte:               
#
#       fit.model <- ajuste
#       attach(dados)
#       source("envel_gama_dglm")                                      
#
# A saida ser o grafico de envelope para o residuo componente 
# do desvio padronizado para a media. Para colocar um titulo
# no grafico use o comando title("..."). Para  usar  
# outras  ligacoes para a media mudar
# no programa abaixo o termo family=Gamma(link=log) para
# family=Gamma no caso de ligacao  reciproca ou por 
# family= Gamma(link=identity) no caso de ligacao identidade.
#------------------------------------------------------------#
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
Z=X
library(dglm)
fi <- fitted(fit.model$dispersion)
fi <- 1/fi
mu = fitted(fit.model)
w = fi/(mu^2)
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
#
e <- matrix(0,n,100)
#
for(i in 1:100){
resp <- rgamma(n,fi)
resp <- (fitted(fit.model)/fi)*resp
fit <- dglm(resp ~ X, ~ Z, family=Gamma(link=identity))
fi <- fitted(fit$dispersion)
fi <- 1/fi
mu = fitted(fit)
w = fi/(mu^2)
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
e[,i] <- sort(resid(fit,type="deviance")*sqrt(fi/(1-h)))}
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
par(new=T)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=T)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=T)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")
#------------------------------------------------------------#                      
