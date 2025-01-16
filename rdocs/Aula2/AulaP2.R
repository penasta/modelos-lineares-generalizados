require(MASS)
require(glmtoolbox)#pacote para gerar graficos de envelope


#*********************** APLICACAO - DADOS EXPECTATIVA DE VIDA ****************#


#**************** Ajuste via MLGs normal, gama e normal inverso*******************#

#Dados de um estudo sobre expectativa de vida em 1969-70
#de 50 estados norte-americanos. Os dados estao disponibilizados em
#https://www.ime.usp.br/˜giapaula/reg3.txt

ev <- scan("rdocs/Aula2/reg3.txt", list(estado="  ", pop=0, percap=0, analf=0, expvida=0, crime=0, estud=0, ndias=0, area=0))
attach(ev)
dens <- pop/area # definindo nova covariável

# Ajuste final com modelo normal linear que tinhamos escolhido:

fit3_N <- lm(expvida ~ crime+estud+ndias)
summary(fit3_N)

#*************Diagnostico************#

fit.model <- fit3_N

#Via scripts Prof. Gilberto A. Paula - Site: https://www.ime.usp.br/~giapaula/textoregressao.htm
source("rdocs/Aula2/envel_norm.R")
source("rdocs/Aula2/diag_norm.R") 


#Usando o pacote glmtoolbox

envelope(fit3_N, rep=100, conf=0.95, type ="external")
# Mesmo grafico usando o residuo studentizado com fit3_N obtido via função lm


#  Ajustando via glm

fit3_N <- glm(expvida ~ crime+estud+ndias) 

envelope(fit3_N, rep=100, conf=0.95, type="quantile")
envelope(fit3_N, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(fit3_N, rep=100, conf=0.95, type="pearson", standardized	=T)


# Digamos que queremos ver os valores dos resíduos usados na função envelope. Basta fazer:

res_fitN <- envelope(fit3_N, rep=100, conf=0.95, type="quantile", plot.it =F)

head(res_fitN)

res_fitN[,4] #estes são os n resíduos quantílicos para o modelo ajustado


#********************* Graficos de diagnostico separadamente usando diag_norm.R******************#

fit.model <- lm(expvida ~ crime+estud+ndias)

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
lms <- summary(fit.model)
s <- lms$sigma
r <- resid(lms)
ts <- r/(s*sqrt(1-h))
di <- (1/p)*(h/(1-h))*(ts^2)
si <- lm.influence(fit.model)$sigma
tsi <- r/(si*sqrt(1-h))
a <- max(tsi)
b <- min(tsi)


#********Alavancagem******#
plot(h,xlab="Índice", ylab="Medida h", pch=16, ylim=c(0,1))
cut <- 2*p/n
abline(cut,0,lty=2)
identify(h, n=3) #Obs. 28, 11 e 2 - O que estas observacoes possuem de diferente?
# Obs. 11 é o estado do Hawaii; faça estado[11] ou identify(h, labels=estado)

#***********Influência*********#
plot(di,xlab="Índice", ylab="Distância de Cook", pch=16)
identify(di, n=1) #Obs. 11

#********* Verificar adequação de independência das respostas*****#
plot(tsi,xlab="Índice", ylab="Resíduo Padronizado",ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
identify(tsi, n=1)#obs. 19

#*************Verificar adequação da homoscedasticidade****#
plot(fitted(fit.model),tsi,xlab="Valor Ajustado", 
ylab="Resíduo Padronizado", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),tsi, n=1) # Obs. 19
par(mfrow=c(1,1))
#------------------------------------------------------------#


#Retirar observações identificadas para avaliar influencia#

# Calculo da mudança relativa
MR <- function(a,b) cat("Mudança relativa = ", (b/a-1)*100, "%","\n")

#exemplos:
MR(10,5)
MR(2,4)

#Pontos destacados sob Fit1: 2,11,19, 28

fit3_N <- lm(expvida ~ crime+estud+ndias); summary(fit3_N) #ajuste completo - todos coeficientes significantes aos níveis usuais
fit3_N2  <- lm(expvida ~ crime+estud+ndias, subset=-c(2)); summary(fit3_N2) #ajuste excluindo obs. 2
fit3_N11  <- lm(expvida ~ crime+estud+ndias, subset=-c(11)); summary(fit3_N11)#ajuste excluindo obs. 11
fit3_N19  <- lm(expvida ~ crime+estud+ndias, subset=-c(19)); summary(fit3_N19)#ajuste excluindo obs. 19
fit3_N28  <- lm(expvida ~ crime+estud+ndias, subset=-c(28)); summary(fit3_N28)#ajuste excluindo obs. 28
fit3_NT  <- lm(expvida ~ crime+estud+ndias, subset=-c(2,11,19,28)); summary(fit3_NT)#ajuste excluindo todas obs. identificadas

#Analisando os resumos dos ajustes, houve mudança inferencial em algum caso? Fixe um nível nominal para suas conclusões.


#***mudança nas estimativas***#
MR(coef(fit3_N), coef(fit3_N2)) # ao retirar obs. 2
MR(coef(fit3_N), coef(fit3_N11)) # ao retirar obs. 11
MR(coef(fit3_N), coef(fit3_N19)) # ao retirar obs. 19
MR(coef(fit3_N), coef(fit3_N28)) # ao retirar obs. 28
MR(coef(fit3_N), coef(fit3_NT)) # ao retirar todas obs. identificadas

#***mudança relativa no p-valores***#
MR(summary(fit3_N)$coefficients[,4], summary(fit3_N2)$coefficients[,4]) #retirando obs. 2
MR(summary(fit3_N)$coefficients[,4], summary(fit3_N11)$coefficients[,4]) #retirando obs. 11
MR(summary(fit3_N)$coefficients[,4], summary(fit3_N19)$coefficients[,4]) #retirando obs. 19
MR(summary(fit3_N)$coefficients[,4], summary(fit3_N28)$coefficients[,4]) #retirando obs. 28
MR(summary(fit3_N)$coefficients[,4], summary(fit3_NT)$coefficients[,4]) #ao retirar todas obs. identificadas

#A mudança relativa nos p-valores é relevantes? Se sim, em que cenário?


#QUAL É A CONCLUSAO? O modelo está adequado?
#E se retirarmos a covariável ndias?

fit4_N <- lm(expvida ~ crime+estud); summary(fit4_N) 
fit.model <- fit4_N
source("rdocs/Aula2/envel_norm.R")
source("rdocs/Aula2/diag_norm.R")

#verifique se os possíveis pontos discrepantes sob fit4_N são influentes!


#Usando o pacote glmtoolbox

envelope(fit3_N, rep=100, conf=0.95, type ="external")#mesmo grafico usando o residuo studentizado com fit3_N obtido via função lm


#Ajustando via glm
fit3_N <- glm(expvida ~ crime+estud+ndias) 

envelope(fit3_N, rep=100, conf=0.95, type="quantile")
envelope(fit3_N, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(fit3_N, rep=100, conf=0.95, type="pearson", standardized	=T)


#Digamos que queremos ver os valores dos resíduos usados na função envelope. Basta fazer:

res_fitN <- envelope(fit3_N, rep=100, conf=0.95, type="quantile", plot.it =F)

res_fitN[,4] #estes são os n resíduos quantílicos para o modelo ajustado



#************* Ajuste final com modelo gama ligação log**************#

fit2_G <- glm(expvida ~ crime+estud+ndias, family=Gamma(link = "log"))
summary(fit2_G)

envelope(fit2_G, rep=100, conf=0.95, type="quantile")
envelope(fit2_G, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(fit2_G, rep=100, conf=0.95, type="pearson", standardized=T)

fit.model <- fit2_G
source("rdocs/Aula2/envel_gama.R")
source("rdocs/Aula2/diag_gama.R") # Falta gráfico resíduos componente do desvio X índices para avaliar independência

#********************* Graficos de diagnostico separadamente usando diag_gama.R******************#

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
library(MASS)
fi <- gamma.shape(fit.model)$alpha
ts <- resid(fit.model,type="pearson")*sqrt(fi/(1-h))
td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
di <- (h/(1-h))*(ts^2)
a <- max(td)
b <- min(td)

plot(fitted(fit.model),h,xlab="Valor Ajustado", ylab="Medida h", pch=16)
identify(fitted(fit.model), h) # Obs.28, 2, 11
#

plot(di,xlab="Índice", ylab="Distância de Cook", pch=16)
identify(di, n=2) #Obs. 11

#
plot(fitted(fit.model),td,xlab="Valor Ajustado", ylab="Resíduo Componente do Desvio",
ylim=c(b-1,a+1),pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
identify(fitted(fit.model),td, n=1) #Obs. 19
#
w <- fit.model$weights
eta <- predict(fit.model)
z <- eta + resid(fit.model, type="pearson")/sqrt(w)
plot(predict(fit.model),z,xlab="Preditor Linear", 
ylab="Variável z", pch=16)


#Pontos destacados: 2,11,19, 28 (mesmos pontos)

fit2_G <- glm(expvida ~ crime+estud+ndias, family=Gamma(link = "log")); summary(fit2_G) #ajuste completo - todos coeficientes significantes aos níveis usuais
fit2_G2  <- glm(expvida ~ crime+estud+ndias, family=Gamma(link = "log"), subset=-c(2)); summary(fit2_G2) #ajuste excluindo obs. 2
fit2_G11  <- glm(expvida ~ crime+estud+ndias, family=Gamma(link = "log"), subset=-c(11)); summary(fit2_G11) #ajuste excluindo obs. 11
fit2_G19  <- glm(expvida ~ crime+estud+ndias, family=Gamma(link = "log"), subset=-c(19)); summary(fit2_G19) #ajuste excluindo obs. 19
fit2_G28  <- glm(expvida ~ crime+estud+ndias, family=Gamma(link = "log"), subset=-c(28)); summary(fit2_G28) #ajuste excluindo obs. 28
fit2_GT  <- glm(expvida ~ crime+estud+ndias, family=Gamma(link = "log"), subset=-c(2,11,19,28)); summary(fit2_GT) #ajuste excluindo todas obs. identificadas 


#Analisando os resumos dos ajustes, houve mudança inferencial em algum caso? Fixe um nível nominal para suas conclusões.


#***mudança nas estimativas***#
MR(coef(fit2_G), coef(fit2_G2)) # ao retirar obs. 2
MR(coef(fit2_G), coef(fit2_G11)) # ao retirar obs. 11
MR(coef(fit2_G), coef(fit2_G19)) # ao retirar obs. 19
MR(coef(fit2_G), coef(fit2_G28)) # ao retirar obs. 28
MR(coef(fit2_G), coef(fit2_GT)) # ao retirar todas obs. identificadas


#Qual é sua conclusão?

#------------------------------------------------------------#


#---------------->Fazer mesmo procedimento com ajuste via normal inversa.




#************************ APLICACAO - AJUSTE POISSON E BINOMIAL NEGATIVA*******************#

# No arquivo tvcabo.txt os dados seguem a seguinte ordem: 

#Para cada área:

# Nass: numero de assinantes (em milhares) de TV a cabo
# Domic: numero de domicilios (em milhares) na área
# Percap: renda per capita (em USD) por domicílio com TV a cabo
# Taxa: taxa de instalacao de TV a cabo (em USD)
# Custo: custo medio mensal de manutencao de TV a cabo (em USD)
# Ncabo: numero de canais a cabo disponiveis na area
# Ntv: numero de canais nao pagos com sinal de boa qualidade disponiveis na area

# Vamos considerar um conjunto de dados sobre a demanda de TV a cabo em 40 áreas metropolitanas
# dos EUA (Ramanathan, 1993).

# O objetivo do estudo é explicar o número de assinantes (Nass) de TV a cabo dadas algumas variáveis 
# explicativas observadas em cada área.

TVCabo <- scan("rdocs/Aula2/tvcabo.txt", list(Nass=0, Domic=0, Percap=0,Taxa=0, Custo=0, Ncabo=0, Ntv=0))
attach(TVCabo)
#head(TVCabo)
#names(TVCabo)

# ** Ajuste via MLG Poisson **#

fit1 <- glm(Nass ~ Domic+Percap+Taxa+Custo+Ncabo+Ntv, family=poisson(link = "log"));summary(fit1)

# ao aumentar 1 dolar da `Taxa`, aumenta o número de assinantes em em exp(0.0386881)% (3,9%)
# ao aumentar 5 dolares da `Taxa`, aumenta o número de assinantes em exp(0.0386881*5)% (21,34%)
# Quanto mais cara a taxa, mais assinantes!?!

1-exp(-0.1223528)
# Ao aumentar em uma unidade o `Custo`, DIMINUI o número de assinantes em 11,51%

stepAIC(fit1) #selecao de covariaveis

#install.packages("AER")
require(AER)
dispersiontest(fit1, alternative ="greater") # Teste de equidispersão (E(Y)=Var(Y)) sob MLG Poisson - Com 5% de significância existem evidencias contra a equidispersao 
# Hipótese nula: variância igual a média
# como o teste rejeita, o modelo Poisson não é adequado para estes dados.

#*** Calculo de Pseudo R2 ***#
Rp <- cor(fit1$y, fit1$fitted.values,)^2; Rp # Pseudo R^2 
Rp2 <- 1 - fit1$deviance/fit1$null; Rp2 # Pseudo R^2 of McFadden(1974)
# Para previsão, pode utilizar R² para avaliar MLGs
# Para interpretar parâmetros, R² nos MLGs não é uma boa medida.

#** Diagnostico **#
fit.model <- fit1
source("rdocs/Aula2/envel_pois.R")
source("rdocs/Aula2/diag_pois.R")

envelope(fit1, rep=100, conf=0.95, type="quantile")
envelope(fit1, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(fit1, rep=100, conf=0.95, type="pearson", standardized=T)

# ** Ajuste via Regressão Binomial Negativa **#

?glm.nb

fit2 <- glm.nb(Nass ~ Domic+Taxa+Custo+Ncabo+Ntv, link = "log");summary(fit2)

stepAIC(fit2) #selecao de covariaveis

fit.model <- fit2
source("rdocs/Aula2/envel_nbin.R")
source("rdocs/Aula2/diag_nbin.R") # Falta resíduo componente desvio X valor ajustado

#Existem pontos influentes? Baseado isto, escolha o modelo final.

#************************ APLICACAO - AJUSTE BINOMIAL*******************#

besouros <- read.table("rdocs/Aula2/besouros.txt",header=TRUE)
attach(besouros)


# Como aplicação de modelos binomiais de dose resposta vamos
# considerar os dados descritos em Bliss (1935), em que besouros
# adultos são submetidos à exposição do composto
# disulfeto de carbono gasoso (CS2) durante cinco horas.

# Os resultados obtidos a partir de 481 besouros expostos segundo
# diferentes doses de CS2 são apresentados a seguir. 
# Um dos objetivos desse estudo é estimar DL100p: dose letal que mata 100p%.

# ldose: logdose de CS2
# tbe: besouros expostos
# mbe: besouros mortos


# Para um MLG binomial, Para entrar com a resposta no ajuste 

# Obs: Primeira coluna deve ser o número de sucesso e a segunda coluna deve ser o número de fracasso

matbe <- cbind(mbe, tbe - mbe)
#Primeira coluna: numero de besouros mortos
#Segunda coluna: numero de besouros nao mortos


#Objetivo: Modelar a proporção de besouros mortos em função da log-dose de CS2

#Ligação logit
ajuste1.besouros <- glm(matbe ~ ldose, family=binomial(link="logit"))
summary(ajuste1.besouros)


# Verificando a qualidade do ajuste do MLG com ligação logit:

envelope(ajuste1.besouros, rep=100, conf=0.95, type="quantile")
envelope(ajuste1.besouros, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(ajuste1.besouros, rep=100, conf=0.95, type="pearson", standardized=T)

#Ligação probit

ajuste2.besouros <- glm(matbe ~ ldose, family=binomial(link="probit"))
summary(ajuste2.besouros)


# Verificando a qualidade do ajuste do MLG com ligação probit:

envelope(ajuste2.besouros, rep=100, conf=0.95, type="quantile")
envelope(ajuste2.besouros, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(ajuste2.besouros, rep=100, conf=0.95, type="pearson", standardized=T)


#Ligação cloglog

ajuste3.besouros <- glm(matbe ~ ldose, family=binomial(link="cloglog"))
summary(ajuste3.besouros)


# Verificando a qualidade do ajuste do MLG com ligação cloglog:

envelope(ajuste3.besouros, rep=100, conf=0.95, type="quantile") #Lembre que o resíduo é aleatorizado
envelope(ajuste3.besouros, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(ajuste3.besouros, rep=100, conf=0.95, type="pearson", standardized=T)

# Qual é o "melhor" MLG ajustado?

# A Dose Letal de ordem p DL100p num MLG binomial é obtida da expressão:

#  g(p) = beta1 + beta2*DL100p, com g(.) sendo a ligação e 0<p<1 a proporção de besouros mortos.

# Em outras palavras, DL100p é o valor estimado da dose para que tenhamos p*100% de besouros mortos


# Assim, a estimativa pontual para a DL100p sob o MLG binomial é 

# DL100p_hat <- (g(p) - beta1hat)/beta2hat.


# Sob a ligação cloglog, a Dose letal estimada para ter 50% dos besouros mortos é obtida de:

p <- 0.5
gp <- log(-log(1-p)) # definição da função de ligação Complemento log-log

beta1hat <- coef(ajuste3.besouros)[[1]]
beta2hat <- coef(ajuste3.besouros)[[2]]

DL100p_hat <- (gp - beta1hat)/beta2hat;  DL100p_hat

#Assim, a dose estimada necessária para ter 50% de besouros mortos é de 1.77875

