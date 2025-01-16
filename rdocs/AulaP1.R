
#*********************** APLICACAO 1****************#


#**************** Ajuste via MLGs normal, gama e normal inverso*******************#

#Dados de um estudo sobre expectativa de vida em 1969-70
#de 50 estados norte-americanos. Os dados estao disponibilizados em
#https://www.ime.usp.br/˜giapaula/reg3.txt


# Y = expvida (expectativa de vida em anos 1969-70)
# x1 = percap (renda per-capita em 1974)
# x2 = analf (proporção de analfabetos em 1970)
# x3 = crime (taxa de criminalidade por 100000 habitantes em 1976)
# x4 = estud (porcentagem de estudantes que concluíram o segundo grau em 1970)
# x5 = ndias (número de dias do ano com temperatura abaixo de zero graus Celsus na cidade mais importante do estado)
# x6 = area (area do estado em milhas quadradas)
# x7 = dens=pop/área 

ev <- scan("rdocs/reg3.txt", list(estado="  ", pop=0, percap=0, analf=0, expvida=0, crime=0, estud=0, ndias=0, area=0))
attach(ev)
head(ev)
names(ev)

dens <- pop/area # definindo nova covariável

xreg <- cbind(percap,analf,crime,estud,ndias,dens,expvida)
xreg

cor(xreg) # matriz de correlações 


#Graficos de dispersao para avaliar as relacoes
plot(percap,expvida, xlab="percap", ylab="expvida", pch=16)
plot(analf,expvida, xlab="analf", ylab="expvida", pch=16)
plot(crime,expvida, xlab="crime", ylab="expvida", pch=16)
plot(estud,expvida, xlab="estud", ylab="expvida", pch=16)
plot(ndias,expvida, xlab="ndias", ylab="expvida", pch=16)
plot(dens,expvida, xlab="dens", ylab="expvida", pch=16)
plot(log(dens),expvida, xlab="dens", ylab="expvida", pch=16)

# II- Análise Inferencial

# Ajuste inicial com modelo normal linear

?lm
fit1_N <- lm(expvida ~ percap+analf+crime+estud+ndias+log(dens))
summary(fit1_N)

?glm
fit2_N <- glm(expvida ~ percap+analf+crime+estud+ndias+log(dens),
              family = gaussian(link = "identity"))
summary(fit2_N)

#QUAIS SAO AS INTERPRETACOES?

X <- model.matrix(fit2_N)
n <- nrow(X); p <- ncol(X)

sigma2hat <- sum(resid(fit2_N)^2)/(n-p); sigma2hat
phihat <- 1/sigma2hat; phihat #inverter valor!

R2 <- cor(expvida,fitted(fit2_N))^2 ; R2 #R2 no caso normal linear

#Escolha automatica das covariaveis
require(MASS)
stepAIC(fit2_N)

fit3_N <- glm(expvida ~ crime+estud+ndias)
summary(fit3_N)

R2_fit3 <- cor(expvida,fitted(fit3_N))^2; R2_fit3


# Interpretações???

#*************Diagnostico************#

#install.packages("glmtoolbox")
require(glmtoolbox)
?envelope

#Avaliando a suposicao de distribuicao normal da resposta
envelope(fit3_N, rep=100, conf=0.95, type="quantile")
envelope(fit3_N, rep=100, conf=0.95, type="deviance")
envelope(fit3_N, rep=100, conf=0.95, type="pearson")

# Ajuste inicial com MLG Gama ligação log

fit1_G <- glm(expvida ~ percap+analf+crime+estud+ndias+log(dens),
              family=Gamma(link = "log"))
summary(fit1_G)

#Como interpretamos?

#Seleção de covariaveis
stepAIC(fit1_G)

#Avaliando a suposicao de distribuicao gama da resposta
envelope(fit1_G, rep=100, conf=0.95, type="quantile")
envelope(fit1_G, rep=100, conf=0.95, type="deviance")
envelope(fit1_G, rep=100, conf=0.95, type="pearson")

fit2_G <- glm(expvida ~ crime+estud+ndias, family=Gamma(link = "log"))
summary(fit2_G)

envelope(fit2_G, rep=100, conf=0.95, type="quantile")
envelope(fit2_G, rep=100, conf=0.95, type="deviance")
envelope(fit2_G, rep=100, conf=0.95, type="pearson")


# Ajuste inicial com MLG normal inverso ligação log

fit1_NI <- glm(expvida ~ percap+analf+crime+estud+ndias+log(dens),
               family=inverse.gaussian(link = "log"))
summary(fit1_NI)
stepAIC(fit1_NI)

envelope(fit1_NI, rep=100, conf=0.95, type="quantile")
envelope(fit1_NI, rep=50, conf=0.95, type="deviance")
envelope(fit1_NI, rep=50, conf=0.95, type="pearson")

fit2_NI <- glm(expvida ~ crime+estud+ndias, family=inverse.gaussian)
summary(fit2_NI)

require(glmtoolbox)
envelope(fit2_NI, rep=100, conf=0.95, type="quantile")
envelope(fit2_NI, rep=100, conf=0.95, type="deviance")
envelope(fit2_NI, rep=100, conf=0.95, type="pearson")

#*********************** APLICACAO 2********************#

# Dados sobre o perfil dos clientes de uma determinada loja oriundos de 110 áreas de uma cidade. 

#O objetivo principal do estudo é relacionar o número esperado de clientes de cada área (nclientes) com as seguintes variáveis explicativas em cada área:

# x1 = número de domicílios (domic) em milhares,
# x2 = renda média anual (renda) em mil USD,
# x3 = idade média dos domicílios (em anos) (idade),
# x4 = distância ao concorrente mais próximo (em milhas) (dist1),
# x5 = distância à loja (em milhas) (dist2) (1 milha = 1609,344m).


#*********** Ajuste Poisson **********#

store <- read.table("rdocs/store.txt", header=TRUE)
attach(store)

plot(domic, nclientes, pch=16, ylab="Numero de Clientes", xlab="Numero de Domicilios")
plot(renda, nclientes, xlab="Renda", ylab="Clientes", pch=16)
plot(idade, nclientes, xlab="Idade", ylab="Clientes", pch=16)
plot(dist1, nclientes, xlab="Dist1", ylab="Clientes", pch=16)
plot(dist2, nclientes, xlab="Dist2", ylab="Clientes", pch=16)

fit1_p <- glm(nclientes ~ domic + renda + idade + dist1 + dist2,
              family=poisson(link="log"))
summary(fit1_p)

#Seleção
stepAIC(fit1_p) 

# Quais são as interpretações?

#Por exemplo, o aumento de uma milha na distância ao concorrente mais próximo conduz a variação de exp(1.684e-01) = 1.18341.
#Assim, o aumento de uma milha na distância ao concorrente mais próximo conduz um aumento de 18,3% no número médio de clientes.

#Diagnostico 
envelope(fit1_p, rep=100, conf=0.95, type="quantile")
envelope(fit1_p, rep=100, conf=0.95, type="deviance")
envelope(fit1_p, rep=100, conf=0.95, type="pearson")

#Como ficaria o ajuste normal linear a estes dados?

#*********************** APLICACAO 3********************#

# Dados sobre preferência de automóveis (1: americano, 0: japonês) de uma amostra aleatória de 263 consumidores.
#A probabilidade de preferência por carro americano será relacionada com as seguintes variáveis explicativas
# do comprador(a):
#idade (em anos);
#gênero (0: masculino; 1: feminino);
#estado civil (0:casado(a), 1:solteiro(a)

prefauto <- read.table("rdocs/prefauto.txt", header=TRUE)
attach(prefauto)

#Faça uma análise descritiva

genero <- as.factor(genero); ecivil <- as.factor(ecivil)

fit1_B <- glm(pref ~ idade + genero + ecivil, family=binomial(link = "logit")) #modelo logístico
summary(fit1_B)

stepAIC(fit1_B) 

2*pnorm(abs(0.03032/0.01326), lower.tail=F)

fit2_B = glm(pref ~ idade + ecivil, family=binomial(link = "logit"))
summary(fit2_B)

pchisq(349.83, df= 263 - 3, lower.tail=F)

envelope(fit2_B, rep=50, conf=0.95, type="quantile")
envelope(fit2_B, rep=50, conf=0.95, type="deviance")
envelope(fit2_B, rep=50, conf=0.95, type="pearson")

1-exp(-0.51787)

#Interpretações?

# Por exemplo, a chance de um indivíduo preferir carro americano aumenta 28,29% se aumentarmos
# 5 anos de idade deste, desde que exp(5*0.04982)=1.28287

# A chance de um indivíduo solteiro preferir carro americano é 40,4% menor do que a chance de um indivíduo
#casado preferir carro americano desde que # 1-exp(-0.51787)= 0.4042118