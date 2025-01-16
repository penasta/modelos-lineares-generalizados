#******************* AJUSTES MLG DUPLO e GAMLSS*******************#


#install.packages("dglm")
require(dglm) #pacote para ajuste de MLGs duplos

#install.packages("car")
require(car)

#install.packages("gamlss")
require(gamlss)

#********************* APLICACAO 1 *********************#

# Para uma amostra de 64 ovelhas merino castradas (machos) em pastagem,
# os requisitos energéticos diários e o peso foram registrados.

require(GLMsData)
data(sheep); attach(sheep)
?sheep

scatterplot(Energy~Weight, regLine = T, smooth=F, boxplots=F, pch=16, ylab="energia diária necessária",
 xlab="peso da ovelha",cex=1.2,cex.lab=1.5, cex.axis=1.5)
identify(Weight, Energy) #ovelha 11


# ----->  O objetivo é explicar a energia diária necessária em função do peso

#**** Ajuste inicial com modelo normal linear homoscedastico *****#

# Ajuste via lm

fitN1 <- lm(Energy ~ Weight, family=gaussian(link = "identity"));summary(fitN1)
fit.model <- fitN1
source("envel_norm.R")
source("diag_norm.R")


#**** Ajuste com modelo normal linear heteroscedastico *****#

fitN2 <- dglm(Energy ~ Weight, ~Weight, family=gaussian(link = "identity"));summary(fitN2)
fit.model <- fitN2

# Graficos de envelope baseado no residuo desvio
source("envel_norm_dglm.R") # Avalia o ajuste do submodelo da media
source("envel_norm_dglm_disp.R") # Avalia o ajuste do submodelo da precisao

#Graficos de diagnostico
source("diag_norm_dglm.R")  # Diagnostico do ajuste do submodelo da media
source("diag_norm_dglm_disp.R")  # Diagnostico do ajuste do submodelo da precisao/dispersao


# O ajuste via MLG duplo normal produz um melhor ajuste aos dados. Entretanto, ainda não é um ajuste ideal. 


# Ajuste via gamlss

fitN1gam <- gamlss(Energy ~ Weight, sigma.formula = ~ Weight, 
family = NO2(mu.link = "identity", sigma.link = "log"))
summary(fitN1gam) 
wp(fitN1gam) #wormplot baseado nos residuos quantilicos
plot(fitN1gam)



#*********** Ajuste via MLG gama - Modelando apenas media *************#

fitG1 <- glm(Energy ~ Weight, family=Gamma(link = "identity"))
summary(fitG1)
fit.model <- fitG1
source("envel_gama_lig_id.R") 
source("diag_gama.R")

# O ajuste com um MLG gama com ligação identidade produz um ajuste muito parecido ao ajuste duplo normal.


# Ajuste via gamlss

fitG1gam <- gamlss(Energy ~ Weight, sigma.formula =  Weight ~ 1,
family = GA(mu.link = "identity", sigma.link = "log"))
summary(fitG1gam) 
wp(fitG1gam)
plot(fitG1gam)

#*********** Ajuste via MLG gama - Modelando media e variancia *************#

fitG2 <- dglm(Energy ~ Weight, ~ Weight, family=Gamma(link = "identity"))
summary(fitG2)
fit.model <- fitG2
source("envel_gama_dglm_lig_id.R") 
source("envel_gama_dglm_disp_lig_id.R")
source("diag_gama_dglm.R")


# O ajuste de MLG duplo gama parece não ser muito diferente daquele com precisao constante 


# Ajuste via gamlss

?gamlss

fitG2gam <- gamlss(Energy ~ Weight, sigma.formula = ~Weight, family = GA(mu.link = "identity",sigma.link = "log"))
summary(fitG2gam) 
wp(fitG2gam)
plot(fitG2gam)


# A função fitDist identifica a "melhor" distribuicao de probabilidades ajustada marginal
# o argumento type especifica o suporte das distribuições consideradas para o ajuste aos dados

?fitDist
f1 <- fitDist(Energy, type="realplus") 
f1$fits

#Distribuição marginal sugerida é a IGAMMA - gama inversa

# Ajuste via gamlss
fitIGgam <- gamlss(Energy ~ Weight, sigma.formula = ~Weight, family = IGAMMA(mu.link = "log",
sigma.link = "log"))
summary(fitIGgam) 
wp(fitIGgam)
plot(fitIGgam)



# realAll:  Todas as distribuições continuas do gamlss com suporte (-infty, infty) e (0, infty)

# realline: Todas as distribuições continuas do gamlss com suporte (-infty, infty)

# realplus: Todas as distribuições continuas do gamlss com suporte (0, infty)

# real0to1: Todas as distribuições continuas do gamlss com suporte (0, 1), [0,1), (0,1] e [0,1]

# counts: Todas as distribuições do gamlss com suporte para dados de contagem 

# binom: Todas as distribuições do gamlss para dados do tipo binomial



# Ajuste via gamlss
fitBCCGgam <- gamlss(Energy ~ Weight, sigma.formula = ~Weight, family = BCCG(mu.link = "log",
sigma.link = "log"))
summary(fitIGgam) 
wp(fitIGgam)
plot(fitIGgam)


#********************* APLICACAO 2 *********************#

#Os dados do arquivo UN11 contêm diversas variáveis, incluindo
# ppgdp: o produto nacional bruto por pessoa em dólares americanos,
# fertility: o número de filhos por mulher;
# lifeExpF: Expectativa de vida para mulheres em anos

# Os dados são para 199 localidades, principalmente países membros da ONU,
#mas também outras áreas, como Hong Kong, que não são países independentes.
#Referência - WEISBERG, S.. Applied Linear Regression. John Wiley & Sons, Inc., 2014.



#install.packages("alr4"); 
#?UN11

require(alr4); data(UN11); attach(UN11)

scatterplot(fertility~log(ppgdp), regLine = T, smooth=F, boxplots=F, pch=16, xlab="produto nacional bruto por pessoa",
 ylab="taxa de fertilidade",cex=1.2,cex.lab=1.5, cex.axis=1.5)

scatterplot(fertility~lifeExpF, regLine = T, smooth=F, boxplots=F, pch=16, xlab="expectativa de vida feminina",
 ylab="Taxa de fertilidade",cex=1.2,cex.lab=1.5, cex.axis=1.5)

# Ajuste via lm

fitN1 <- lm(fertility~lifeExpF+log(ppgdp));summary(fitN1)
fit.model <- fitN1
source("envel_norm.R")
source("diag_norm.R")

#**** Ajuste com modelo normal linear heteroscedastico *****#

fitN2 <- dglm(fertility~lifeExpF+log(ppgdp), ~lifeExpF+log(ppgdp), family=gaussian(link = "identity"));summary(fitN2)
fit.model <- fitN2

source("envel_norm_dglm.R") # Avalia o ajuste do submodelo da media
source("envel_norm_dglm_disp.R") # Avalia o ajuste do submodelo da precisao

source("diag_norm_dglm.R")  # Diagnostico do ajuste do submodelo da media
source("diag_norm_dglm_disp.R")  # Diagnostico do ajuste do submodelo da precisao/dispersao



#----------> Via gamlss

fitN1gam <- gamlss(fertility~lifeExpF+log(ppgdp), sigma.formula = ~ lifeExpF+log(ppgdp), 
family = NO2(mu.link = "identity", sigma.link = "log"))
summary(fitN1gam) 
wp(fitN1gam) #Forma de U - indica necessidade de uma distribuicao com assimetria à direita
plot(fitN1gam)


f1 <- fitDist(fertility, type="realplus") 
f1$fits

#Distribuição marginal sugerida é a BCPE - Box-Cox power exponential distribution

# Ajuste via gamlss

fitBCPEgam <- gamlss(fertility~lifeExpF+log(ppgdp), sigma.formula = ~lifeExpF+log(ppgdp),
nu.formula = ~1, tau.formula = ~1, family = BCPE(mu.link = "log",
sigma.link = "log", nu.link="identity", tau.link ="log"))
summary(fitBCPEgam) 
wp(fitBCPEgam)
plot(fitBCPEgam)



#********************* APLICACAO 3 *********************#

# Um estudo com 654 jovens investigou as relações entre a capacidade pulmonar (medida pelo volume expiratório forçado,
 ou fev, em litros) e o status de tabagismo (Smoke), idade, altura e gênero

data(lungcap); attach(lungcap)
?lungcap
head(lungcap)

Smoke <- as.factor(Smoke)


scatterplot(FEV~Age, regLine = T, smooth=F, boxplots=F, pch=16, xlab="idade",
 ylab="capacidade pulmonar",cex=1.2,cex.lab=1.5, cex.axis=1.5)

scatterplot(FEV~Ht, regLine = T, smooth=F, boxplots=F, pch=16, xlab="altura",
 ylab="capacidade pulmonar",cex=1.2,cex.lab=1.5, cex.axis=1.5)

plot(Smoke, FEV)
plot(Gender, FEV)


# Ajuste via lm

fitN1 <- lm(FEV~Ht+Age+Smoke+Gender);summary(fitN1)
fit.model <- fitN1
source("envel_norm.R")
source("diag_norm.R")

#**** Ajuste com modelo normal linear heteroscedastico *****#

fitN2 <- dglm(FEV~Ht+Age+Smoke+Gender, ~Ht+Age+Smoke+Gender, family=gaussian(link = "identity"));summary(fitN2)
fit.model <- fitN2

source("envel_norm_dglm.R") # Avalia o ajuste do submodelo da media
source("envel_norm_dglm_disp.R") # Avalia o ajuste do submodelo da precisao

source("diag_norm_dglm.R")  # Diagnostico do ajuste do submodelo da media
source("diag_norm_dglm_disp.R")  # Diagnostico do ajuste do submodelo da precisao/dispersao



#----------> Via gamlss

f1 <- fitDist(FEV, type="realplus") 
f1$fits

#Distribuição marginal sugerida é a BCPE - Box-Cox power exponential distribution

# Ajuste via gamlss

fitGAgam <- gamlss(FEV~Ht+Age+Smoke+Gender, sigma.formula = ~Ht+Age+Smoke+Gender, family = GA(mu.link = "identity",
sigma.link = "log"))
summary(fitGAgam) 
wp(fitGAgam)
plot(fitGAgam)


fitGIGgam <- gamlss(FEV~Ht+Age+Smoke+Gender, sigma.formula = ~Ht+Age+Smoke+Gender,
nu.formula = ~1,  family = GIG(mu.link = "identity",
sigma.link = "log" ))
summary(fitGIGgam) 
wp(fitGIGgam)
plot(fitGIGgam)


fitBCPEgam <- gamlss(FEV~Ht+Age+Smoke+Gender, sigma.formula = ~Ht+Age+Smoke+Gender,
nu.formula = ~1,  family = BCPE(mu.link = "identity",
sigma.link = "log" ))
summary(fitBCPEgam) 
wp(fitBCPEgam)
plot(fitBCPEgam)

