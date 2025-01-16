if(!require(pacman))install.packages("pacman")
p_load(GLMsData,tidyverse,corrplot,moments,janitor,glmtoolbox)

# Lista 1
# Questão 1 ----

data(nambeware)

# Variáveis:
# Type: tipo do item (Bowl, CassDish, Dish, Plate, Tray);
# Diam: diâmetro do produto em polegadas;
# Time: tempo total de lixamento e polimento em minutos;
# Price: preco em dólares do item.|-> Variável resposta!
df = clean_names(nambeware)
colnames(df) = c("tipo","diametro","tempo","preco")
attach(df);rm(nambeware)
cor1 = paste0("Correlação: ", round(cor(preco,diametro),3))
cor2 = paste0("Correlação: ", round(cor(preco,tempo),3))

par(mfrow= c(1,2))
plot(preco ~ diametro, main = "Relação entre Diâmetro e preco", sub = cor1)
plot(preco ~ tempo, main = "Relação entre Tempo e preco", sub = cor2)
par(mfrow= c(1,1))
# Correlação linear forte entre os dados

par(mfrow= c(3,1))
hist(diametro, main="Distribuição do Diâmetro", xlab="Diâmetro",ylab="Frequência", col="blue")
hist(tempo, main="Distribuição do Tempo", xlab="Tempo",ylab="Frequência", col="green")
hist(preco, main="Distribuição do preco", xlab="preco",ylab="Frequência", col="red")
par(mfrow= c(1,1))

# Distribuições assimétricas, alongadas à direita
round(skewness(preco),3) # O coeficiente de assimetria explicita a afirmação anterior.

round(kurtosis(preco),3) # Curtose da variável preco 4,343419; isto é, 1,343419 de excesso de curtose, caracterizando a cauda pesad e forma leptocúrtica.

par(mfrow= c(2,2))
boxplot(diametro, main="Boxplot do Diâmetro", ylab="Diâmetro", col="blue",horizontal=T)
boxplot(tempo, main="Boxplot do Tempo", ylab="Tempo", col="green",horizontal=T)
boxplot(preco, main="Boxplot do preco", ylab="preco", col="red",horizontal=T)
boxplot(preco ~ tipo, main="Boxplot do preco pelo Tipo", ylab="preco", col="yellow",horizontal=T)
par(mfrow= c(1,1))

ggplot(data.frame(tipo, preco), aes(x=tipo, y=preco)) + 
  geom_violin(fill="lightblue") + 
  geom_boxplot(width=0.1, fill="white")

corrplot(cor(data.frame(diametro, tempo, preco)), method="circle")

# Uma alta correlação entre as covariáveis numéricas com a variável resposta (e entre si) indicam que 
# pode ser útil realizar a modelagem da variável preco em função das covariáveis dos dados. A praxis manda
# iniciar a tentativa com um modelo simples e parcimonioso: a regressão linear gaussiana; ainda que pelo diagnóstico
# preliminar a forma da distribuição empírica da variável preco indica que teremos possíveis problemas com esta modelagem.

## Modelo 1: Gaussiana com link identidade ----

fit1 = glm(preco ~ diametro + tempo + tipo,family = gaussian(link = "identity"))
smr = summary(fit1) # Categoria de referência do tipo: Bowl
round(smr[["coefficients"]],4)

R2 <- cor(preco,fitted(fit1))^2 ; R2 #R2 no caso normal linear
adjR2(fit1) # R2 ajustado

backward <- step(fit1,direction = 'both')
model = summary(backward)
round(model[["coefficients"]],4)

sw = shapiro.test(fit1$residuals) # Normalidade REJEITADA a 5%
qqnorm(fit1$residuals,
       main = paste0("P-valor do teste de Shapiro-Wilk: ",round(sw$p.value,3)),
       xlab = "",
       ylab = "")
qqline(fit1$residuals)

envelope(fit1, rep=100, conf=0.95, type="quantile")

fit.model = lm(preco ~ diametro + tempo + tipo)
source("rdocs/envel_norm.R")
source("rdocs/diag_norm.R") 

## Modelo 2: Gaussiana com link log ----

fit2 = glm(preco ~ diametro + tempo + tipo,family = gaussian(link = "log"))
smr = summary(fit2) # Categoria de referência do tipo: Bowl
round(smr[["coefficients"]],4)

R2 <- cor(preco,fitted(fit2))^2 ; R2 #R2 no caso normal linear
adjR2(fit2) # R2 ajustado

backward <- step(fit2,direction = 'both')
model = summary(backward)
round(model[["coefficients"]],4)

sw = shapiro.test(fit2$residuals) # Normalidade REJEITADA a 5%
qqnorm(fit2$residuals,
       main = paste0("P-valor do teste de Shapiro-Wilk: ",round(sw$p.value,3)),
       xlab = "",
       ylab = "")
qqline(fit2$residuals)

envelope(fit2, rep=100, conf=0.95, type="quantile")

## Modelo 3: gama com link identidade ----
fit3 = glm(preco ~ diametro + tempo + tipo,family = Gamma(link = "identity"))
smr = summary(fit3) # Categoria de referência do tipo: Bowl
round(smr[["coefficients"]],4)

backward <- step(fit3,direction = 'both')
model = summary(backward)
round(model[["coefficients"]],4)

envelope(fit3, rep=100, conf=0.95, type="quantile")
fit.model = fit3
source("rdocs/envel_gama.R")
source("rdocs/diag_gama.R") 

## Modelo 4: gama com link inversa ----
fit4 = glm(preco ~ diametro + tempo + tipo,family = Gamma(link = "inverse"))
smr = summary(fit4) # Categoria de referência do tipo: Bowl
round(smr[["coefficients"]],4)

backward <- step(fit4,direction = 'both')
model = summary(backward)
round(model[["coefficients"]],4)

envelope(fit4, rep=100, conf=0.95, type="quantile")
fit.model = fit4
source("rdocs/envel_gama.R")
source("rdocs/diag_gama.R") 

## Modelo 5: Normal inversa com link canônico ----
fit5 = glm(preco ~ diametro + tempo + tipo,
           family = inverse.gaussian(link = "1/mu^2"),
           start = coef(fit1))
smr = summary(fit5) # Categoria de referência do tipo: Bowl
round(smr[["coefficients"]],4)

backward <- step(fit5,direction = 'both')
model = summary(backward)
round(model[["coefficients"]],4)

envelope(fit5, rep=100, conf=0.95, type="quantile")

## Modelo 6: Normal inversa com link identidade ----
fit6 = glm(preco ~ diametro + tempo + tipo,
           family = inverse.gaussian(link = "identity"))
smr = summary(fit6) # Categoria de referência do tipo: Bowl
round(smr[["coefficients"]],4)

backward <- step(fit6,direction = 'both')
model = summary(backward)
round(model[["coefficients"]],4)

envelope(fit6, rep=100, conf=0.95, type="quantile")

## Modelo 7: Normal inversa com link log ----
fit7 = glm(preco ~ diametro + tempo + tipo,
           family = inverse.gaussian(link = "log"))
smr = summary(fit7) # Categoria de referência do tipo: Bowl
round(smr[["coefficients"]],4)

backward <- step(fit7,direction = 'both')
model = summary(backward)
round(model[["coefficients"]],4)

envelope(fit7, rep=100, conf=0.95, type="quantile")

# Continuar daqui