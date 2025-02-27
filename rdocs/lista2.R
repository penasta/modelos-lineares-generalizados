# Pacotes ----
if(!require(pacman))install.packages("pacman")
p_load(MASS,dglm,car,tidyverse,gamlss,readr,janitor,glmtoolbox,
       skimr,corrplot,knitr,moments)

# Dados ----
df <- read_csv("dados/Refrigerator.csv")
df = clean_names(df)
df = df %>% select(price,rsize,fsize,ecost,shelves,features)
attach(df)

# Análise exploratória (q1a) ----

df %>% 
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~key, scales = 'free_x') +
  labs(x = '', y = '', title = 'Histogramas das Variáveis') +
  theme_minimal()

df %>% 
  gather(key = "variable", value = "value") %>%
  ggplot(aes(y = value,x=1)) +
  geom_violin(fill="lightblue",alpha = .5) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~variable, scales = 'free_y') +
  labs(x = '', y = '', title = 'Boxplot das Variáveis') +
  theme_minimal()

table = skim(df)[,c(2,5:12)]
colnames(table) = c("Variável","Média","D.P.","Min.","q25","Mediana","q75","Max.","Hist.")
kable(table)

min = tablemin = min(price)
q1 = quantile(price,.25)
mediana = median(price)
media = round(mean(price),2)
q3 = quantile(price, .75)
max = max(price)
assimetria = round(skewness(price),2)
curtose = round(kurtosis(price),2)
medidas = c(min,q1,mediana,media,q3,max,assimetria,curtose)
names(medidas) = c("Mín.","1º Quartil","Mediana","Média","3º Quartil","Máx.","Assimetria","Curtose")
kable(t(medidas))

corrplot(cor(data.frame(price, rsize, fsize,ecost,shelves,features)),
         method="circle",
         type = "lower",
         addCoef.col = "grey")

# Modelagem ----

# fit1_normal_gamlss <- gamlss(price ~ .,
#                              data=df,
#                      family = NO2(mu.link = "identity",sigma.link = 'identity'))
# summary(fit1_normal_gamlss) 
# wp(fit1_normal_gamlss)
# plot(fit1_normal_gamlss)

fit1_normal = lm(price ~.,
                 data=df
                 )
summary(fit1_normal)
stepAIC(fit1_normal)

# Notamos que todas as covariáveis são significativas e o R² ajustado é bom.
# Entretando, o coeficiente de ecost não faz sentido, visto que na análise exploratória
# foi observada correlação linear positiva entre price e ecost, e o ajuste indica que o coeficiente 
# é negativo. Isso possivelmente é causado pela alta correlação entre ecost e fsize, indicando
# problemas de multicolinearidade neste modelo.


######

# Escolha via AUC
f1 <- fitDist(price, type="realplus") 
f1$fits


