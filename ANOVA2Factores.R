#### Análisis de varianza de dos factores
## 1 objetivos: diferenciar entre grupos su acuerdo con el cobro de impuestos con base en dos factores: habito y sexo
fumadores <- read.csv("~/GitHub/AnalisisMultivariante/fumadores.csv") ### cargar los datos
## 2 condiciones de aplicabilidad
#descriptivos de la variable dependiente
library(plyr)
ddply(fumadores, .(habito,sexo), summarise, N=length(impuest), Media=mean(impuest), Desv_Est = sd(impuest))
# test de Levene para la homocedasticidad
library(car)
leveneTest(fumadores$impuest ~ fumadores$habito*fumadores$sexo, center = mean)
# test de normalidad grafico qqplot
qqnorm(fumadores$impuest, main="Deben aumentarse los impuestos", datax=TRUE); qqline(fumadores$impuest, datax=TRUE,col=3)
# normalidad con KSL
library(nortest)
lillie.test(fumadores$impuest)
## 3 Estimacion del modelo: VI : habito, VD : impuest
modelo2f <- aov(impuest ~ habito*sexo, data=fumadores)
summary(modelo2f) # Muestra los resultados del modelo
model.tables(modelo2f, "means") # tabla de medias
TukeyHSD(modelo2f) # Post-hoc Tukey HSD
## 4 Interpretacion
model.tables(modelo2f) # tabla de efectos
## Bondad del ajuste
resumen <- as.data.frame(summary(modelo2f)[[1]])
bondad <- (resumen[1,2]+resumen[2,2]+resumen[3,2])/(resumen[1,2]+resumen[2,2]+resumen[3,2]+resumen[4,2])
print(bondad)
## 5 VAlidacion: no se estila en CS pues requiere el contraste con otra muestra
