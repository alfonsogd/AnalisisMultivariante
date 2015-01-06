#### Análisis de varianza de un factor
## 1 objetivos: diferenciar entre grupos de fumadores y no fumadores su intención de pagar impuestos por tabaco
fumadores <- read.csv("~/GitHub/AnalisisMultivariante/fumadores.csv") ### cargar los datos
## 2 condiciones de aplicabilidad
#descriptivos de la variable dependiente
library(plyr)
ddply(fumadores, .(habito), summarise, N = length(impuest), Media = mean(impuest)
      , Desv_Est = sd(impuest))
# test de Levene para la homocedasticidad
library(car)
leveneTest(fumadores$impuest ~ fumadores$habito, center = mean)
# test de normalidad grafico qqplot
qqnorm(fumadores$impuest, main="Deben aumentarse los impuestos", datax=TRUE); qqline(fumadores$impuest, datax=TRUE,col=3)
# normalidad con KSL
library(nortest)
lillie.test(fumadores$impuest)
## 3 Estimacion del modelo: VI : habito, VD : impuest
modelo <- aov(impuest ~ habito, data=fumadores)
summary(modelo) # Muestra los resultados del modelo
model.tables(modelo, "means") # tabla de medias
TukeyHSD(modelo) # Post-hoc Tukey HSD
## 4 Interpretacion
model.tables(modelo) # tabla de efectos
## Bondad del ajuste
resumen <- as.data.frame(summary(modelo)[[1]])
bondad <- resumen[1,2]/(resumen[1,2]+resumen[2,2])
print(bondad)
## 5 VAlidacion: no se estila en CS pues requiere el contraste con otra muestra

