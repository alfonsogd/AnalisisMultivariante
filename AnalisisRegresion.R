### ANALISIS DE REGRESION
## 1 Objetivos
# Si el grado de relación de la empresa con los clientes de Hatco, x9, tiene relación con la percepción (x1-x7)
# dependiente: x9
# independientes: x1, x2, x3, x4, x5, x6, x7
hatco<- read.csv("~/GitHub/AnalisisMultivariante/hatco_etiquetado.csv")
#### variables y etiquetador de variables
variable_names <- list("x1"="Rapidez del Servicio", 
                       "x2"="Nivel de precios",
                       "x3"="Flexibilidad de precios",
                       "x4"="Imagen del fabricante",
                       "x5"="Servicio",
                       "x6"="Imagen de los vendedores",
                       "x7"="Calidad del producto",
                       "x8"="Tamaño de la empresa",
                       "x9"="Nivel de utilización de los servicios de HATCO",
                       "x10"="Nivel de satisfacción con HATCO",
                       "x11"= "procedimiento de compra",
                       "x12"="Estructura de decisión",
                       "x13"="Tipo de industria",
                       "x14"="Tipo de situación de compra"
)
vlab <- function(value){return(as.character(variable_names[value]))}
numer_hatco <- hatco[,c(2:8,10)]
## 2 Desarrollo del plan de analisis
# ratio de observaciones / variables independientes alrededor de 15 a 1
## 3 Condicones de aplicabilidad
# Verificar linealidad con relación a x9
# verificar homocedasticidad (no afecta mucho)
# verificar normalidad: x2, x4 y x6 no normales
## 4 Estimacion del modelo y establecimiento del ajuste
# Modelo simple
fit <- lm(x9 ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data=numer_hatco)
fit
summary(fit)
# Stepwise Regression
library(MASS)
fit2 <- lm(x9~x1+x2+x3+x4+x5+x6+x7,data=numer_hatco)
step <- stepAIC(fit2, direction="both")
step$anova # display results
### Verificar si el modelo cumple con normalidad, linealidad y homocedasticidad conjunta
# graficamente
plot(step)
# Durbin watson 