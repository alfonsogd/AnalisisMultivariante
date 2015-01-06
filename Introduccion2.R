### Cargar la base de datos HATCO
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
### PROBAR Normalidad
### grafico qq
qqnorm(hatco$x1, main=vlab("x1")); qqline(hatco$x1, col=2)
qqnorm(hatco$x2, main=vlab("x2")); qqline(hatco$x2, col=2)
qqnorm(hatco$x3, main=vlab("x3")); qqline(hatco$x3, col=2)
qqnorm(hatco$x4, main=vlab("x4")); qqline(hatco$x4, col=2)
qqnorm(hatco$x5, main=vlab("x5")); qqline(hatco$x5, col=2)
qqnorm(hatco$x6, main=vlab("x6")); qqline(hatco$x6, col=2)
qqnorm(hatco$x7, main=vlab("x7")); qqline(hatco$x7, col=2)
qqnorm(hatco$x9, main=vlab("x9")); qqline(hatco$x9, col=2)
qqnorm(hatco$x10, main=vlab("x10")); qqline(hatco$x10, col=2)
#### KSL test
library(nortest)
vlab("x1"); lillie.test(hatco$x1)
vlab("x2"); lillie.test(hatco$x2)
vlab("x3"); lillie.test(hatco$x3)
vlab("x4"); lillie.test(hatco$x4)
vlab("x5"); lillie.test(hatco$x5)
vlab("x6"); lillie.test(hatco$x6)
vlab("x7"); lillie.test(hatco$x7)
vlab("x9"); lillie.test(hatco$x9)
vlab("x10"); lillie.test(hatco$x10)
######  Pobar Homocedasticidad
#### visual mediante boxplot
par(mar=c(4,4,3,2))
plot(hatco$x1 ~ hatco$x8, xlab=vlab("x8"), ylab=vlab("x1"))
plot(hatco$x2 ~ hatco$x8, xlab=vlab("x8"), ylab=vlab("x2"))
plot(hatco$x3 ~ hatco$x8, xlab=vlab("x8"), ylab=vlab("x3"))
plot(hatco$x4 ~ hatco$x8, xlab=vlab("x8"), ylab=vlab("x4"))
plot(hatco$x5 ~ hatco$x8, xlab=vlab("x8"), ylab=vlab("x5"))
plot(hatco$x6 ~ hatco$x8, xlab=vlab("x8"), ylab=vlab("x6"))
plot(hatco$x7 ~ hatco$x8, xlab=vlab("x8"), ylab=vlab("x7"))
plot(hatco$x9 ~ hatco$x8, xlab=vlab("x8"), ylab=vlab("x9"))
plot(hatco$x10 ~ hatco$x8, xlab=vlab("x8"), ylab=vlab("x10"))
### Test de Levene
library(car)
leveneTest(hatco$x1 ~ hatco$x8, center = mean)
leveneTest(hatco$x1 ~ hatco$x8, center = median) # con la mediana
leveneTest(hatco$x1 ~ hatco$x8, center = mean, trim = 0.1) # con media recortada
leveneTest(hatco$x2 ~ hatco$x8, center = mean)
leveneTest(hatco$x3 ~ hatco$x8, center = mean)
leveneTest(hatco$x4 ~ hatco$x8, center = mean)
leveneTest(hatco$x5 ~ hatco$x8, center = mean)
leveneTest(hatco$x6 ~ hatco$x8, center = mean)
leveneTest(hatco$x7 ~ hatco$x8, center = mean)
leveneTest(hatco$x9 ~ hatco$x8, center = mean)
leveneTest(hatco$x10 ~ hatco$x8, center = mean)
#### LINEALIDAD
### Grafico scatterplot
library(car)
spm(~ x1 + x4 + x6, data=hatco, var.labels=c(vlab("x1"),vlab("x4"),vlab("x6"))) ## solo unas cuantas
spm(~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x9 + x10, data=hatco, 
    var.labels=c(vlab("x1"),vlab("x2"),vlab("x3"), vlab("x4"), 
                 vlab("x5"), vlab("x6"), vlab("x7"), vlab("x9"), vlab("x10"))) ## todas las variables
### Tabla de correlaciones metodo = "pearson"
numer_hatco <- hatco[,c(2:8,10,11)]
cor(numer_hatco)
