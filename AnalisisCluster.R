#### ANALISIS CLUSTER
## 1 Objetivos del AC
# Segmentar: establecer grupos de clientes de HATCO en función a la imagen que tienen de la empresa
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
numer_hatco <- hatco[,c(2:8)]
## 2 Desarrollo del plan de analisis
# Distancia Mahalanobis
Sx <- cov(numer_hatco)
D2 <- mahalanobis(numer_hatco, colMeans(numer_hatco), Sx)
plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, n=100, p=3") ; rug(D2)
qqplot(qchisq(ppoints(100), df = 3), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                                 " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')
boxplot(D2)
abline(,,12, col = 'red')
D2 >= 12 # Posibles outliers
## 3 Condiciones de aplicabilidad
# Checar multicolinealidad
### Grafico scatterplot
library(car)
spm(~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data=numer_hatco, 
    var.labels=c(vlab("x1"),vlab("x2"),vlab("x3"), vlab("x4"), 
                 vlab("x5"), vlab("x6"), vlab("x7"))) ## todas las variables
### Tabla de correlaciones metodo = "pearson"
cor(numer_hatco)
## 4 Estimacion del modelo y ajuste global
# Primero con metodo jerarquico
distancias<-dist(numer_hatco)
cluster.1<-hclust(distancias,method="ward.D")
summary(cluster.1)
plot(cluster.1)
heatmap(as.matrix(distancias))
# Ahora metodo no jerarquico con 2 clusters
cluster.1.nojerar<-kmeans(distancias,2)
asignacion<-data.frame(cluster.1.nojerar$cluster)
analisis.ok<-cbind(numer_hatco,asignacion)
names(analisis.ok)[length(colnames(analisis.ok))]<-"cluster" #renombramos la última variable
plot(analisis.ok$x2, analisis.ok$x3, col=analisis.ok$cluster, pch=19, cex = 2)
points(cluster.1.nojerar$centers, col=1:2, pch= 3, cex=3, lwd=3)
heatmap(as.matrix(cluster.1.nojerar$centers))
