### Analisis factorial exploratorio
# 1 Objetivos
# Reducir el numero de factore ej. HATCO
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
## 2 Diseño del plan de analisis
str(hatco) # Variables y tipos
nrow(hatco) # número de observaciones
## 3 Condiciones de aplicabilidad
## se realiza el analisis con las 7 variables numericas
numer_hatco <- hatco[,c(2:8)]
## Correlations with significance levels
library(Hmisc)
tabcor <- rcorr(as.matrix(numer_hatco), type="pearson") # type can be pearson or spearman
tabcor
## representaciones visuales "correlogramos"
# primera representacion
library(ellipse)
plotcorr(tabcor$r)  # grafico blanco y negro
colorfun <- colorRamp(c("#CC0000","white","#3366CC"), space="Lab")
plotcorr(tabcor$r, col=rgb(colorfun((tabcor$r+1)/2), maxColorValue=255)) # grafico color
# segunda representacion
library(corrgram)
corrgram(tabcor$r, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Datos de Hatco")
# test de esfericidad de Bartlett
n <- nrow(numer_hatco)
p <- ncol(numer_hatco)
chi2 <- -(n-1-(2*p+5)/6)*log(det(tabcor$r))
gl <- p*(p-1)/2
print(chi2)
print(gl)
print(pchisq(chi2,gl,lower.tail=F))
# segunda forma (directa)
library(psych)
print(cortest.bartlett(tabcor$r, n=nrow(numer_hatco)))
## Matriz de corrrelacion anti-imagen
#inverse of the correlation matrix
invR <- solve(tabcor$r)
#partial correlation matrix (-1 * spss anti-image matrix, unless the diagonal)
A <- matrix(1,nrow(invR),ncol(invR))
for (i in 1:nrow(invR)-1){
        for (j in (i+1):ncol(invR)){
                #above the diagonal
                A[i,j] <- -invR[i,j]/sqrt(invR[i,i]*invR[j,j])
                #below the diagonal
                A[j,i] <- A[i,j]
        }
}
colnames(A) <- vlab(colnames(numer_hatco))
rownames(A) <- vlab(colnames(numer_hatco))
print(A)
## Test KMO
# Global
kmo.num <- sum(tabcor$r^2) - sum(diag(tabcor$r^2))
kmo.denom <- kmo.num + (sum(A^2) - sum(diag(A^2)))
kmo <- kmo.num/kmo.denom
print(kmo)
# por variable (para sustituir diagonal en la matriz anti-imagen)
for (j in 1:ncol(numer_hatco)) {
        kmo_j.num <- sum(tabcor$r[,j]^2) - tabcor$r[j,j]^2
        kmo_j.denom <- kmo_j.num + (sum(A[,j]^2) - A[j,j]^2)
        kmo_j <- kmo_j.num/kmo_j.denom
        print(paste(vlab(colnames(numer_hatco)[j]),"=",kmo_j))
}
#### En este ejemplo se elimina la variable Servicio (x5) por tener el valor mas bajo en KMO
#
#
### se repite el analisis pero ahora con solo 6 variables
numer_hatco <- hatco[,c(2:5,7,8)]
## Correlations with significance levels
library(Hmisc)
tabcor <- rcorr(as.matrix(numer_hatco), type="pearson") # type can be pearson or spearman
tabcor
## representaciones visuales "correlogramos"
# primera representacion
library(ellipse)
plotcorr(tabcor$r)  # grafico blanco y negro
colorfun <- colorRamp(c("#CC0000","white","#3366CC"), space="Lab")
plotcorr(tabcor$r, col=rgb(colorfun((tabcor$r+1)/2), maxColorValue=255)) # grafico color
# segunda representacion
library(corrgram)
corrgram(tabcor$r, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Datos de Hatco")
# test de esfericidad de Bartlett
n <- nrow(numer_hatco)
p <- ncol(numer_hatco)
chi2 <- -(n-1-(2*p+5)/6)*log(det(tabcor$r))
gl <- p*(p-1)/2
print(chi2)
print(gl)
print(pchisq(chi2,gl,lower.tail=F))
# segunda forma (directa)
library(psych)
print(cortest.bartlett(tabcor$r, n=nrow(numer_hatco)))
## Matriz de corrrelacion anti-imagen
#inverse of the correlation matrix
invR <- solve(tabcor$r)
#partial correlation matrix (-1 * spss anti-image matrix, unless the diagonal)
A <- matrix(1,nrow(invR),ncol(invR))
for (i in 1:nrow(invR)-1){
        for (j in (i+1):ncol(invR)){
                #above the diagonal
                A[i,j] <- -invR[i,j]/sqrt(invR[i,i]*invR[j,j])
                #below the diagonal
                A[j,i] <- A[i,j]
        }
}
colnames(A) <- vlab(colnames(numer_hatco))
rownames(A) <- vlab(colnames(numer_hatco))
print(A)
## Test KMO
# Global
kmo.num <- sum(tabcor$r^2) - sum(diag(tabcor$r^2))
kmo.denom <- kmo.num + (sum(A^2) - sum(diag(A^2)))
kmo <- kmo.num/kmo.denom
print(kmo)
# por variable (para sustituir diagonal en la matriz anti-imagen)
for (j in 1:ncol(numer_hatco)) {
        kmo_j.num <- sum(tabcor$r[,j]^2) - tabcor$r[j,j]^2
        kmo_j.denom <- kmo_j.num + (sum(A[,j]^2) - A[j,j]^2)
        kmo_j <- kmo_j.num/kmo_j.denom
        print(paste(vlab(colnames(numer_hatco)[j]),"=",kmo_j))
}


## 4 Obtención de los factores y establecimiento del ajuste global
fit <- princomp(numer_hatco, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)
##################################
# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(numer_hatco)) # get eigenvalues
ap <- parallel(subject=nrow(numer_hatco),var=ncol(numer_hatco),
               rep=100,cent=.3)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
#### 5 interpretación de los resultados
# Varimax Rotated Principal Components
# retaining 2 components 
library(psych)
fit2 <- principal(numer_hatco, nfactors=2, rotate="varimax")
fit2 # print results
### 6 Validacion de resultados
# para validar con la mitad aleatoriamente obtenida
mitad <- numer_hatco[sample(nrow(numer_hatco), nrow(numer_hatco)/2), ]
fit3 <- principal(mitad, nfactors=2, rotate="varimax")
fit3 # print results
