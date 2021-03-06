#######################################
#                                     #
# AN�LISIS EXPLORATORIO Y PREDICTIVO  #
#       CASO: RIESGO MOROSIDAD        #
#       JES�S SALINAS FLORES          #   
#      jsalinas@lamolina.edu.pe       #
#                                     #
#######################################

# Paquetes a utilizar
library(car)
library(PerformanceAnalytics)
library(psych)
library(ggplot2)
library(gmodels)
library(gplots)
library(rpart)
library(rpart.plot)
library(partykit)
library(pROC)
library(vcd)
library(caret)

#########################
#  1. Lectura de datos  #
#########################

datos <- read.csv("Riesgo_morosidad.csv",sep=";")

# Viendo la estructura de los datos
str(datos)

datos$sexo      <- factor(datos$sexo,levels=c(1,2),
                         labels=c("Masculino","Femenino"))

datos$fonopart  <- factor(datos$fonopart,levels=c(1,2),
                         labels=c("No","Si"))

datos$fonolab   <- factor(datos$fonolab,levels=c(1,2),
                         labels=c("No","Si"))

datos$autovaluo <- factor(datos$autovaluo,levels=c(1,2),
                         labels=c("No","Si"))

datos$esaval    <- factor(datos$esaval,levels=c(1,2),
                         labels=c("No","Si"))

datos$tieneaval <- factor(datos$tieneaval,levels=c(1,2),
                         labels=c("No","Si"))

datos$tiporenta <- factor(datos$tiporenta,levels=c(2,3),
                         labels=c("Fijo","Variable"))

datos$dpto      <- factor(datos$dpto,levels=c(1,2,3,4,5,6),
                          labels=c("Lima","Trujillo","Arequipa",
                                   "Cusco","Ica","Piura"))

datos$morosidad <- factor(datos$morosidad,levels=c(1,2),
                         labels=c("No Moroso","Moroso"))

str(datos)


# 'data.frame':	8471 obs. of  11 variables:
# $ edad      : int  21 22 22 22 22 22 22 22 22 22 ...
# $ sexo      : Factor w/ 2 levels "Masculino","Femenino": 1 1 2 1 1 1 2 1 1 1 ...
# $ nrodepen  : int  5 0 2 1 0 0 0 5 4 5 ...
# $ fonopart  : Factor w/ 2 levels "No","Si": 2 1 2 1 2 2 1 1 2 2 ...
# $ fonolab   : Factor w/ 2 levels "No","Si": 2 2 2 1 2 2 2 1 1 2 ...
# $ autovaluo : Factor w/ 2 levels "No","Si": 1 1 1 1 1 1 1 1 1 1 ...
# $ esaval    : Factor w/ 2 levels "No","Si": 1 1 1 1 1 1 1 1 1 1 ...
# $ tieneaval : Factor w/ 2 levels "No","Si": 1 2 1 2 1 1 1 2 2 1 ...
# $ antiguedad: int  1 11 10 10 10 10 10 7 4 3 ...
# $ tiporenta : Factor w/ 2 levels "Fijo","Variable": 1 1 1 2 1 1 2 2 2 1 ...
# $ dpto      : Factor w/ 6 levels "Lima","Trujillo",..: 1 1 1 1 1 1 1 1 1 1
# $ morosidad : Factor w/ 2 levels "No Moroso","Moroso": 2 1 1 1 1 1 1 2 2 2 ...

# Otra forma directa
library(foreign)
dataset = read.spss("Riesgo_morosidad.sav", 
                    use.value.labels = T,  
                    to.data.frame=TRUE)

str(dataset)

######################################
#  2. An�lisis Exploratorio de Datos #
######################################

# Medidas de Tendencia Central
mean(datos$edad)               
mean(datos$nrodepen)

# Ej 1. Calcular la antiguedad promedio


# Edad promedio seg�n la morosidad
tapply(datos$edad,datos$morosidad,mean) 

# Antiguedad promedio seg�n la morosidad
tapply(datos$antiguedad,datos$morosidad,mean) 


# Ej 2. Calcular el n�mero de dependientes seg�n la morosidad




# Graficar un diagrama de cajas, BoxPlot
boxplot(datos$edad)              

boxplot(datos$edad, ylab = "Edad", col = c("lightblue"), 
        main="Boxplot de la Edad de los clientes")

boxplot(datos$edad ~ datos$morosidad)

boxplot(datos$nrodepen ~ datos$morosidad, 
        main = "Boxplot del n�mero de dependientes seg�n la morosidad",
        xlab = "Condici�n del cliente", 
        ylab = "N�mero de dependientes", 
        col = c("red","blue"))

# Ej 3. Graficar un boxplot de la antig�edad seg�n la morosidad






# Medidas de Variabilidad o Dispersi�n
var(datos$edad)                
var(datos$antiguedad)                 

# Calcula la variancia del monto seg�n la morosidad
tapply(datos$edad,datos$morosidad,var)    


# Resumen descriptivo de los datos
summary(datos)


# Tablas y Gr�ficos para una Variable Cualitativa
# Distribuci�n seg�n la morosidad, frecuencias absolutas
table(datos$morosidad)                

# Distribuci�n seg�n la morosidad, proporciones
prop.table(table(datos$morosidad))    

plot(datos$morosidad,
     main="Gr�fico de barras de la morosidad",
     xlab="Morosidad",
     col=c("blue","orange"))


# Distribuciones para una Variable Cuantitativa
hist(datos$antiguedad)             

hist(datos$antiguedad, 
     main = "Histograma de la Antig�edad de la empresa", 
     xlab = "Antig�edad de la empresa", 
     ylab= "Frecuencia",col = "grey")

hist(datos$antiguedad, 
     nclass=5, 
     main = "Histograma de la Antig�edad de la empresa", 
     xlab = "Antig�edad de la empresa", 
     ylab= "Frecuencia",col = "grey")

# Histograma con funci�n de densidad
hist(datos$antiguedad,
     prob=T,
     ylim=c(0,0.08),
     col="grey")             
lines(density(datos$antiguedad),
      col="blue",
      lwd=2)

# Uso de la funci�n attach
attach(datos)


# Gr�ficos de Barras
plot(sexo)                      

barplot(tapply(nrodepen,morosidad,mean),
        xlab="Morosidad",
        ylab="N�mero de dependientes promedio",
        main="N�mero de dependientes promedio seg�n la morosidad")

barplot(tapply(antiguedad,morosidad,mean),
        xlab="Morosidad",
        ylab="Antig�edad promedio en a�os de la empresa",
        main="Antig�edad promedio seg�n la morosidad")

# Ej 4. Presentar un Gr�fico de Barras de la edad promedio seg�n la morosidad






# Gr�ficos de Pie
pie(table(dpto),
    main="Distribuci�n de clientes por Departamento")


#--------------------------
# Gr�ficos de Dispersi�n 

# Gr�fico de Dispersi�n Edad vs. Monto
plot(edad ~ antiguedad,data=datos,
     xlab = "Antig�edad", 
     ylab = "Edad")

plot(edad ~ antiguedad,
     data=datos, 
     col=ifelse(morosidad=="Moroso","red","blue"), 
     main = "Gr�fico de dispersi�n de edad vs antig�edad seg�n la morosidad", 
     xlab = "Antig�edad", 
     ylab = "Edad")
legend("bottomright",
       pch=c(2,2), 
       col=c("red", "blue"), 
       c("Moroso","No Moroso"), 
       bty="o", 
       box.col="darkgreen",cex=0.8)


# Gr�fico de Dispersi�n con L�nea de Regresi�n Edad vs. Monto
plot(edad ~ antiguedad,data=datos,
     xlab = "Antig�edad", 
     ylab = "Edad",
     col="blue")
abline(lm(edad ~ antiguedad),col="red")



# Selecci�n de algunas variables cuantitativas
datos2 <- datos[,c(1,3,9)]
pairs(datos2)
pairs(~ edad + nrodepen + antiguedad)

library(lattice)
splom(datos2,cex=0.3)

# Grafico de Dispersi�n Matricial
library(car)
scatterplotMatrix(datos2, diagonal = "hist")
scatterplotMatrix(datos2, diagonal = "boxplot")
scatterplotMatrix(datos2, diagonal = "density")

library(PerformanceAnalytics)
chart.Correlation(datos2, histogram=TRUE, pch=19)


# Mapas de calor
library(psych)
cor.plot(cor(datos2),
         main="Mapa de Calor")        


##############################
# 3. Mejorando las gr�ficas  #             
##############################

library(ggplot2)

# Gr�fica de Dispersi�n
plot(datos$antiguedad,datos$edad)
qplot(antiguedad,edad,data = datos,
      xlab = "Antig�edad", ylab = "Edad")

# Color seg�n la condici�n de la morosidad
plot(edad ~ antiguedad,
     data=datos, 
     col=ifelse(morosidad=="Moroso","red","blue"), 
     main = "Gr�fico de dispersi�n de edad vs antig�edad seg�n la morosidad", 
     xlab = "Antig�edad", 
     ylab = "Edad")
legend("bottomright",
       pch=c(2,2), 
       col=c("red", "blue"), 
       c("Moroso","No Moroso"), 
       bty="o", 
       box.col="darkgreen",cex=0.8)

qplot(antiguedad,edad,data=datos,colour=morosidad)              

qplot(antiguedad,edad,
      data=datos,
      colour=morosidad,
      main = "Gr�fico de dispersi�n de edad vs antig�edad seg�n morosidad", 
      xlab = "Antig�edad", 
      ylab = "Edad")
              
# Gr�fica de BoxPlot
plot(datos$morosidad,datos$edad)
qplot(morosidad, edad, data = datos,geom="boxplot")

qplot(morosidad, 
      edad, 
      data = datos, 
      geom="boxplot",
      main = "BoxPlot de la Edad seg�n Morosidad", 
      xlab = "Morosidad", 
      ylab = "Edad (a�os)")


# Histograma
hist(datos$antiguedad)
qplot(antiguedad, data = datos,geom = "histogram")

qplot(antiguedad, data = datos,
      main = "Histograma de la antig�edad de la empresa",
      xlab = "Antig�edad (a�os)",
      ylab = "Frecuencia",
      geom = "histogram",
      colour=I("black"),
      fill=I("blue"))

# Gr�fica de barras
plot(datos$morosidad)                
qplot(morosidad, 
      data = datos, 
      main = "Gr�fico de barras",
      xlab="Situaci�n de la morosidad", 
      ylab="Frecuencia",
      geom = "bar",
      fill=I("blue"))

# Gr�fico en 3 dimensiones
library(car)
scatter3d(edad,nrodepen,antiguedad)
scatter3d(edad,nrodepen,antiguedad,groups=morosidad,surface=FALSE, grid = FALSE, ellipsoid = TRUE)

# Tablas Cruzadas, de Doble Entrada o de Contigencia

prop.table(table(datos$morosidad)) 

table(dpto,morosidad)

library(gmodels)
CrossTable(dpto,
           morosidad,
           prop.t=FALSE,
           prop.r=TRUE,
           prop.c=FALSE,
           prop.chisq=FALSE)

Tabla1 <- prop.table(table(dpto,morosidad),margin=1)

library(gplots)
balloonplot(t(Tabla1), 
            main ="Tabla de Contingencia",
            xlab ="Morosidad", 
            ylab="Departamento",
            label = FALSE, cum.margins=FALSE, 
            label.lines=FALSE,
            show.margins = FALSE)

library(vcd)
mosaic(morosidad ~ dpto, 
       main = "Dpto vs Morosidad", 
       data=datos, shade=TRUE)


#############################
#  4. An�lisis Predictivo   # 
#  Modelo de Clasificaci�n  #
#############################

# �rbol CART 
library(rpart)

# Estimar el arbol
arbol1 <- rpart(morosidad ~ . , 
                data=datos, 
                method="class", 
                control=rpart.control(minsplit=90, minbucket=30, cp=0.001))

# Graficando el arbol
library(rpart.plot)
rpart.plot(arbol1, 
           type=3, 
           extra=101,
           cex = 0.7, 
           nn=TRUE)

# Mejorando los Gr�ficos
library(partykit)
plot(as.party(arbol1), tp_args = list(id = FALSE))

# Importancia de las variables predictoras
arbol1$variable.importance

# Prediciendo la clase y probabilidad
clase.pred <- predict(arbol1,datos[,c(1:11)],type="class")
head(clase.pred)

proba.pred <- predict(arbol1, datos, type = "prob")
head(proba.pred)

proba.pred <- proba.pred[,2]
head(proba.pred)

# Estimando el error de mala clasificaci�n
error <- mean(clase.pred!=datos$morosidad)
error 

# Calcular la matriz de confusi�n
table(datos$morosidad,clase.pred)

library(gmodels)
CrossTable(datos$morosidad,
           clase.pred,
           prop.t=FALSE,
           prop.r=TRUE,
           prop.c=FALSE,
           prop.chisq=FALSE)

library(caret)
confusionMatrix(clase.pred,datos$morosidad,positive="Moroso")

library(pROC)
TARGET <- datos$morosidad
TARGET <- as.numeric(TARGET)

#----------------------------------------
# Calculando �rea bajo la curva
areaROC <- auc(roc(TARGET,proba.pred))
areaROC

# Curva ROC 
ROC <- plot.roc(TARGET,proba.pred, 
                xlab="1- Especificidad", 
                ylab="Sensibilidad", 
                main = paste('�rea bajo la curva =',round(areaROC,4)), 
                col="lightblue")


# Junta el archivo de datos con la columna de predicci�n 
# y de probabilidad
datoscart <- cbind(datos,clase.pred,proba.pred)
str(datoscart)

# Archivo de datos con valor y probabilidad predicha de 
# morosidad.csv
write.csv(datoscart,"morosidad-valor-proba.csv")
