# 18.Mayo.16.Tarea
install.packages("foreign")
install.packages("forecast")
install.packages("fpp")
require (foreign)
require (forecast)
require (fpp)

###Tendencia hacia arriba
cem <- read.csv("C:\\Users\\HP\\Downloads\\cm.csv")
cemts <- ts(cem [,2] ,start= 2000, frequency = 12)
cemts

m1 <- holt(cemts, initial = "simple", h = 4)
m2 <- holt(cemts, initial = "simple",exponential = T, h = 4)
m3 <- holt(cemts, damped = T)
m4 <- holt(cemts, exponential = T, damped = T)
m5 <-hw(cemts, seasonal = "additive")
m6 <-hw(cemts, seasonal = "multiplicative")
plot(m1, plot.conf=FALSE, ylab="Precio",
     xlab="Año", main="", fcol="white", type="o")

lines(fitted(m1), col="red", type="o")
lines(fitted(m2), col="green", type="o")
lines(fitted(m3), col="blue", type="o")
lines(fitted(m4), col="orange", type="o")
lines(fitted(m5), col="gold", type="o")
lines(fitted(m6), col="chocolate", type="o")

lines(m1$mean, col="red", type="o")
lines(m2$mean, col="green", type="o")
lines(m3$mean, col="blue", type="o")
lines(m4$mean, col="orange", type="o")
lines(m5$mean, col="gold", type="o")
lines(m6$mean, col="chocolate", type="o")
legend("topleft",lty=1, col=c(1,"red","green","blue", "orange","gold", "chocolate"), 
       c("cemts", expression("Modelo lineal Holt"), expression("Modelo holt exponencial"),
         expression("Modelo tendencia aditiva amortiguado"), expression("Modelo tendencia multiplicativa amortiguado"),
         expression("Modelo holt winter estacinal"), expression("Modelo holt winter multiplicativo")
       ),pch=1)

cemm <- meanf(cemts, 4)
cemin <- naive(cemts, 4)
cemie <- snaive(cemts, 4)
cemde <- rwf(cemts, 4, drift = T)
uno <- accuracy(cemm)
dos <- accuracy(cemin)
tres <- accuracy(cemie)
cuatro <- accuracy(cemde)
uno
dos
tres
cuatro
d1<-data.frame(uno,dos,tres,cuatro)
View(d1)

###Tendencia hacia abajo
mor <- read.csv("C:\\Users\\HP\\Downloads\\Mor.csv")
morts <- ts(mor [,2] ,start= 1990, frequency = 12)
morts

m1 <- holt(morts, initial = "simple", h = 4)
m2 <- holt(morts, initial = "simple",exponential = T, h = 4)
m3 <- holt(morts, damped = T)
m4 <- holt(morts, exponential = T, damped = T)
m5 <-hw(morts, seasonal = "additive")
m6 <-hw(morts, seasonal = "multiplicative")
plot(m1, plot.conf=FALSE, ylab="Mortalidad",
     xlab="Año", main="", fcol="white", type="o")

lines(fitted(m1), col="red", type="o")
lines(fitted(m2), col="green", type="o")
lines(fitted(m3), col="blue", type="o")
lines(fitted(m4), col="orange", type="o")
lines(fitted(m5), col="gold", type="o")
lines(fitted(m6), col="chocolate", type="o")

lines(m1$mean, col="red", type="o")
lines(m2$mean, col="green", type="o")
lines(m3$mean, col="blue", type="o")
lines(m4$mean, col="orange", type="o")
lines(m5$mean, col="gold", type="o")
lines(m6$mean, col="chocolate", type="o")
legend("topleft",lty=1, col=c(1,"red","green","blue", "orange","gold", "chocolate"), 
       c("morts", expression("Modelo lineal Holt"), expression("Modelo holt exponencial"),
         expression("Modelo tendencia aditiva amortiguado"), expression("Modelo tendencia multiplicativa amortiguado"),
         expression("Modelo holt winter estacinal"), expression("Modelo holt winter multiplicativo")
       ),pch=1)

morm <- meanf(morts, 4)
morin <- naive(morts, 4)
morie <- snaive(morts, 4)
morde <- rwf(morts, 4, drift = T)
uno1 <- accuracy(morm)
dos1 <- accuracy(morin)
tres1 <- accuracy(morie)
cuatro1 <- accuracy(morde)
uno1
dos1
tres1
cuatro1
d<-data.frame(uno1,dos1,tres1,cuatro1)
View(d)

