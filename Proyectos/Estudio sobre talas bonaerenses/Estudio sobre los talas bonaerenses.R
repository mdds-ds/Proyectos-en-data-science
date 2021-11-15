rm(list=ls())

# Los talares bonaerenses constituyen el principal bosque nativo de la provincia de Buenos
# Aires. Sin embargo, se encuentran en continua reducción. Su superficie actual es mucho
# menor que la original, que se extendía desde el norte de la provincia de Buenos Aires, en
# una estrecha franja costera que llegaba hasta Mar del Plata. Esta es la zona más poblada de
# Argentina y, donde las ciudades crecieron, los talares desaparecieron casi totalmente. 

# Para poder tomar medidas adecuadas de manejo a fin de evitar su desaparición total, es
# necesario tener una estimación del estado actual de los remanentes de talar que todavía
# perduran.

# Se extrajeron muestras aleatorias de tamaño n a partir de una población de 500 talas 
# (Celtis ehrenbergiana) de la reserva "El Destino" (partido de Magdalena).


#  Cargo la base de datos poblacion_talas.txt y observo qué información contiene. 
setwd("C:/Users/Matu/Desktop/Data Science/Proyectos/Estudio sobre talas bonaerenses")
poblacion_talas <- read.table("poblacion_talas.txt", header=T)


# Esta base de datos simula una población compuesta por 500 árboles, en un área donde se extrajo 
# gran parte de los individuos de mayor tamaño.


# Trabajo con la variable altura.
# La simetría de la distribución de esta variable; gráfico
min<-min(poblacion_talas$altura)
max<-max(poblacion_talas$altura)
cant_cortes<-sqrt(length(poblacion_talas$altura))
cant_cortes_red<-round(cant_cortes)
intervalos <- seq(min,max, by=((max-min)/cant_cortes_red))
hist(poblacion_talas$altura, breaks = intervalos, col = "green", 
     main = "Histograma altura árboles",xlab="Altura árboles")
# La simetría es asimétrica positiva (asimetría hacia la derecha).



# ¿Cuál es la altura media de los talas de esta población y su dispersión? 
mu <- mean(poblacion_talas$altura)
mu
# La altura media de los talas es de 4.6335909m.
# La función var(x) calcula la varianza de una muestra. Para calcular la varianza poblacional,
# se puede definir la función: 

varp<-function(x) mean((x-mean(x))^2)

#y luego aplicarla a la variable de interés.
varp(poblacion_talas$altura)
# La varianza de la altura de los talas es de 1.569006m^2.
# Se debe crear la función varp() ya que, en R, el comando var() calcula la varianza 
#muestral, y, entre la varianza poblacional (lo que se está pidiendo) y la muestral, 
#cambian los grados de libertad.

# Estos valores en la población reciben el nombre de parámetros, siendo mu la media, sigma el desvío estándar y sigma al cuadrado la varianza. Los parámetros no cambian a menos que cambie la población. 


# Tomo muestras aleatorias de la variable altura que sean de distinto tamaño (n=5, n=20
# y n=50), para lo cual uso el comando "sample" del paquete "base". 

?sample

# Tomo muestras de cada uno de los tamaños y las guardo como objetos diferentes. Por ejemplo:
muestra_n5<-sample(poblacion_talas$altura, 5, replace=TRUE)
muestra_n5

# Calculo la media y la varianza de cada una de las muestras.

media_muestra_n5<-mean(muestra_n5)
media_muestra_n5
var_muestra_n5<-var(muestra_n5)
var_muestra_n5

# Vuelco la información obtenida de media y varianza para cada uno de los tamaños de 
# muestra en la planilla. 


# Automatización de la extracción de muestras.

# Le pido a R que extraiga muchas muestras de tamaño n
# al mismo tiempo. Esto permitirá poder hacer la simulación
# individualmente, pudiendo cambiar las condiciones iniciales fácilmente.
# Lo hago en dos pasos:

# Paso 1) Creo un data frame que tenga tantas filas como muestras quiero extraer.
# Lo llamo "muestreo_500_nx" porque voy a extraer 500 muestras al azar de un
# determinado tamaño (n).
# Este data frame tendrá tres columnas, la primera indicará el numero de la muestra
# extraída; la segunda, la media de esa muestra; y la tercera, su varianza. 

# Ejemplo para n = 5

cant_muestras<- 500
muestreo_500_n5 <- data.frame(muestra=1:cant_muestras,media=NA, var=NA)
head(muestreo_500_n5)

# n = 20
cant_muestras<- 500
muestreo_500_n20 <- data.frame(muestra=1:cant_muestras,media=NA, var=NA)
head(muestreo_500_n20)

# n = 50
cant_muestras<- 500
muestreo_500_n50 <- data.frame(muestra=1:cant_muestras,media=NA, var=NA)
head(muestreo_500_n50)

# Paso 2) Mediante el comando "for" le pedimos a R que repita una misma acción 
# tantas veces como el número de muestras que sacamos ("cant_muestras"). 
# A esto se llama "bucle" o "loop".Esta acción llena las columnas "media" y "var" del data frame. 

for(i in 1:cant_muestras) {
  muestra_n5 <-(sample(poblacion_talas$altura,5, replace=TRUE))
  muestreo_500_n5$media[i] <-mean(muestra_n5)
  muestreo_500_n5$var[i] <-var(muestra_n5)
  }
head(muestreo_500_n5)

for(i in 1:cant_muestras) {
  muestra_n20 <-(sample(poblacion_talas$altura,20, replace=TRUE))
  muestreo_500_n20$media[i] <-mean(muestra_n20)
  muestreo_500_n20$var[i] <-var(muestra_n20)
}
head(muestreo_500_n20)

for(i in 1:cant_muestras) {
  muestra_n50 <-(sample(poblacion_talas$altura,50, replace=TRUE))
  muestreo_500_n50$media[i] <-mean(muestra_n50)
  muestreo_500_n50$var[i] <-var(muestra_n50)
}
head(muestreo_500_n50)

# Comparo la forma de la distribución de la variable media para los distintos
# tamaños de muestra y respecto de la distribución original de la variable altura.
# Para ello, en primer lugar genero una ventana que permita visualizar juntos
# todos los histogramas:  

windows()

par(mfrow=c(4,1))
hist(poblacion_talas$altura, xlim=c(0, 10), ylim=c(0,200))
hist(muestreo_500_n5$media, xlim=c(0, 10), ylim=c(0,200))
hist(muestreo_500_n20$media, xlim=c(0, 10), ylim=c(0,200))
hist(muestreo_500_n50$media, xlim=c(0, 10), ylim=c(0,200))
par(mfrow=c(1,1))

# Podemos concluir que a pesar de que el histograma de las alturas es asimétrico a la derecha, la de las medias es simétrico, además a mayor n menor variabilidad en los datos.
# Teorema central del límite: si de una población no normal  se extraen muestras de un tamaño grande se demuestra que las medias de las muestras tienen un comportamiento normal.
#  La varianza de una media muestral varía inversamente proporcional al n de  la muestra.

# El desvío de la variable media recibe el nombre de ERROR ESTANDAR

# La varianza de la distribución de medias tiende a disminuir mientras se aumenta el n.

# Al aumentar el n la distribución de la variable media se normaliza.
# La varianza de una muestra es sigma^2/sqrt(n), por lo que al aumentar n disminuye el Error Estándar.

# Ahora compararo la forma de la distribución de la variable media de la altura de
# talas obtenida para cada tamaño de muestra n (5, 20 y 50) con la forma esperada si fuese
# normal.
# Tener en cuenta que el eje y del gráfico ya no es frecuencia sino que es una función de
# densidad de probabilidad f(x).

media_original<-mean(poblacion_talas$altura)
SD_original<-sqrt(varp(poblacion_talas$altura)) 

windows()

par(mfrow=c(4,1))
hist(poblacion_talas$altura, xlim=c(2, 10), freq=F ,col = 8 , main ="Original",xlab=NULL)
curve(dnorm(x, mean=media_original,sd=SD_original), col = 2, lwd = 2, add = TRUE)
hist(muestreo_500_n5$media, xlim=c(2, 10),  freq=F ,col = 8 , main ="n= 5" ,xlab=NULL)
curve(dnorm(x, mean=media_original,sd=SD_original/sqrt(5)), col = 2, lwd = 2, add = TRUE)
hist(muestreo_500_n20$media, xlim=c(2, 10),freq=F , col = 8, main = "n= 20",xlab=NULL)
curve(dnorm(x, mean=media_original,sd=SD_original/sqrt(20)), col = 2, lwd = 2, add = TRUE)
hist(muestreo_500_n50$media, xlim=c(2, 10),freq=F , col =8, main ="n= 50",xlab=NULL )
curve(dnorm(x, mean=media_original,sd=SD_original/sqrt(50)), col = 2, lwd = 2, add = TRUE)
par(mfrow=c(1,1))


# Comparo los gráficos obtenidos y concluyo.
# Efectivamente se comprueba que al aumentar el n, la distribución de las muestras se aproxima
# y se asemeja cada vez más a una distribución normal.
# Observamos que, a medida que aumenta el tamaño de las muestras (n), la distribución muestral de las medias se comporta cada vez más como una distribución Normal (aun si la variable original no presenta tal distribución).
# Se concluye que (tal como se enuncia en el Teorema Central del Límite) cuando de una poblacio??n con distribucio??n no Normal o desconocida, de media ?? y desvi??o esta??ndar ??, se extraen infinitas muestras aleatorias de taman~o n, cuando n es lo suficientemente grande (n ??? 30) el promedio (X) se comporta segu??n una distribucio??n Normal. 


# Ahora exploro si las características de la distribución de la variable media que
# deducí (forma de la distribución, esperanza y varianza), se mantienen
# para una variable con una distribución normal.

# Para ello, simularo una población de ligustros (Ligustrum lucidum), un árbol
# originario de Asia y muy común en la reserva. Se sabe que la altura de esta población en la
# reserva presenta una media de 10 m y un desvío de 1 m.
# A partir de esta información genero 1000 datos de la variable "altura" con distribución
# normal con media de 10 m y desvió de 1 m usando la función rnorm:

?rnorm
altura<- rnorm(1000, 10, 1)
hist(altura)
cant_muestras <- 500
muestreo_l_500_n20 <- data.frame(muestra=1:cant_muestras,media=NA, var=NA)

# Repito la simulación extrayendo muestras de tamaño n (5, 20 y 50), al igual que antes
# , graficando los histogramas correspondientes a la variable altura media.

# Ej. n=20
for(i in 1:cant_muestras) {
  muestra_n20 <-(sample(altura,20, replace=TRUE))
  muestreo_l_500_n20$media[i] <-mean(muestra_n20)
  muestreo_l_500_n20$var[i] <-var(muestra_n20)
}
head(muestreo_l_500_n20)

hist(altura, xlim=c(6,14))
hist(muestreo_l_500_n20$media, xlim=c(6,14))
hist(muestreo_l_500_n20$var, xlim=c(0, 4))


# Observo la forma de la distribución de la media y la varianza para la población original y 
# para cada tamaño de muestra.
# La distribución de las medias se hace normal ya que antes ya era normal.
# La distribución de las varianzas es asimétrica.


