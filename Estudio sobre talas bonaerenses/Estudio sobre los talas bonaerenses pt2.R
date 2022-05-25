# Los talares bonaerenses constituyen el principal bosque nativo de la provincia de Buenos
# Aires. Sin embargo, se encuentran en continua reducción. Su superficie actual es mucho
# menor que la original, que se extendía desde el norte de la provincia de Buenos Aires, en
# una estrecha franja costera que llegaba hasta Mar del Plata. Esta es la zona más poblada de
# Argentina y, donde las ciudades crecieron, los talares desaparecieron casi totalmente. 

# Para poder tomar medidas adecuadas de manejo para evitar su desaparición total, es
# necesario tener una estimación del estado actual de los remanentes de talar que todavía
# perduran.

# A continuación vamos a simular la extracción de muestras aleatorias de tamaño n a partir de
# una población de 500 talas (Celtis ehrenbergiana) de la reserva "El Destino" (partido de
# Magdalena).


#  #Cargo la base de datos BD_poblacion_talas.txt y observo qué información contiene.
datos<-read.table("BD_poblacion_talas.txt",header=T)
datos


# Trabajaremos con la variable altura
min<-min(datos$altura)
max<-max(datos$altura)
cant_cortes<-sqrt(length(datos$altura))
cant_cortes_red<-round(cant_cortes)
intervalos <- seq(min,max, by=((max-min)/cant_cortes_red))
distribucion <- hist(datos$altura, breaks = intervalos, col = "orange", main = "Dist de alturas",xlab="Altura en metros", ylab="frecuencia")
#Tiene distribución normal.


#Media poblacional para la altura es 4.6335909m


# Tomo tres muestras aleatorias de tamaño 30 de la variable altura mediante el comando "sample"


#Para muestra 2 y 3
muestra_n30_2<-sample(datos$altura, 30, replace = TRUE)
muestra_n30_2

muestra_n30_3<-sample(datos$altura, 30, replace = TRUE)
muestra_n30_3



# Calculo la media y el desvío estándar de las muestras. 
#ej:
m1<-mean(muestra_n30_1)
sd1<-sd(muestra_n30_1)

#para muestra 2 y 3
mu2<-mean(muestra_n30_2)
mu3<-mean(muestra_n30_3)
sd2<-sd(muestra_n30_2)
sd3<-sd(muestra_n30_3)



# Los elementos que componen un IC son: el estimador del parametro +- el EM (error muestral).
# A su vez este EM es el resultado del producto del valor critico (VC) y el Error Estandar (ES)

# El valor critico es la medida de la confianza con la que hay que trabajar y esta expresada en desvios estandares,
# por eso se utiliza la distribución normal estandar (normal 0,1) cuando se conoce sigma (desvío poblacional), o la distribución t 
# cuando sigma es desconocido

# El ES es la variabilidad del estimador



# Estimo con las muestras 1, 2 y 3 la altura media de la población de talas con una confianza
#   del 95 %, sabiendo que la varianza poblacional es de 1,57 m al cuadrado. ¨Para ello debo calcular
#   el valor crítico y el error muestral. Como alfa = 5% y se tiene que repartir en dos intervalos,
#   le resto 2,5% a cada extremo.

VC<-qnorm(0.975, 0, 1)
VC
VC2<-qnorm(0.025, 0, 1) 
VC2
ES<-sqrt(1.57)/sqrt(30)
ES

#Error muestral = ES  = 0.228765

#Para muestra 2 y 3
LI_2<-mu2-(VC*ES)
LS_2<-mu2+(VC*ES)
LI_3<-mu3-(VC*ES)
LS_3<-mu3+(VC*ES)


# EJ
c(LI_1, LS_1)  #  agregar valor obtenido
# Límites inferior y superior para la muestra 2 y 3:
c(LI_2, LS_2)
c(LI_3, LS_3)

# Alturas medias
mu2
mu3



#Estimo con la muestra 2 y 3 la altura media de la población de talas con una confianza
#del 95 %, con varianza desconocida.
# Estos datos fueron obtenidos a partir de la SD de cada muestra, y no de la varianza poblacional,
# por lo que al ser obtenidos de estimadores, no son valores constantes.


VC3<-qt(0.975,29) #VC obtenido de la distribución t
VC3

ES_2<-sd2/sqrt(30)
ES_3<-sd3/sqrt(30)

LI_2.2<-m2-(VC3*ES_2)
LS_2.2<-m2+(VC3*ES_2)
EM2<- VC3*ES_2
EM2

LI_3.2<-m3-(VC3*ES_3)
LS_3.2<-m3+(VC3*ES_3)
EM3<- VC3*ES_3
EM3
c(LI_2.2, LS_2.2) # agregar valor obtenido -> 4.040525 a 4.734227
c(LI_3.2, LS_3.2)  # idem -> 4.267673 a 5.136426

#####################   ACTIVIDAD 1b   ##########################
# 1.b. Explore las simulaciones realizadas sobre la población de talas y resuelva.

# Le pediremos a R que extraiga muchas muestras de tamaño n = 30.
# Esto les permitirá poder hacer la simulación individualmente.
# Lo haremos en dos pasos:

# Paso 1) Crearemos un data frame que tenga tantas filas como muestras queramos extraer.
# Lo llamaremos "muestreo_n30" porque  vamos a extraer 100 muestras al azar de un
# determinado tamaño (n = 30) y considerando unicamente para la variable altura (m)
# Este data frame tendrá cinco columnas, la primera indicará el numero de la muestra
# extraída; la segunda, la media de esa muestra; la tercera, su varianza; 
# la cuarta el límite inferior  y la quinta el límite superior del intervalo de confianza del 95%.. 


cant_muestras<- 100
muestreo_n30 <- data.frame(muestra=1:cant_muestras,media=NA, var=NA,LI=NA, LS=NA)
muestreo_n30

# Paso 2) Mediante el comando "for" le pedimos a R que repita una misma acción 
# tantas veces como el número de muestras que sacamos ("cant_muestras"). 
# A esto se llama "bucle" o "loop".Esta acción llena las columnas del data frame. 

#Nota: Las siguientes líneas de comando se proporcionan sólo a los fines didácticos. 
#Saber cómo hacer un loop excede los objetivos del TP y no será evaluado.

for(i in 1:cant_muestras) {
  muestra_n30 <-(sample(datos$altura,30, replace=TRUE))
  muestreo_n30$media[i] <-mean(muestra_n30)
  muestreo_n30$LI[i] <-muestreo_n30$media[i]-(qnorm(0.975, 0, 1)*(sqrt(1.57))/sqrt(30))
  muestreo_n30$LS[i] <-muestreo_n30$media[i]+(qnorm(0.975, 0, 1)*(sqrt(1.57))/sqrt(30))
}
muestreo_n30
data<-data.frame(muestreo_n30)

# ¿Cuánto fue  la cobertura de su simulación (número de IC que contienen a mu)?
# En mi caso la cobertura fue de 98 intervalos conteniendo a mu. El intervalo 46
# no incluye a mu por debajo y el intervalo 58 no lo incluye por arriba.

cobertura_S1arr<-subset(data, subset=LI>4.6335909) #cuenta cuántos intervalos no incluyen a mu por debajo
cobertura_S2deb<-subset(data, subset=LS<4.6335909) #cuenta cuántos intervalos no incluyen a mu por arriba
cobertura<-nrow(data)-(nrow(cobertura_S1arr)+nrow(cobertura_S2deb))

## RESPUESTA:


# Representación Gráfica de los intervalos obtenidos (La línea horizontal roja representa el
# valor de la media poblacional, mu = 4.63 m)
#Nota: Las siguientes líneas de comando se proporcionan sólo a los fines didácticos.

library(ggplot2)
ggplot(data = data) + 
  geom_errorbar(aes(x = data$muestra, ymin = data$LI, ymax = data$LS), lwd = 1.05) +
  geom_hline(yintercept = 4.63, lwd = 1.025, colour = "red") +
  xlab("IC 95% - Simulación  1") +
  ylab("Altura (metros)") +
  scale_x_continuous(breaks = NULL, limits = c(0, 100)) +
  scale_y_continuous(limits = c(3, 7), labels = as.character(seq(0, 10, 2)),
                     breaks = seq(0, 10, 2))


# Chequée en el gráfico la cobertura de su simulación (número de IC que contienen a mu.

## RESPUESTA: 2 intervalos no contienen a mu.


#########################################################################################
## Repita nuevamente la simulación, pero agregue en la tercer columna el calculo de la varianza 
#  de cada muestra. Analice la cobertura de la simulación. 


muestreo_n30.2<- data.frame(muestra=1:cant_muestras,media=NA, var=NA,LI=NA, LS=NA)
head(muestreo_n30.2)
for(i in 1:cant_muestras) {
  muestra_n30 <-(sample(datos$altura,30, replace=TRUE))
  muestreo_n30.2$media[i] <-mean(muestra_n30)
  muestreo_n30.2$var[i] <-var(muestra_n30)
  muestreo_n30.2$LI[i] <-muestreo_n30.2$media[i]-(qt(0.975, 29)*(sqrt(muestreo_n30.2$var[i]))/sqrt(30))
  muestreo_n30.2$LS[i] <-muestreo_n30.2$media[i]+(qt(0.975, 29)*(sqrt(muestreo_n30.2$var[i]))/sqrt(30))
}
muestreo_n30.2
data2<-data.frame(muestreo_n30.2)

cobertura_s2arr <-subset(data2, subset=LI>4.633591) #cuenta cuántos intervalos no incluyen a mu por debajo
cobertura_s2deb<-subset(data2, subset=LS<4.633591) #cuenta cuántos intervalos no incluyen a mu por arriba


library(ggplot2)
ggplot(data = data2) + 
  geom_errorbar(aes(x = data2$muestra, ymin = data2$LI, ymax = data2$LS), lwd = 1.05) +
  geom_hline(yintercept = 4.63, lwd = 1.025, colour = "BLUE") +
  xlab("IC 95% - Simulación 2") +
  ylab("Altura (metros)") +
  scale_x_continuous(breaks = NULL, limits = c(0, 100)) +
  scale_y_continuous(limits = c(3, 7), labels = as.character(seq(0, 10, 2)),
                     breaks = seq(0, 10, 2))



# ¿Qué diferencias observa con respecto a la simulación 1?

## RESPUESTA: Al tener en cuenta la varianza, hay más intervalos que quedarán excluidos.
# Quedaron 95 intervalos de 100. Además en estos nuevos intervalos, la longitud de los mismos
# es más variable, ya que sin tener en cuenta la varianza los intervalos tenían una longitud
# más o menos constante de 0.9.



####
#####################   ACTIVIDAD 1.c.   ##########################
# Repetirán la simulación del punto 1b para la variable DAP (cm). Estime el DAP medio
# de la población con una confianza  del 90 %.


# Paso 1) Crearemos un data frame que tenga tantas filas como muestras queramos extraer.
# Lo llamaremos "muestreo_n30" porque vamos a extraer 100 muestras al azar de un
# determinado tamaño (n = 30) y considerando solo la variable DAP (cm).
# Este data frame tendrá cinco columnas, la primera indicará el numero de la muestra
# extraída; la segunda, la media de esa muestra; la tercera, su varianza; 
# la cuarta el límite inferior  y la quinta el límite superior del intervalo de confianza del 95%.. 

media_DAP<-mean(datos$DAP)
varp<-function(x) mean((x-mean(x))^2) 
Var_DAP<- varp(datos$DAP)
hist(datos$DAP, xlim=c(0, 70), ylim=c(0, 200), main = "", 
     ylab = "Frecuencia absoluta",xlab = "DAP(cm)", col = "red")

cant_muestras<- 100
muestreo_n30_DAP <- data.frame(muestra=1:cant_muestras,media=NA, var=NA,LI=NA, LS=NA)
muestreo_n30_DAP

# Paso 2) Mediante el comando "for" le pedimos a R que repita una misma acción 
# tantas veces como el número de muestras que sacamos ("cant_muestras"). 
# A esto se llama "bucle" o "loop".Esta acción llena las columnas del data frame. 

#Nota: Las siguientes líneas de comando se proporcionan sólo a los fines didácticos. Saber cómo hacer un loop excede los objetivos del TP y no será evaluado.

for(i in 1:cant_muestras) {
  muestra_n30_DAP <-(sample(datos$DAP,30, replace=TRUE))
  muestreo_n30_DAP$media[i] <-mean(muestra_n30_DAP)
  muestreo_n30_DAP$var[i] <-var(muestra_n30_DAP)
  muestreo_n30_DAP$LI[i] <-muestreo_n30_DAP$media[i]-(qnorm(0.95, 0, 1)*(sqrt(146.3))/sqrt(30))
  muestreo_n30_DAP$LS[i] <-muestreo_n30_DAP$media[i]+(qnorm(0.95, 0, 1)*(sqrt(146.3))/sqrt(30))
}
muestreo_n30_DAP
data_DAP<-data.frame(muestreo_n30_DAP)


# Representación Gráfica de los intervalos obtenidos (La línea horizontal roja representa el
# valor de la media poblacional, mu = 36.3 cm)
#Nota: Las siguientes líneas de comando se proporcionan sólo a los fines didácticos.

library(ggplot2)
ggplot(data = data_DAP) + 
  geom_errorbar(aes(x = data_DAP$muestra, ymin = data_DAP$LI, ymax = data_DAP$LS), lwd = 1.05) +
  geom_hline(yintercept = 36.28, lwd = 1.025, colour = "red") +
  xlab("IC 95% - Simulación  1") +
  ylab("DAP (centímetros)") +
  scale_x_continuous(breaks = NULL, limits = c(0, 100)) +
  scale_y_continuous(limits = c(25, 47), labels = as.character(seq(0, 44, 2)),
                     breaks = seq(0, 44, 2))


# ¿Cuánto fue  la cobertura de su simulación (número de IC que contienen a mu)?
cobertura_s2arr_DAP <-subset(data_DAP, subset=LI>36.3) #cuenta cuántos intervalos no incluyen a mu por debajo
cobertura_s2deb_DAP<-subset(data_DAP, subset=LS<36.3) #cuenta cuántos intervalos no incluyen a mu por arriba


## RESPUESTA: Hubo 91 intervalos que contuvieron a mu para la variable DAP.



### Addenda: Comando para estimar  el IC de una muestra (SIGMA DESCONOCIDO)

library(Rmisc)

CI(data$variable, ci=0.95)  # comando por default 

# ej para la altura y el DAP tomando la muestra de n=30 
CI(muestra_n30_1,
   ci=0.95)


CI(muestra_n30_DAP,
   ci=0.95)





