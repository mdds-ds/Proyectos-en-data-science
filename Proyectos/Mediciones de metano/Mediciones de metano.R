#### ANÁLISIS DE MUESTRAS DE MEDICIONES DE METANO

#El gas metano, proveniente de variadas actividades humanas y de fuentes 
#naturales, es un generador de efecto invernadero varias veces más potente que 
#el CO2. En los sedimentos presentes en humedales del delta del Río Paraná 
#existen grandes reservas de este gas, el que ha sido utilizado por algunos de 
#los isleños como biogas. En un estudio cuyo objetivo fue estimar la concentración
#de metano (partes por millón; ppm) en superficie en una zona del Delta 
#Bonaerense se obtuvieron los resultados indicados en el archivo metano.txt. 

rm(list=ls())
setwd("C:/Users/Matu/Desktop/Data SCience/Proyectos/Mediciones de metano")   #seteo carpeta de trabajo

#   Cargo la base de datos CH4 en R. 
datos <- read.table("metano.txt", header = T)

# genero una muestra aleatoria con el siguiente comando.

m1 <- c(sample (datos$CH4, 50, replace = T))

# Se puede ver que las observaciones no se ajustan a la recta normal del gráfico 
#de dispersión, por lo que es seguro decir que la muestra no sigue distribución 
#normal:
qqnorm(datos$CH4)
qqline(datos$CH4, col=2)
install.packages("moments")
library(moments)
asimetría <- skewness(datos$CH4)
asimetría
curtosis <- kurtosis(datos$CH4)
curtosis
m1

# Hay 4 datos outliers:
boxplot(m1, main="Boxplot para muestra m1", ylab="Concentración de CH4 en ppm")

# La muestra es asimétrica a la derecha y su coeficiente de asimetría es 
#positivo. Al mismo tiempo, la media es mayor que la 
#mediana y, en el gráfico de caja (boxplot), la mediana está desplazada hacia abajo
#de la caja.:
summary(m1)

# Intervalos de clase:
hist(datos$CH4, breaks = "Sturges", col = "blue", main = "Histograma de Concentración CH4")

N_intervalos <- 1+3.3*log10(1018) # Fórmula de regla de Sturges
cant_cortes <- round(N_intervalos)
min <- min(datos$CH4)
max <- max(datos$CH4)
Amplitud <- (max-min)/cant_cortes
intervalos<- seq(min, max, by = Amplitud)
ft <- cut(datos$CH4, intervalos, include.lowest = TRUE)
ft <- transform(table(ft))
ft_completa <- transform(ft,rf=prop.table(Freq),cf=cumsum(Freq),crf=cumsum(prop.table(Freq)))
ft_completa

# Genero el intervalo de confianza (90%) respecto a donde se encuentra la media:
library(Rmisc)
CI <- CI(m1,ci=0.90) # Sigma (Desviación estándar) es desconocido.
CI
LI<-round(14.29491,2)
LS<-round(26.11789,2)

# Si tengo mu=media y sigma=desv estandar la probabilidad de obtener un promedio 
#menor al obtenido será:
mu_muestral<-mean(m1)
mu = 20.78
sigma = 25.1
Probabilidad_mu_menor <- round(pnorm(mu_muestral,mu,sigma)-dnorm(mu_muestral,mu,sigma),2)
Probabilidad_mu_menor

# Se realizó un grillado del área de estudio y se mapearon puntos donde se 
#relevaron concentraciones más elevadas de metano (> a 100 ppm) Obteniendo un 
#promedio de 2,2 puntos por cuadrante (es una dist Poisson):
#la probabilidad de obtener en un cuadrante elegido al azar entre 3 y 5 puntos es:
lambda = 2.2
Prob_x_entre3y5 <- round((ppois(5,lambda) - ppois(3,lambda) + dpois(3,lambda)),3)
Prob_x_entre3y5

#Calculando el Coeficiente de Dispersión que es igual a varianza/media, si éste 
#es mayor a uno significa que los puntos están agrupados en el espacio.


####################################################

#A nivel global, gran parte de las emisiones del gas metano son debidas a la 
#actividad ganadera, principalmente por los procesos fermentativos que ocurren 
#en el rumen de animales poligástricos. Históricamente se ha determinado que la 
#concentración media de metano en el delta del Río Paraná es de 20.78 ppm (partes
#por millón; ppm) con un desvío estándar de 5.1 ppm. Sin embargo, un grupo de 
#investigadores cree que en una región del delta, fuertemente impactada por la 
#actividad ganadera, la concentración de metano es mayor. Para dilucidar esta 
#interrogante estiman la concentración de metano en superficie en 45 muestras 
#tomadas aleatoriamente cuyos resultados se encuentran en el archivo 
#ganadería.txt. Con un alfa del 5%, compruebo la hipótesis del grupo de investigadores.

#Cargo la base de datos
data <-read.table("ganaderia.txt", header = T)

m2 <- c(sample (data$metano, 45, replace = F))

# Ho µ???20.78       Ha  µ>20.78
# Uso Prueba Z de media para una muestra con ?? conocido
# La independencia entre las observaciones se ve asegurada debido a que 
#la concentración de metano medido en una unidad no se ve afectado por la 
#medida en otra unidad.

# Testeo la hipótesis:
#Ho mu=<20.78
#Ha mu>20.78
library(BSDA)
z.test( data$metano , alternative = "greater", mu = 20.78 , sigma.x = 5.1 )
#Como el p valor resultó ser menor al alfa especificado, rechazo la hipótesis alternativa (Ha).
