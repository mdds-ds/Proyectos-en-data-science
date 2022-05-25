#cargar base de datos de dietas para colesterol
setwd("C:/Users/Matu/Desktop/Data Science/Proyectos/Peso de ratas y relacion con dietas")
BD_dietas=read.csv("BD_dietas.csv")
# ESTE SÍMBOLO SIGNIFICA DESIGUAL (!=)

#ejecuto la orden para extraer muestra
set.seed(56); m <- rbind(subset(BD_dietas, dieta=="lino")[sample(1000,100),], subset(BD_dietas, dieta=="chia")[sample(1000,100),], subset(BD_dietas, dieta=="girasol")[sample(1000,100),])
BD_dietas$dieta<-as.factor(BD_dietas$dieta)
BD_dietas$nivel<-as.factor(BD_dietas$nivel)
str(m)
# a partir de ahora trabajo con la muestra generada, llamada m

#El test es un ANOVA de un factor (se quiere conocer el efecto de la dieta 
#sobre el cambio de nivel de colesterol en sangre)

# SUPUESTOS:
# Muestras aleatorias e independientes (asumida).
# Observaciones procedentes de poblaciones normales.
# Homocedasticidad entre tratamientos.

library(nlme)
modelo<-aov(cambio_col~dieta, data=m)
resid<-resid(modelo)
pred<-fitted(modelo)
#Pruebo normalidad (no hay):
shapiro.test(resid) # H0: Buen ajuste a normalidad de residuos
                    # Ha: Caso contrario
windows()
par(mfrow=c(1,2))
qqnorm(resid)
qqline(resid,col=2)
#Pruebo homocedasticidad (no hay):
plot(pred,resid)
abline(0,0)
#H0: varianza_i = varianza (Homocedasticidad: todas las varianzas son iguales)
#HA: varianza_i != varianza Al menos una varianza difiere del resto
library(car)
leveneTest(cambio_col~dieta, data=m, center = mean)
#DEBO REFORMULAR EL MODELO PARA QUE CUMPLA SUPUESTOS DE NORMALIDAD Y HOMOCEDASTICIDAD:
#Utilizo gls (MÁXIMA VEROSIMILITUD):
library(nlme)
modeloMV<-gls(cambio_col~dieta, data=m)
#Modelo varianzas:
m2_varIdent<-gls(cambio_col~dieta, weights=varIdent(form=~1|dieta), data = m)
m3_varExp<-gls(cambio_col~dieta, weights=varExp(form=~fitted(.)), data = m)
m4_varPow<-gls(cambio_col~dieta, weights=varPower(form=~fitted(.)), data = m)
#Mediante AIC decido cual es el mejor modelo:
AIC(modeloMV,m2_varIdent,m3_varExp, m4_varPow) #El modelo más adecuado es con VarExp.
m3_varExp<-gls(cambio_col~dieta, weights=varExp(form=~fitted(.)), data = m)
residMV<-resid(m3_varExp, type = "pearson")
predMV<-fitted(m3_varExp)
#Pruebo normalidad y homocedasticidad del modelo:
shapiro.test(residMV) # H0: Buen ajuste a normalidad de residuos
# Ha: Caso contrario
windows()
par(mfrow=c(1,2))
qqnorm(residMV,main="VarExp")
qqline(residMV,col=2)
#Pruebo homocedasticidad (no hay):
plot(predMV,residMV,main="VarExp")
abline(0,0)
#H0: varianza_i = varianza (Homocedasticidad: todas las varianzas son iguales)
#HA: varianza_i != varianza Al menos una varianza difiere del resto
library(car)
leveneTest(residMV, m$dieta, center = mean)
#Los supuestos se cumplen, por lo tanto, trabajo con varianzas modeladas (m3_varExp).
anova(m3_varExp) #Pongo a prueba:
# Ho: mu_i = mu (Rechazo si P-valor es menor a alfa = 0.05 o si F es menor a F crítico)
# Ha: Al menos una mu_i != mu
#Hay evidencias para rechazar Ho (con una confianza del 95%). Biológicamente,
#hay evidencias para afirmar que dieta produce un efecto en el cambio de
#colesterol en sangre de los ratones (la media del colesterol en sangre de al
#menos una dieta difiere del resto).
#Para saber qué tanto difieren, debo hacer una prueba de Tukey:
library(emmeans)
Tukey <- emmeans(m3_varExp, pairwise~dieta, adjust = "tukey")
summary(Tukey)
install.packages("multcompView")
install.packages("multcomp")
library(multcompView)
library(multcomp)
cld(Tukey$emmeans)
confint(Tukey) #Magnitud de las diferencias
plot(Tukey, comparisons = T, horizontal = F) #COMPARACIONES A POSTERIORI, las zonas grises son intervalos de confianza.
#Se puede apreciar que las 3 dietas difieren entre sí y que la dieta con lino
#es la que menor cambio del colesterol en sangre tuvo



# Quiero probar otra hipótesis
#Ho: Las tres dietas tienen frecuencias similares, es decir, 
#las dietas utilizadas son homogéneas respecto al nivel de colesterol en sangre.
#Ha: Caso contrario.
#Es una prueba de homogeneidad (la variable dieta es fija).
tabla<-table(m$dieta,m$nivel)
tabla
# SUPUESTOS DE PRUEBA DE CHI CUADRADO:
# 1) n >= 50 (cumple)
# 2) Todas las fe > 0 (cumple)
# 3) Tener como máximo el 20% de categorías con fe < 5. ()
# ACLARACIÓN: Si no se cumplieran las restricciones 1) o 2), se pueden agrupar categorías.
test <- chisq.test(tabla, correct = F)
test
#Como p-valor es menor a alfa=0.05, tengo evidencias para rechazar (con una
#confianza del 95%) Ho, o sea, que las dietas fueran homogéneas respecto al
#nivel de colesterol en sangre.


#Tengo un problema de ganancia de peso en ratas wistar
#cargo base de datos
BD_pesoratas=read.csv("BD_pesoratas.csv")
#Ejecuto la orden para extraer muestra
library(dplyr)
set.seed(56); m2 <- as.data.frame(BD_pesoratas %>% group_by(GS) %>% sample_n(21) %>%  group_by(GS) %>%  summarise(ganancia_peso = mean(ganancia_peso)))
str(m2)

# a partir de ahora trabajo con la muestra generada, llamada m2
# se trata de un estudio de correlación, ya que se quiere demostrar si existe o no
# relación funcional. En caso de existir, el modelo que la explicaría sería
# una relación lineal.
r<-cor(m2$GS, m2$ganancia_peso)
cor.test(m2$GS, m2$ganancia_peso) #Como p-valor es mucho menor a alfa=0.05, se
#concluye que sí existe una correlación lineal. Aparte como el coeficiente de Pearson
#es casi 1 (r=0.9965312), se puede decir que la relación es directa y casi perfecta.
R_cuad<-r**2

#######Muestras extraídas:
write.csv(m,file="muestra1.csv",row.names = F)
write.csv(m2,file="muestra2.csv",row.names = F)