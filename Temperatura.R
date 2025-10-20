##PRIMERO DESCARGAMOS LAS LIBRERIAS NECESARIAS 
install.packages("stats")
install.packages("astsa")
install.packages("forecast")
install.packages("tseries")
install.packages("nortest")
install.packages("fBasics")
install.packages("car")
install.packages("MLmetrics")
install.packages("matrixcalc")
install.packages("aTSA")
install.packages("MASS")
install.packages("TSA")
install.packages("pastecs")
install.packages("fTrading")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("zoo")
install.packages("tidyr")
install.packages("urca")
library(lmtest)
library(urca)
library(matrixcalc)
library(aTSA)
library(MASS)
library(TSA)
library(pastecs)
library(dplyr)
library(stats)
library(astsa)
library(forecast)
library(tseries)
library(nortest)
library(fBasics)
library(car)
library(MLmetrics)
library(fTrading)
library(ggplot2)
library(zoo)
library(lubridate)
library(tidyr)
##COMO SEGUNDO VAMOS A IMPORTAR LOS DATAFRAMES 
Entreno<-(DailyDelhiClimateTrain)
Testeo<-(DailyDelhiClimateTest)

###TRANSFORMEMOS LA SERIE DE MEDIAS DIARIAS A MEDIAS SEMANALES




df_temp_diaria <- DailyDelhiClimateTrain %>%
  mutate(date = seq(as.Date("2013-01-01"), by = "day", length.out = nrow(DailyDelhiClimateTrain)))

# Calcular promedio semanal de la temperatura media
df_temp_semanal <- df_temp_diaria %>%
  mutate(semana = floor_date(date, unit = "week")) %>%
  group_by(semana) %>%
  summarise(mean_temp = mean(meantemp, na.rm = TRUE))

ts_temp_semanal <- ts(df_temp_semanal$mean_temp, 
                      start = c(2013, 1), 
                      frequency = 52)


df_temp_diaria_test <- DailyDelhiClimateTest %>%
  mutate(date = seq(as.Date("2017-01-01"), by = "day", length.out = nrow(DailyDelhiClimateTest)))

# Calcular promedio semanal de la temperatura media
df_temp_semanal_test <- df_temp_diaria_test %>%
  mutate(semana = floor_date(date, unit = "week")) %>%
  group_by(semana) %>%
  summarise(mean_temp = mean(meantemp, na.rm = TRUE))

# Crear objeto ts semanal
ts_temp_semanal_test <- ts(df_temp_semanal_test$mean_temp, 
                           start = c(2017, 1), 
                           frequency = 52)


##LISTO AHORA TENEMOS LA SERIE DE TIEMPO DADA EN TEMPERATURAS SEMANALES VAMOS A COMENZAR CON EL ANALISIS EXPLORATORIO

options(repr.plot.width=14, repr.plot.height=8)
ts.plot(ts_temp_semanal, ylab="Temperatura promedio", main="Serie del promedio de temperatura semanal", lwd=2); grid(col = "black")

##GRAFICAMENTE OBSERVAMOS QUE SE PRESENTA UNA ESTACIONALIDAD CADA ANO APROXIMADAMENTE Y UNA VARIANZA APROXIMADAMENTE 
##CONSTANTE POR LO QUE NO APLICAREMOS UNA TRANSFOMRACION DE BOX COX

##OBSERVEMOS UNA DESCOMPOSICION ADITIVA CON EL FIN DE TENER INDICIOS SOBRE LA ESTACIONALIDAD Y
##LA POSIBLE TENDENCIA DE LA SERIE
descomp <- decompose(ts_temp_semanal)
plot(descomp)

##OBSERVAMOS UNA CLARA ESTACIONALIDAD ADEMAS DE OBSERVAR UNA TENDENCIA A LA ALZA POR PARTE DE LA SERIE, 
##POR LO QUE PODRIAMOS ESPERAR QUE LA TEMPERATURA MEDIA SEMANAL ESTE AUMENTANDO, ESTO PUEDE
##DEVERSE A DIFERENTES FACTORES, COMO POSIBLES DE ELLOS EL CALENTAMIENTO GLOBAL

##REALICEMOS UN TEST DE ESTACIONARIEDAD
df = adf.test(ts_temp_semanal)
df

##LA PRUEBA NOS DA COMO RESULTADO QUE LA SERIE ES ESTACIONARIA  
adf_ur <- ur.df(ts_temp_semanal, type = "trend", lags = 5)

# Ver resultado
summary(adf_ur)

##OBSERVAMOS QUE LA SERIE CUMPLE EL SUPUESTO DE ESTACIONARIDAD MEDIANTE LA PREUBA DE DICKYFUILLER

##AHORA MIREMOS LA PRUEBA DE HOMOGENEIDAD DE VARIANZAS DE LEVEANE, 
segmentos <- cut(1:length(ts_temp_semanal), breaks = 52)

# Prueba de Levene
pl = leveneTest(ts_temp_semanal ~ segmentos)
print(pl)

##OBSERVAMOS QUE RECHAZAMOS H0, POR LO QUE HAY HOMOGENEIDAD DE VARIANZAS DE FORMA ANUAL, LO QUE SE EVIDNECIA
##EN EL GRAFICO PUES HAY UN GRAN PARECIDO EN LA SERIE TODOS LOS ANOS

##BOXPLOTS MENSUALES

fechas <- seq(as.Date("2013-01-01"), by = "week", length.out = length(ts_temp_semanal))
datos <- data.frame(Fecha = fechas, Valor = ts_temp_semanal)
tamaño_ventana <- 4##TOMAR UNA VENTANA MAS ALTA, POR EJEMPLO 30, TRIMESTRAL,SEMESTRAL 
datos <- datos %>%
  mutate(Semana = as.numeric(difftime(Fecha, min(Fecha), units = "weeks")),
         Ventana = ceiling(Semana / tamaño_ventana))

ggplot(datos, aes(x = as.factor(Ventana), y = Valor)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(
    title = "Boxplot por Ventana de Temperaturas promedio mensual",
    x = "Ventana",
    y = "Sales"
  ) +
  theme_minimal()


##AHORA MIREMOS LA ACF Y LA PACF

par(mfrow=c(2,1))
acf(ts_temp_semanal, lag.max=156, main="ACF de la serie ")     # ACF hasta el rezago 48
pacf(ts_temp_semanal, lag.max=156, main="PACF de la serie ") 

##TENEMOS SOSPECHAS DE QUE LA SERIE EN REALIDAD NO ES ESTACIONARIA PUES LA ACF PRESENTA UN DECAIMIENTO LENTO
##APLIQUEMOS UNA PRIMERA DIFERNECIA A LA SERIE
par(mfrow=c(1,1))
ts_temp_semanal_dif<-diff(ts_temp_semanal)
ts.plot(ts_temp_semanal_dif, ylab="Temperatura promedio", main="Serie del promedio de temperatura semanal diferenciada", lwd=2); grid(col = "black")

##AHORA CON LA PRIMERA DIFERENCIA OBSERVAMOS QUE PRESENTA UNA PATRON ESTACIONARIO

##AHORA MIREMOS LA ACF Y PACF DE LA SERIE DIFERENCIADA PARA LOGRAR ESTACIONARIEDAD
par(mfrow=c(2,1))
acf(ts_temp_semanal_dif, lag.max=156, main="ACF de la serie diferenciada ")     # ACF hasta el rezago 48
pacf(ts_temp_semanal_dif, lag.max=156, main="PACF de la serie diferenciada ") 

###AHORA APLIQUEMOS LA DIFERENCIACION ESTACIONAL 
par(mfrow = c(2,1))

# ACF sin eje x automático
acf(diff(ts_temp_semanal_dif, lag = 52), lag.max = 156, main = "ACF aplicando diferencia estacional cada 52 semanas ")
# etiquetas cada 365

# PACF sin eje x automático
pacf(diff(ts_temp_semanal_dif, lag = 52), lag.max = 156, main = "PACF aplicando diferencia estacional cada 52 semanas ")


##OBSERVAMOS UN PICO FUERTE EN EL LAG=1 QUE SERIA 52 SEMANAS, Y UN POSIBLE DEACIMIENTO EXPONENCIAL LA PACF 
##POR LO QUE LE MODELO ELEGIDO PODRIA SER UN MODELO CON UNA MEDIA MOVIL ESTACIONAL CADA 52 SEMANAS

##BUSQUEMOS EL POSIBLE MEJOR MODELO 

library(astsa)

# Definir los órdenes
p <- 1; q <- 1
P <- 1; Q <- 1
d <- 1; D <- 1
S <- 52 # Periodo estacional anual

# Cantidad total de combinaciones
maxfilas <- (p + 1) * (q + 1) * (P + 1) * (Q + 1)

# Matriz para guardar resultados
ic_mod <- matrix(NA, nrow = maxfilas, ncol = 6)
colnames(ic_mod) <- c("p", "q", "P", "Q", "AIC", "BIC")

# Iterar modelos
k <- 1
for (i in 0:p) {
  for (j in 0:q) {
    for (s in 0:P) {
      for (m in 0:Q) {
        cat("Evaluando modelo (", i, d, j, ") x (", s, D, m, ")[", S, "]\n")
        
        ajuste <- tryCatch(
          {
            mod <- sarima(ts_temp_semanal, p = i, d = d, q = j,
                          P = s, D = D, Q = m, S = S,
                          details = FALSE, no.constant = TRUE)
            ic_mod[k, ] <- c(i, j, s, m, mod$AIC, mod$BIC)
          },
          error = function(e) {
            cat("Error con modelo (", i, d, j, ") x (", s, D, m, "): ", e$message, "\n")
            ic_mod[k, ] <- c(i, j, s, m, NA, NA)
          }
        )
        k <- k + 1
      }
    }
  }
}

# Ver resultados ordenados por AIC
ic_mod_ordenado <- ic_mod[order(ic_mod[,5], na.last = NA), ]
print(ic_mod_ordenado)

##OBSERVAMOS GRACIAS A LA FUNCION QUE EL MEJOR MODELO SEGUN EL CRITERIO DEL AIC Y DEL BIC SERA EL MODELO SARIMA (1,1,1)X(0,1,1)[52]
##ALGO MUY RAZONABLE SEGUN VIMOS EN EL PASO A PAS ODE LA ACF Y LA PACF PUES TUVIMOS QUE DIFERENCIAR UNA VEZ
#PARA SER AUTOREGRESIVO Y TUVIMOS QUE REALIZAR UNA DIFERNECIACION ESTACIONAL DE 52 SEMANAS

modelo_seleccionado=sarima(ts_temp_semanal,
                               p = 1, d = 1, q = 1,
                               P = 0, D = 1, Q = 1,
                               S = 52, details = TRUE, no.constant = TRUE)


res_mod <- modelo_seleccionado$fit$residuals
Box.test(res_mod, type = "Ljung-Box")##PRUEBA PARA MIRAR QUE LOS RESIDUALES NO ESTAN CORRELACIONADOS
ajust = ts_temp_semanal-res_mod

# Gráfico para los valores ajustados #
par(mfrow=c(1,1))
ts.plot(ts_temp_semanal, ajust , main = "Valores ajustados")
lines(ts_temp_semanal, col="red")

##OBSERVAMOS A GRANDES RAZGOS QUE LOS RESIDUALES NO ESTAN CORRELACIONADOS Y QUE EL MODELO APUNTA MUY BIEN PUES
##AL MIRAR LOS VALORES AJUSTADOS COMPARADOS CON LA SERIE REAL PARECE HABER UN BUEN COMPORTAMIENTO 

h <- length(ts_temp_semanal_test)
predicciones <- predict(modelo_seleccionado$fit, n.ahead = h)

##COMPARAMOS LAS PREDDICIONES CON LOS VALORES DE TESTEO
ts.plot(ts_temp_semanal, predicciones$pred, col = c("black", "blue"))
lines(ts_temp_semanal_test, col = "red")  # Valores reales en rojo
legend("topleft", legend = c("Entrenamiento", "Predicción", "Real"),
       col = c("black", "blue", "red"), lty = 1)



##OBSERVEMOS EL MAPE
# Asegúrate de que ambas series tengan la misma longitud
longitud <- min(length(predicciones$pred), length(ts_temp_semanal_test))

# Calculo del MAPE
mp <- MAPE(predicciones$pred, ts_temp_semanal_test) * 100
cat("MAPE del modelo (%):", round(mp, 2), "\n")

##AHORA QUE TENEMOS UNA IDEA DE CUAL MODELO ELEGIR, APLICAREMOS LAS VARIABLE EXOGENAS PARA EVALUAR 
##EVALUEMOS COHERENCIA 
##MATRICES DE VARIABLES EXOGENAS
X_train <- as.matrix(Entreno[, c("humidity", "wind_speed", "meanpressure")])
X_test  <- as.matrix(Testeo[,  c("humidity", "wind_speed", "meanpressure")])

##CONVERTIMOS LAS EXOGENAS A VALORES SEMANALES TAMBIEN
Entreno_semanal <- Entreno %>%
  mutate(date = as.Date(date),                     #  convertir a Date
         semana = floor_date(date, "week")) %>%
  group_by(semana) %>%
  summarise(
    humidity = mean(humidity, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE),
    meanpressure = mean(meanpressure, na.rm = TRUE)
  )

# Para Testeo
Testeo_semanal <- Testeo %>%
  mutate(date = as.Date(date),                     #  convertir a Date
         semana = floor_date(date, "week")) %>%
  group_by(semana) %>%
  summarise(
    humidity = mean(humidity, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE),
    meanpressure = mean(meanpressure, na.rm = TRUE)
  )

##SACAMOS LAS SERIES DE TIEMPO RELACIONADAS A LAS VARIABLES EXOGENAS

ts_humedad_semanal<-ts(Entreno_semanal$humidity,start = c(2013, 1), 
                       frequency = 52)

ts_velocidad_viento_semalan<-ts(Entreno_semanal$wind_speed,start = c(2013, 1), 
                                frequency = 52)

ts_presion_media_semanal<-ts(Entreno_semanal$meanpressure,start = c(2013, 1), 
                             frequency = 52)

##REALIZAMOS LOS GRÁFICOS
par(mfrow = c(4, 1), mar = c(4, 4, 2, 1))  # márgenes ajustados

# Graficar cada serie de tiempo
plot(ts_temp_semanal, col = "tomato", lwd = 2, 
     main = "Temperatura semanal", xlab = "Semana", ylab = "°C")

plot(ts_humedad_semanal, col = "steelblue", lwd = 2, 
     main = "Humedad semanal", xlab = "Semana", ylab = "%")

plot(ts_velocidad_viento_semalan, col = "forestgreen", lwd = 2, 
     main = "Velocidad del viento semanal", xlab = "Semana", ylab = "m/s")

plot(ts_presion_media_semanal, col = "darkorange", lwd = 2, 
     main = "Presión media semanal", xlab = "Semana", ylab = "hPa")

par(mfrow = c(3, 1)) 

graficoc_coherencia <- mvspec(cbind(ts_temp_semanal, ts_humedad_semanal), spans = 7, kernel = "daniell")
graficoc_coherencia2 <- mvspec(cbind(ts_temp_semanal, ts_velocidad_viento_semalan), spans = 7, kernel = "daniell")
graficoc_coherencia3 <- mvspec(cbind(ts_temp_semanal, ts_presion_media_semanal), spans = 7, kernel = "daniell")

# Extraer frecuencias y coherencia
frecuencias <- graficoc_coherencia$freq  # Ciclos por intervalo de muestreo
coherencia <- Mod(graficoc_coherencia$coh)^2
periodos_semanales <- 1 / frecuencias
# Asumir datos semanales (ajusta según tu caso)

# Combinar en data.frame
df <- data.frame(
  Periodo_semanas = periodos_semanales,
  Coherencia = coherencia
)


# Ordenar por coherencia descendente
df_ordenado <- df[order(-df$Coherencia), ]

# Ver los principales picos de coherencia
head(df_ordenado, 5)


##OBSERVAMOS EN EL PERIODOGRMA SUAVIZADO QUE TODAS LAS SEIRES PRESENTAN ESTACIONALIAD PUES HAY PICOS EN FRECUENCIAS CORTAS
##ARPOXIMADAMENTE EN 1 QUE HACE ALUCION A 52 SEMANAS, ES DECIR UN ANO 


##RECORDEMOS EL MODELO QUE YA HABIAMOS SELECCIONADO EL CUAL ERA UN MODELO SARIMA Y AHORA LE AGREGAREMOS LAS 
##EXOGENAS, EVALUAREMOS SU AJUSTE Y TAMBIEN EVALUAREMOS LOS VALORES P ASOCIADOS A LA SIGINFICANCIA
##DE LAS EXOGENAS, ADEMAS EVALUAREMOS EL PRONOSTICO Y EL MAPE 

#CONFIGURAMOS LAS MATRICES
Entreno_xreg <- Entreno_semanal %>%
  select(humidity, wind_speed, meanpressure) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

Testeo_xreg <- Testeo_semanal %>%
  select(humidity, wind_speed, meanpressure) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()
##AJUSTAMOS EL MODELO CON LAS EXOGENAS
modelo_exogenas <- Arima(ts_temp_semanal,
                       order = c(1, 1, 1),
                       seasonal = list(order = c(0, 1, 1), period = 52),
                       method = "CSS-ML",xreg =Entreno_xreg )

n_a_predecir <- nrow(Testeo_xreg)

##REALIZAMOS LA PREDICCION CON LAS VARIABLES EXOGENAS
prediccion <- forecast(modelo_exogenas, xreg = Testeo_xreg, h = n_a_predecir)

par(mfrow = c(1, 1)) 

plot(prediccion, main = "Predicción de temperatura media semanal")
lines(ts_temp_semanal_test, col = "red", lwd = 2)
legend("topleft", legend = c("observado", "prediccion"),
       col = c("red", "blue"), lty = 1, lwd = 2)

##GRACFICAMOS LA PREDICCION 



autoplot(prediccion) +
  autolayer(ts_temp_semanal_test, series = "Real", color = "red") +
  ggtitle("Pronóstico ARIMAX vs Observado") +
  theme_minimal()

##AHORA CALCULEMOS EL MAPE 
summary(modelo_exogenas)


coefs <- summary(modelo_exogenas)$coef
print(coefs)

coeftest(modelo_exogenas)

##FORMA DE EVALUR EL BUEN AJUSRTE DEL MODELO CON LAS VARAIBLES EXOGENAS

Box.test(modelo_exogenas$residuals, lag = 20, type = "Ljung-Box")

acf(modelo_exogenas$residuals, main = "ACF de los residuos del modelo ARIMAX")

# PACF
pacf(modelo_exogenas$residuals, main = "PACF de los residuos del modelo ARIMAX")
