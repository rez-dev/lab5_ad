# Laboratorio 5 Análisis de Datos

# Integrantes:
# Angel Avendaño
# Rodrigo Escobar

normocapnia <- read.table("DY000.txt", encoding="utf8")
hipercapnia <- read.table("DY001.txt", encoding="utf8")

nombres_columnas <- c("PAM", "C02", "VFSC")
colnames(normocapnia) <- nombres_columnas
colnames(hipercapnia) <- nombres_columnas

library("MTS")
library("signal")
library("TSA")
library("oce")
library("ggplot2")


# ###### NORMOCAPNIA ###### 
# # Número total de puntos en la señal
# num_puntos <- length(normocapnia$PAM)
# # Frecuencia de muestreo (Hz)
# frecuencia_muestreo <- 5
# # Crear el vector de tiempo
# tiempo <- seq(0, (num_puntos - 1) / frecuencia_muestreo, by = 1 / frecuencia_muestreo)
# # Graficar la serie de tiempo
# plot(tiempo, normocapnia$PAM, type = "l", xlab = "Tiempo (s)", ylab = "PAM", main = "Serie de tiempo Normocapnia PAM")
# 
# ###### HIPERCAPNIA ######
# num_puntos <- length(normocapnia$PAM)
# # Frecuencia de muestreo (Hz)
# frecuencia_muestreo <- 5
# # Crear el vector de tiempo
# tiempo <- seq(0, (num_puntos - 1) / frecuencia_muestreo, by = 1 / frecuencia_muestreo)
# # Graficar la serie de tiempo
# plot(tiempo, hipercapnia$PAM, type = "l", xlab = "Tiempo (s)", ylab = "PAM", main = "Serie de tiempo Hipercapnia PAM")

###### CORRELACION NORMO ######
correlation_normocapnia <- ccf(normocapnia$PAM, normocapnia$VFSC)
correlation2_normocapnia <- ccf(normocapnia$PAM, normocapnia$VFSC, lag.max = 200)

###### CORRELACION HIPER ######
correlation_hipercapnia <- ccf(hipercapnia$PAM, hipercapnia$VFSC)
correlation2_hipercapnia <- ccf(hipercapnia$PAM, hipercapnia$VFSC, lag.max = 200)

###### AUTOCORRELACION NORMO ######
autocorrelation_normocapnia <- acf(normocapnia$PAM,lag.max = 200)
###### AUTOCORRELACION HIPER ######
autocorrelation_hipercapnia <- acf(hipercapnia$PAM,lag.max = 200)


# ##### OBTENCION FUNCION DE TRANSFERENCIA ######
# Calcular la densidad espectral de potencia cruzada mediante el método de Welch
cross_spect <- pwelch(normocapnia$PAM, normocapnia$VFSC, taper = 0, log = "no")

# Extraer la función de transferencia en el dominio de la frecuencia
transfer_function <- cross_spect$spec

# Graficar la densidad espectral de potencia cruzada
ggplot(data = cross_spect, aes(x = freq, y = spec)) +
  geom_line() +
  labs(title = "Densidad Espectral de Potencia Cruzada entre PAM y VFSC",
       x = "Frecuencia (Hz)", y = "Densidad Espectral de Potencia") +
  theme_minimal()

# Aplicar un escalón inverso de presión
inverted_PAM <- max(normocapnia$PAM) - normocapnia$PAM

# Obtener la respuesta del sistema mediante la convolución
system_response <- convolve(inverted_PAM, transfer_function, type = "open")

# Graficar la respuesta del sistema
ggplot(data = data.frame(time = seq_along(system_response), response = system_response),
       aes(x = time, y = response)) +
  geom_line() +
  labs(title = "Respuesta del Sistema al Escalón Inverso de PAM",
       x = "Tiempo (muestras)", y = "Respuesta") +
  theme_minimal()


# Define la función de transferencia H(s)
# Asegúrate de sustituir los coeficientes y los términos de 's' por los valores correctos de tu sistema.
H <- transfer_function(c(num = c(...), den = c(...)))

# Define la función que representa el escalón inverso de presión
inverse_step <- function(t) {
  ifelse(t < 0, 0, -1)  # Definir la función del escalón inverso de presión
}

# Obtén la señal de entrada del dataframe
input_signal <- hipercapnia$PAM

# Define el tiempo de simulación basado en la longitud de la señal de entrada
time <- seq(0, (length(input_signal) - 1), by = 1)

# Calcula la respuesta del sistema a la señal de entrada
response <- lsim(H, inverse_step, time, input = input_signal)

# Grafica la respuesta del sistema
plot(time, response$y, type = "l", xlab = "Tiempo", ylab = "Respuesta del sistema", main = "Respuesta del sistema al escalón inverso de presión")