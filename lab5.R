# Laboratorio 4 Análisis de Datos

# Integrantes:
# Angel Avendaño
# Rodrigo Escobar

normocapnia <- read.table("DY000.txt", encoding="utf8")
hipercapnia <- read.table("DY001.txt", encoding="utf8")

nombres_columnas <- c("PAM", "C02", "VFSC")
colnames(normocapnia) <- nombres_columnas
colnames(hipercapnia) <- nombres_columnas