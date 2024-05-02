# ========CARGAR LOS DATOS=========

# Cargar el conjunto de datos con los parámetros especificados segun el enunciado
data <- read.csv("C:/Users/Usuario/Desktop/ESTUDIOS  ARCHIVOS/UPC CICLO 5/FUNDAMENTOS DE DATA SCIENCE/hotel_bookings.csv", header = TRUE, stringsAsFactors = FALSE)

#============INSPECCIONAR DATOS============

# Instalar y cargar la biblioteca 'skimr' para resumir los datos
install.packages("skimr")
library(skimr)

# Obtener el resumen de la estructura de los datos
resumen <- skim(data)
resumen

# Ver la estructura de los datos (nombres de las columnas y tipo de datos)
str(data)

# Ver las primeras filas de los datos para obtener una vista previa
head(data)

# Resumen estadístico de las variables numéricas
summary(data)


#===========PRE-PROCESAR LOS DATOS - DATOS FALTANTES(NA)=============

# Verificar datos faltantes (NA)
missing_values <- sum(is.na(data))
cat("Número de datos faltantes (NA):", missing_values, "\n")

# Identificar y mostrar la ubicación de los datos faltantes (NA)
missing_indices <- which(is.na(data), arr.ind = TRUE)
missing_indices

# Identificar columnas con datos faltantes
missing_columns <- colnames(data)[colSums(is.na(data)) > 0]
missing_columns

# Imputar los datos faltantes con la media de cada columna
imputed_values <- list()
for (col in missing_columns) {
  if (is.numeric(data[[col]])) {
    imputed_values[[col]] <- mean(data[[col]], na.rm = TRUE)
    data[[col]][is.na(data[[col]])] <- imputed_values[[col]]
  } else {
    imputed_values[[col]] <- as.character(names(sort(table(data[[col]], exclude = NULL), decreasing = TRUE)[1]))
    data[[col]][is.na(data[[col]])] <- imputed_values[[col]]
  }
}

# Verificar que no haya más datos faltantes
missing_values_after_imputation <- sum(is.na(data))
missing_values_after_imputation
cat("Número de datos faltantes después de la imputación:", missing_values_after_imputation, "\n")

# Imprimir los valores imputados
for (col in missing_columns) {
  cat("Valores imputados para la columna '", col, "': ", imputed_values[[col]], "\n")
}

#===========PRE-PROCESAR LOS DATOS - DATOS ATIPICOS(OUTLIERS)=============
#IDENTIFICACION DE DATOS ATIPICOS CON GRAFICAS:

# Obtener nombres de las variables numéricas
numeric_columns <- colnames(data)[sapply(data, is.numeric)]
numeric_columns

# Generar diagrama de cajas para cada variable numérica
for (col in numeric_columns) {
  boxplot(data[[col]], main=col, ylab=col)
}

#USO DE TECNICA DE REEMPLAZO PARA LOS DATOS ATIPICOS ENCONTRADOS:

# Calcular el promedio o la mediana redondeados de cada columna sin decimales
replacement_values <- sapply(data[numeric_columns], function(x) ifelse(is.numeric(x), round(mean(x, na.rm = TRUE), 0), x))
replacement_values

# Reemplazar los outliers por el promedio o la mediana redondeados
for (col in numeric_columns) {
  if (is.numeric(data[[col]])) {
    outliers <- boxplot.stats(data[[col]])$out
    data[[col]][data[[col]] %in% outliers] <- replacement_values[[col]]
  }
}

#=============VISUALIZAR LOS DATOS=====================
# Instalar y cargar la librería ggplot2 (si aún no está instalada)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Gráfico de dispersión Lead Time  y ADR 
ggplot(data, aes(x = lead_time, y = adr)) 
  geom_point(alpha = 0.5)  
  labs(x = "Lead Time (Tiempo de anticipación)", y = "ADR (Tarifa diaria promedio)")
  ggtitle("Relación entre Lead Time y ADR")
  theme_minimal() 

# Grafido de dispercion Adults y ADR 
ggplot(data, aes(x = adults, y = adr)) +
  geom_point(alpha = 0.5) +
  labs(x = "Número de adultos", y = "ADR (Tarifa diaria promedio)") +
  ggtitle("Relación entre Número de Adultos y ADR") +
  theme_minimal()

# Grafido de dispercion Número total de noches y ADR 
ggplot(data, aes(x = stays_in_week_nights + stays_in_weekend_nights, y = adr)) +
  geom_point(alpha = 0.5) +
  labs(x = "Número total de noches de estadía", y = "ADR (Tarifa diaria promedio)") +
  ggtitle("Relación entre Duración de la Estadía y ADR") +
  theme_minimal()

# Grafido de dispercion Lead Time y Tipo de cliente
ggplot(data, aes(x = lead_time, y = as.numeric(factor(customer_type)))) +
  geom_point(alpha = 0.5) +
  labs(x = "Lead Time (Tiempo de anticipación)", y = "Tipo de Cliente") +
  scale_y_continuous(labels = c("Transient", "Contract", "Transient-Party", "Group")) +
  ggtitle("Relación entre Tipo de Cliente y Lead Time") +
  theme_minimal()

# ==========CONCLUSIONES PRELIMINARES (RESPONDIENDO A LAS PREGUNTAS EN EL CASO DE ANALISIS)======

# PREGUNTA 1: ¿Cuántas reservas se realizan por tipo de hotel? o ¿Qué tipo de hotel prefiere la gente?
# Calcular el número de reservas por tipo de hotel
reservas_por_hotel <- table(data$hotel)

# Crear un gráfico de barras para visualizar el número de reservas por tipo de hotel
barplot(reservas_por_hotel, 
        main = "Número de Reservas por Tipo de Hotel",
        xlab = "Tipo de Hotel",
        ylab = "Número de Reservas",
        col = "skyblue",
        border = "black")


# PREGUNTA 2: ¿Está aumentando la demanda con el tiempo?
# Calcular el número de reservas por año
reservas_por_anio <- table(data$arrival_date_year)

# Crear un gráfico de líneas para visualizar la demanda a lo largo del tiempo
plot(as.numeric(names(reservas_por_anio)), reservas_por_anio, type = "o", 
     main = "Número de Reservas por Año",
     xlab = "Año de Llegada",
     ylab = "Número de Reservas",
     col = "blue",
     pch = 16)


#PREGUNTA 3: ¿Cuándo se producen las temporadas de reservas: alta, media y baja?
# Calcular el número de reservas por mes
reservas_por_mes <- table(data$arrival_date_month)

# Crear un gráfico de barras para visualizar el número de reservas por mes
barplot(reservas_por_mes, 
        main = "Número de Reservas por Mes",
        xlab = "Mes de Llegada",
        ylab = "Número de Reservas",
        col = "skyblue",
        border = "black")

# Calcular los meses con el mayor, mediano y menor número de reservas
mes_alto <- names(sort(reservas_por_mes, decreasing = TRUE)[1])
mes_medio <- names(sort(reservas_por_mes)[ceiling(length(reservas_por_mes)/2)])
mes_bajo <- names(sort(reservas_por_mes)[1])


# PREGUNTAR 4: ¿Cuándo es menor la demanda de reservas?
# Calcular el número de reservas por mes
reservas_por_mes <- table(data$arrival_date_month)

# Crear un gráfico de barras para visualizar el número de reservas por mes
barplot(reservas_por_mes, 
        main = "Número de Reservas por Mes",
        xlab = "Mes de Llegada",
        ylab = "Número de Reservas",
        col = "skyblue",
        border = "black")

# Encontrar el mes con el menor número de reservas
mes_menor_demanda <- names(sort(reservas_por_mes)[1])

# Resaltar el mes con menor demanda de reservas
points(which(names(reservas_por_mes) == mes_menor_demanda), reservas_por_mes[mes_menor_demanda], col = "red", pch = 16)


#PREGUNTA 5: ¿Cuántas reservas incluyen niños y/o bebes?
# Calcular el número de reservas que incluyen niños y/o bebés
reservas_con_ninos <- sum(data$children > 0 | data$babies > 0)
reservas_sin_ninos <- nrow(data) - reservas_con_ninos

# Crear un vector con los datos calculados
datos_reservas <- c(Con_Niños_Bebés = reservas_con_ninos, Sin_Niños_Bebés = reservas_sin_ninos)

# Crear un gráfico de barras para visualizar el número de reservas con y sin niños/bebés
barplot(datos_reservas, 
        main = "Reservas con y sin Niños/Bebés",
        ylab = "Número de Reservas",
        col = c("skyblue", "lightgreen"),
        border = "black")


# PREGUNTA 6: ¿Es importante contar con espacios de estacionamiento?
# Calcular el número de reservas que requieren espacios de estacionamiento
reservas_con_estacionamiento <- sum(data$required_car_parking_spaces > 0)
reservas_sin_estacionamiento <- nrow(data) - reservas_con_estacionamiento

# Crear un vector con los datos calculados
datos_reservas_estacionamiento <- c(Con_Estacionamiento = reservas_con_estacionamiento, Sin_Estacionamiento = reservas_sin_estacionamiento)

# Crear un gráfico de barras para visualizar el número de reservas con y sin espacios de estacionamiento
barplot(datos_reservas_estacionamiento, 
        main = "Reservas con y sin Espacios de Estacionamiento",
        ylab = "Número de Reservas",
        col = c("skyblue", "lightgreen"),
        border = "black")


# PREGUNTA 7: ¿En qué meses del año se producen más cancelaciones de reservas?
# Filtrar el conjunto de datos para incluir solo las reservas canceladas
reservas_canceladas <- data[data$is_canceled == 1, ]

# Calcular el número de cancelaciones por mes
cancelaciones_por_mes <- table(reservas_canceladas$arrival_date_month)

# Crear un gráfico de barras para visualizar el número de cancelaciones por mes
barplot(cancelaciones_por_mes, 
        main = "Cancelaciones de Reservas por Mes",
        xlab = "Mes",
        ylab = "Número de Cancelaciones",
        col = "skyblue",
        border = "black")


#PREGUNTA 8:¿Cuál es la distribución de las tarifas diarias promedio (ADR) por tipo de hotel?
# Crear un boxplot de ADR por tipo de hotel
ggplot(data, aes(x = hotel, y = adr, fill = hotel)) +
  geom_boxplot() +
  labs(title = "Distribución de ADR por Tipo de Hotel",
       x = "Tipo de Hotel",
       y = "ADR") +
  theme_minimal()


#PREGUNTA 9:¿Cuál es la proporción de reservas repetidas en comparación con las reservas no repetidas?
# Calcular el número de reservas repetidas y no repetidas
reservas_repetidas <- sum(data$is_repeated_guest == 1)
reservas_no_repetidas <- nrow(data) - reservas_repetidas

# Calcular la proporción de reservas repetidas en comparación con las reservas no repetidas
proporcion_repetidas <- reservas_repetidas / nrow(data)
proporcion_no_repetidas <- reservas_no_repetidas / nrow(data)

# Crear un vector con las proporciones
proporciones <- c(Repetidas = proporcion_repetidas, No_Repetidas = proporcion_no_repetidas)

# Crear un gráfico de barras para visualizar las proporciones
barplot(proporciones, 
        main = "Proporción de Reservas Repetidas vs. No Repetidas",
        ylab = "Proporción",
        col = c("skyblue", "lightgreen"),
        border = "black")

# =============GUARDAR DATASET FINAL===================
# Guardar el conjunto de datos modificado en un archivo CSV
write.csv(data, file = "datos_modificados.csv", na="NA",row.names=FALSE)
