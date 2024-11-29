# ------------ Regresión logística. Ejercicio 2 ------------
# ------------ NOEMÍ AEDO NOA -----------------

# PASO 1: Cargar librerías y datos
library(tidyverse)  # Para manipulación de datos y visualización
library(caret)      # Para partición de datos y evaluación
library(readxl)     # Para leer el archivo Excel
library(pROC)       # Para la curva ROC

# Cargar los datos desde el archivo Excel
datos <- read_excel("~/Desktop/penta uc/traumas_oseos_chile.xlsx")

# Revisar las primeras filas del conjunto de datos
head(datos)

# PASO 2: Verificar valores faltantes
colSums(is.na(datos))  # Verificar si hay valores faltantes en las columnas

# Eliminar filas con valores faltantes, si fuese necesario
datos_ejercicio <- na.omit(datos_ejercicio)  # Elimina las filas con valores faltantes

# PASO 3: Estadísticas descriptivas
summary(datos) # Nos entregará una serie de valores
                # Deben anotar únicamente los valores que les solicito en la guía de ejercicios

# COMENTARIO: Si se dan cuenta, en este ejercicio, a diferencia del ejercicio 1, nos pasamos directo a la división de la muestra. 
# Omitimos el paso en donde vemos gráficamente la relación entre las variables predictoras y la variable objetivo.
# Si ustedes desean, pueden hacer igualmente ese paso, tomando el código de ejercicio 1 y reemplazandolo por nuestras variables de este ejercicio. Sin embargo, no es obligatorio.

# PASO 4: Crear particiones de entrenamiento y prueba (70/30)
set.seed(123)  # Fijar semilla para reproducibilidad
trainIndex <- createDataPartition(datos$trauma_oseo, p = 0.7, list = FALSE)
datos_entrenamiento <- datos[trainIndex, ]
datos_prueba <- datos[-trainIndex, ]

# PASO 5: Ajustar el modelo de regresión logística. Por favor verificar que estén todas las variables en el modelo.
modelo <- glm(
  trauma_oseo ~ zona_geografica + violencia_reportada + tiempo_detencion +
    presencia_tortura + acceso_medico + indice_pobreza + presencia_militar,
  data = datos_entrenamiento,
  family = binomial
)

# Resumen del modelo. En este paso obtendremos los valores correspondientes a los coeficientes que deben anotar en la tabla 2.
# estimado, error estándar, valor-z y valor-p
summary(modelo)

# PASO 6: Evaluar el modelo en el conjunto de prueba
# Predecir probabilidades
predicciones <- predict(modelo, newdata = datos_prueba, type = "response")

# Clasificar según un umbral (0.5)
clases_predichas <- ifelse(predicciones > 0.5, 1, 0)
# En regresión logística, el modelo calcula una probabilidad para cada observación que va de 0 a 1. 
# El umbral de 0.5 divide estas probabilidades en dos clases:
# 1. Si la probabilidad predicha es mayor o igual a 0.5, se clasifica como 1 (positivo).
# 2. Si la probabilidad predicha es menor a 0.5, se clasifica como 0 (negativo).

# Construir la matriz de confusión
# Ojo que aquí el código se construyó de una manera distinta a como se hizo para el primer ejercicio.
# Ambos van a entregar los mismos valores, solo que de este modo es más detallado. 
# En cambio en el ejercicio 1, nos enfocamos en obtener de inmediato los valores de las tres métricas que necesitamos.
conf_matrix <- confusionMatrix(as.factor(clases_predichas), as.factor(datos_prueba$trauma_oseo))

# Mostrar métricas de desempeño
print(conf_matrix)

# PASO 7: Visualizar impacto de las variables predictoras
# Extraer coeficientes del modelo
coeficientes <- summary(modelo)$coefficients
coef_data <- as.data.frame(coeficientes)
coef_data$Variable <- rownames(coef_data)

# Graficar coeficientes
ggplot(coef_data[-1, ], aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(
    title = "Impacto de las variables predictoras en los traumas óseos",
    x = "Variable",
    y = "Coeficiente"
  ) +
  theme_minimal()
