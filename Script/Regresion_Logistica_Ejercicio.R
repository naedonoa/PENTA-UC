# ------------ Regresión logística. Ejercicio 1 ------------
# ------------ NOEMÍ AEDO NOA -----------------

# PASO 1: Cargar librerías y datos
# Cargamos las librerías necesarias para trabajar con los datos
library(tidyverse)  # Para manipulación de datos y visualización
library(caret)      # Para crear particiones de los datos y evaluación de modelos
library(pROC)       # Para la curva ROC (no utilizada aquí, pero útil para evaluación)
library(readxl)     # Para leer archivos Excel
library(ggplot2)    # Para visualización de datos

# Cargar los datos desde un archivo Excel
datos_ejercicio <- read_excel("~/Desktop/penta uc/D_Forzado.xlsx")  # Carga el archivo de Excel y lo asigna a 'datos_ejercicio'

# PASO 2: Revisar valores faltantes
# Verificar si hay valores faltantes en el conjunto de datos
colSums(is.na(datos_ejercicio))  # Muestra la cantidad de valores faltantes por columna

# Eliminar filas con valores faltantes, si es necesario
datos_ejercicio <- na.omit(datos_ejercicio)  # Elimina las filas con valores faltantes

# PASO 3: Estadísticos descriptivos
# Mostrar los estadísticos descriptivos básicos para todas las variables
summary(datos_ejercicio)  # Incluye Min, Max, Median y Mean de cada variable

# PASO 4: Relación entre variables predictoras y la variable objetivo
# Graficar la relación entre "numero_ataques" y "desplazamiento_forzado"

ggplot(datos_ejercicio, aes(x = numero_ataques, y = desplazamiento_forzado)) +
  geom_point(color = "lightblue", size = 3) +  # Cambié el color a lightblue
  labs(
    title = "Relación entre ataques y desplazamiento",  # Título del gráfico
    x = "Número de ataques",  # Etiqueta del eje X
    y = "Desplazamiento forzado"  # Etiqueta del eje Y
  ) +
  theme_minimal() +  # Usamos un tema minimalista para mejor visualización
  theme(
    text = element_text(family = "Times New Roman", size = 14),  # Fuente Times New Roman, tamaño de texto 14
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),  # Título centrado, tamaño 16, en negro
    axis.title = element_text(size = 14),  # Títulos de los ejes con tamaño 14
    axis.text = element_text(size = 12),  # Texto de los ejes con tamaño 12
    plot.background = element_rect(fill = "white"),  # Fondo blanco para el gráfico
    panel.grid.major = element_line(color = "grey", size = 0.5)  # Líneas de la cuadrícula mayores en gris
  )

# PASO 5: División de la muestra en entrenamiento y prueba
# Dividir el conjunto de datos en dos partes: 70% para entrenamiento y 30% para prueba
set.seed(123)  # Fijar la semilla para que los resultados sean reproducibles
trainIndex <- createDataPartition(datos_ejercicio$desplazamiento_forzado, p = 0.7, list = FALSE)  # Crear partición
datos_entrenamiento <- datos_ejercicio[trainIndex, ]  # 70% de los datos para entrenamiento
datos_prueba <- datos_ejercicio[-trainIndex, ]        # 30% de los datos para prueba

# PASO 6: Ajustar el modelo de regresión logística
# Ajustar el modelo usando las variables predictoras y la variable objetivo "desplazamiento_forzado"
modelo_regresión <- glm(desplazamiento_forzado ~ numero_ataques + poblacion_desplazada + 
                          acceso_servicios + nivel_destruccion + indice_pobreza + presencia_militar, 
                        data = datos_entrenamiento, 
                        family = binomial)  # La familia binomial es para problemas de clasificación binaria (0 o 1)

# PASO 7: Obtener el resumen del modelo
# Obtener el resumen del modelo ajustado, que muestra los coeficientes y otros detalles importantes
summary(modelo_regresión)  # Muestra los coeficientes del modelo, significancia, etc.

# PASO 8: Evaluar el modelo y construir la matriz de confusión
# Predecir probabilidades para los datos de prueba usando el modelo ajustado
predicciones <- predict(modelo_regresión, newdata = datos_prueba, type = "response")  # 'type = "response"' nos da las probabilidades

# Clasificar según el umbral (en este caso 0.5) para asignar una clase (0 o 1)
clases_predichas <- ifelse(predicciones > 0.5, 1, 0)  # Si la probabilidad es mayor que 0.5, asignamos 1 (desplazamiento), si no 0

# Construir la matriz de confusión para comparar las predicciones con los valores reales
conf_matrix <- confusionMatrix(as.factor(clases_predichas), as.factor(datos_prueba$desplazamiento_forzado))

# Extraer y mostrar las métricas de desempeño del modelo
sensibilidad <- conf_matrix$byClass['Sensitivity']  # Sensibilidad o recall: porcentaje de positivos correctamente identificados
especificidad <- conf_matrix$byClass['Specificity']  # Especificidad: porcentaje de negativos correctamente identificados
precisión <- conf_matrix$overall['Accuracy']  # Precisión: porcentaje total de predicciones correctas

# Mostrar las métricas
print(paste("Sensibilidad:", sensibilidad))  # Sensibilidad
print(paste("Especificidad:", especificidad))  # Especificidad
print(paste("Precisión:", precisión))  # Precisión

# PASO 9: Visualizar impacto de las variables
# Extraer los coeficientes del modelo ajustado
coeficientes <- summary(modelo_regresión)$coefficients  # Obtener los coeficientes del modelo
coef_data <- as.data.frame(coeficientes)  # Convertir los coeficientes a un dataframe para visualización

# Crear un gráfico de barras para visualizar el impacto de las variables predictoras
ggplot(coef_data, aes(x = rownames(coef_data), y = Estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +  # Cambié los colores a lightblue 
  coord_flip() +  # Voltear las barras para mejor legibilidad
  labs(
    title = "Influencia de variables en el desplazamiento",  # Título del gráfico
    x = "Variable",  # Etiqueta del eje X
    y = "Coeficiente"  # Etiqueta del eje Y
  ) +
  theme_minimal() +  # Usamos un tema minimalista para mejorar la visualización
  theme(
    text = element_text(family = "Times New Roman", size = 14),  # Fuente Times New Roman, tamaño de texto 14
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),  # Título centrado, tamaño 16, color negro
    axis.title = element_text(size = 14),  # Títulos de los ejes con tamaño 14
    axis.text = element_text(size = 12),  # Texto de los ejes con tamaño 12
    plot.background = element_rect(fill = "white"),  # Fondo blanco para el gráfico
    panel.grid.major = element_line(color = "grey", size = 0.5)  # Líneas de la cuadrícula en gris
  )


