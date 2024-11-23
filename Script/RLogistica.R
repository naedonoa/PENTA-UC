# 1. PREPARACIÓN DEL AMBIENTE DE TRABAJO
# Cargar las bibliotecas necesarias. Recordar que si no tenemos instaladas estas librerías, primero tenemos que instalarlas
library(readxl)      # Para leer archivos Excel
library(ggplot2)     # Para visualizaciones
library(dplyr)       # Para manipulación de datos
library(corrplot)    # Para matrices de correlación
library(car)         # Para diagnósticos adicionales

# 2. CARGA DE DATOS
# Leemos el archivo Excel que contiene nuestros datos 
file.choose()
datos <- read_excel("/Users/noeaedonoa/Desktop/CLASES/Cranial_Measurements.xlsx") #esta corresponde a la ruta de mi computador.


# 3. ANÁLISIS DESCRIPTIVO
print("\
Resumen estadístico de todas las variables:")
print(summary(datos))

# 4. ANÁLISIS DE CORRELACIONES
# Calculamos la matriz de correlación solo para variables numéricas
cor_matrix <- cor(datos[,1:3])
print("\
Matriz de correlaciones entre medidas craneales:")
print(round(cor_matrix, 3))  # Redondeamos a 3 decimales para mejor visualización

# Visualización de la matriz de correlaciones
corrplot(cor_matrix, 
         method="color",      # Modo de visualización
         type="upper",        # Mostrar solo la parte superior
         addCoef.col="black", # Color de los coeficientes
         tl.col="black",      # Color del texto
         tl.srt=45,           # Rotación del texto
         title="Matriz de Correlaciones",
         mar=c(0,0,1,0))      # Márgenes

# 5. VISUALIZACIÓN DE DISTRIBUCIONES
# Crear gráficos de caja (o box plot) para cada medida craneal según la clasificación

# Gráfico para Longitud Craneal
p1 <- ggplot(datos, aes(x=factor(Correct_Classification), 
                        y=Cranial_Length, 
                        fill=factor(Correct_Classification))) +
  geom_boxplot() +
  labs(title="Longitud Craneal por Clasificación",
       x="Clasificación (0=Incorrecta, 1=Correcta)",
       y="Longitud Craneal (mm)") +
  scale_fill_discrete(name="Clasificación", 
                      labels=c("Incorrecta", "Correcta")) +
  theme_minimal()
print(p1)

# Gráfico para Anchura Craneal
p2 <- ggplot(datos, aes(x=factor(Correct_Classification), 
                        y=Cranial_Width, 
                        fill=factor(Correct_Classification))) +
  geom_boxplot() +
  labs(title="Anchura Craneal por Clasificación",
       x="Clasificación (0=Incorrecta, 1=Correcta)",
       y="Anchura Craneal (mm)") +
  scale_fill_discrete(name="Clasificación", 
                      labels=c("Incorrecta", "Correcta")) +
  theme_minimal()
print(p2)

# Gráfico para Altura Facial
p3 <- ggplot(datos, aes(x=factor(Correct_Classification), 
                        y=Facial_Height, 
                        fill=factor(Correct_Classification))) +
  geom_boxplot() +
  labs(title="Altura Facial por Clasificación",
       x="Clasificación (0=Incorrecta, 1=Correcta)",
       y="Altura Facial (mm)") +
  scale_fill_discrete(name="Clasificación", 
                      labels=c("Incorrecta", "Correcta")) +
  theme_minimal()
print(p3)

# 6. MODELO DE REGRESIÓN LOGÍSTICA
# Ajustar el modelo
modelo <- glm(Correct_Classification ~ Cranial_Length + Cranial_Width + Facial_Height,
              data = datos,
              family = binomial())

# Mostrar resumen del modelo
print("\
Resumen del modelo de regresión logística:")
print(summary(modelo))

# Calcular y mostrar Odds Ratios
odds_ratios <- exp(coef(modelo))
print("\
Odds Ratios:")
print(odds_ratios)

# Calcular intervalos de confianza para los Odds Ratios
conf_int <- exp(confint(modelo))
print("\
Intervalos de confianza para Odds Ratios:")
print(conf_int)

# 7. EVALUACIÓN DEL MODELO
# Predicciones del modelo
predicciones <- predict(modelo, type = "response")
clasificacion_predicha <- ifelse(predicciones > 0.5, 1, 0)

# Matriz de confusión
tabla_confusion <- table(Real = datos$Correct_Classification, 
                         Predicha = clasificacion_predicha)
print("\
Matriz de confusión")
print(tabla_confusion)

# Calcular métricas de rendimiento. Este paso final es fundamental, ya que con esto sabremos si nuestro modelo a partir de mediciones creaneales es adecuado o no 
precision <- sum(diag(tabla_confusion)) / sum(tabla_confusion)
print("\
Precisión del modelo:")
print(precision)
