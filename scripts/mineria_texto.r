# ==============================================================================
# PASO 3: Cargar las librerías necesarias
# ==============================================================================
library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)
library(tm)

# ==============================================================================
# PASO 4: Adquirir la obra (Lectura del archivo)
# ==============================================================================
ruta_archivo <- "datos/obra.txt"

# Leer el archivo línea por línea asegurando que reconozca tildes y eñes (UTF-8)
lineas_raw <- readLines(ruta_archivo, encoding = "UTF-8")

cat("✅ PASO 4 - Líneas totales leídas:", length(lineas_raw), "\n")

# ==============================================================================
# PASO 5: Delimitar el contenido (Excluir encabezado y pie de Gutenberg)
# ==============================================================================
linea_inicio <- grep("\\*\\*\\* START OF THE PROJECT GUTENBERG", lineas_raw, ignore.case = TRUE)
linea_fin    <- grep("\\*\\*\\* END OF THE PROJECT GUTENBERG", lineas_raw, ignore.case = TRUE)

if(length(linea_inicio) > 0 && length(linea_fin) > 0) {
  lineas_obra <- lineas_raw[(linea_inicio + 1):(linea_fin - 1)]
  cat("✅ PASO 5 - Texto delimitado correctamente.\n")
} else {
  lineas_obra <- lineas_raw
  cat("⚠️ PASO 5 - No se encontraron marcadores de Gutenberg, se usará todo el texto.\n")
}

# ==============================================================================
# PASO 6: Unificar el texto
# ==============================================================================
texto_unificado <- paste(lineas_obra, collapse = " ")

# Comprobar la cantidad de caracteres obtenidos (Requisito del informe)
total_caracteres <- nchar(texto_unificado)
cat("✅ PASO 6 - Cantidad total de caracteres:", total_caracteres, "\n")

# ==============================================================================
# PASO 7: Convertir el texto a minúsculas
# ==============================================================================
texto_limpio <- tolower(texto_unificado)
cat("✅ PASO 7 - Texto convertido a minúsculas.\n")

# ==============================================================================
# PASO 8: Eliminar números
# ==============================================================================
texto_limpio <- gsub("[0-9]+", " ", texto_limpio)
cat("✅ PASO 8 - Números eliminados.\n")

# ==============================================================================
# PASO 9: Eliminar signos de puntuación
# ==============================================================================
# [[:punct:]] es un atajo en R para referirse a comas, puntos, exclamaciones, etc.
texto_limpio <- gsub("[[:punct:]]", " ", texto_limpio)
cat("✅ PASO 9 - Signos de puntuación eliminados.\n")

# ==============================================================================
# PASO 10: Eliminar espacios innecesarios
# ==============================================================================
# Reemplazar múltiples espacios juntos por un solo espacio
texto_limpio <- gsub("\\s+", " ", texto_limpio)
# Quitar espacios al inicio o al final del texto completo
texto_limpio <- trimws(texto_limpio)
cat("✅ PASO 10 - Espacios adicionales eliminados.\n")

# Veamos una pequeña muestra de cómo quedó el texto (los primeros 200 caracteres)
cat("\n--- MUESTRA DEL TEXTO LIMPIO ---\n")
cat(substr(texto_limpio, 1, 200), "...\n")


# ==============================================================================
# PASO 11: Separar el texto en palabras
# ==============================================================================
# Usamos 'strsplit' nativo de R para separar por cada espacio en blanco
vector_palabras <- unlist(strsplit(texto_limpio, " "))

# Eliminamos espacios vacíos que hayan quedado por error
vector_palabras <- vector_palabras[vector_palabras != ""]

cat("✅ PASO 11 - Número total de palabras brutas:", length(vector_palabras), "\n")
cat("Primeras 5 palabras:", head(vector_palabras, 5), "\n")
cat("Últimas 5 palabras:", tail(vector_palabras, 5), "\n")

# ==============================================================================
# PASO 12: Identificar y eliminar Stopwords
# ==============================================================================
# ⚠️ Como el texto es la versión en inglés, cargamos las stopwords en inglés
stopwords_en <- tm::stopwords("en")

# Filtramos: Nos quedamos solo con las palabras que NO están en las stopwords
palabras_filtradas <- vector_palabras[!(vector_palabras %in% stopwords_en)]

# Quitamos también palabras muy cortas (de 1 o 2 letras) que suelen ser errores
palabras_filtradas <- palabras_filtradas[nchar(palabras_filtradas) > 2]

cat("✅ PASO 12 - Stopwords eliminadas. Palabras útiles restantes:", length(palabras_filtradas), "\n")

# ==============================================================================
# PASO 13: Revisar la calidad del texto
# ==============================================================================
cat("\n--- PASO 13: MUESTRA DE PALABRAS LIMPIAS (Revisión de calidad) ---\n")
print(head(palabras_filtradas, 20))


# ==============================================================================
# PASOS 14, 15 y 18: Frecuencias, Ordenar y Calcular Proporciones
# ==============================================================================
# Contar cuántas veces aparece cada palabra
tabla_frecuencias <- as.data.frame(table(palabras_filtradas), stringsAsFactors = FALSE)
colnames(tabla_frecuencias) <- c("Palabra", "Frecuencia")

# Ordenar de mayor a menor frecuencia
tabla_frecuencias <- tabla_frecuencias[order(-tabla_frecuencias$Frecuencia), ]
rownames(tabla_frecuencias) <- NULL # Reiniciar el índice para que empiece en 1

# Calcular proporciones y porcentajes
total_palabras <- sum(tabla_frecuencias$Frecuencia)
tabla_frecuencias$Proporcion <- tabla_frecuencias$Frecuencia / total_palabras
tabla_frecuencias$Porcentaje <- round(tabla_frecuencias$Proporcion * 100, 2)

# Agregar la Posición
tabla_frecuencias$Posicion <- 1:nrow(tabla_frecuencias)

# Reordenar las columnas exactamente como pide el taller
tabla_frecuencias <- tabla_frecuencias[, c("Posicion", "Palabra", "Frecuencia", "Proporcion", "Porcentaje")]

# ==============================================================================
# PASOS 16 y 17: Obtener el TOP 20 y TOP 40
# ==============================================================================
top_20 <- head(tabla_frecuencias, 20)
top_40 <- head(tabla_frecuencias, 40)

cat("✅ PASOS 14 al 18 completados. Aquí tienes el TOP 10 de muestra:\n")
print(head(top_20, 10))

# ==============================================================================
# PASOS 19 y 20: Crear visualizaciones y exportarlas (Nativo de R)
# ==============================================================================
# Crear y guardar el Gráfico TOP 20
png("resultados/grafico_top20.png", width = 800, height = 600, res = 100)
par(mar = c(5, 7, 4, 2)) # Ajustar márgenes para que quepan las palabras
barplot(rev(top_20$Frecuencia), names.arg = rev(top_20$Palabra), 
        horiz = TRUE, las = 1, col = "#2b5c8f", 
        main = "Top 20 Palabras más Frecuentes - El Misterio del Tren Azul", 
        xlab = "Frecuencia", border = NA)
dev.off()

# Crear y guardar el Gráfico TOP 40
png("resultados/grafico_top40.png", width = 800, height = 900, res = 100)
par(mar = c(5, 7, 4, 2)) 
barplot(rev(top_40$Frecuencia), names.arg = rev(top_40$Palabra), 
        horiz = TRUE, las = 1, col = "#d95f02", 
        main = "Top 40 Palabras más Frecuentes - El Misterio del Tren Azul", 
        xlab = "Frecuencia", border = NA, cex.names = 0.8)
dev.off()

cat("✅ PASOS 19 y 20 - Gráficos creados y guardados.\n")

# ==============================================================================
# PASO EXTRA: Guardar las tablas Excel/CSV para entregar (Entregables 25)
# ==============================================================================
write.csv(tabla_frecuencias, "resultados/tabla_frecuencias_total.csv", row.names = FALSE)
write.csv(top_20, "resultados/tabla_top20.csv", row.names = FALSE)
write.csv(top_40, "resultados/tabla_top40.csv", row.names = FALSE)

cat("✅ ARCHIVOS LISTOS - Revisa tu carpeta 'resultados'.\n")

