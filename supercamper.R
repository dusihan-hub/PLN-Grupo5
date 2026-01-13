set.seed(123)

diccionario <- readLines("dic_es.txt", encoding = "UTF-8")
diccionario <- tolower(trimws(diccionario))
diccionario <- unique(diccionario[diccionario != ""])


ordenada <- function(p) {
  paste(sort(strsplit(p, "")[[1]]), collapse = "")
}

camper <- function(palabra_inicial, diccionario) {
  
  palabra_actual <- tolower(palabra_inicial)
  n <- nchar(palabra_actual)
  
  Vn <- diccionario[nchar(diccionario) == n]
  firmas_Vn <- sapply(Vn, ordenada)
  Vu <- c(palabra_actual)
  
  seguir <- TRUE
  letras <- c(letters, "ñ", "á", "é", "í", "ó", "ú")
  
  while (seguir) {
    
    opciones <- c()
    
    # Regla permutar (anagramas)
    permutaciones <- Vn[
      firmas_Vn == ordenada(palabra_actual) & !(Vn %in% Vu)
    ]
    
    if (length(permutaciones) > 0) {
      opciones <- c(opciones, permutaciones)
    }
    
    # Regla cambio de letra
    palabra_vec <- strsplit(palabra_actual, "")[[1]]
    
    for (i in 1:n) {
      for (l in letras) {
        
        if (l != palabra_vec[i]) {
          
          nueva <- palabra_vec
          nueva[i] <- l
          nueva_palabra <- paste(nueva, collapse = "")
          
          if (nueva_palabra %in% Vn && !(nueva_palabra %in% Vu)) {
            opciones <- c(opciones, nueva_palabra)
          }
        }
      }
    }
    
    opciones <- unique(opciones)
    
    if (length(opciones) == 0) {
      seguir <- FALSE
    } else {
      palabra_actual <- sample(opciones, 1)
      Vu <- c(Vu, palabra_actual)
    }
  }
  
  Vu
}

#supercamper

supercamper <- function(tamano_palabra,
                        num_iteraciones,
                        diccionario,
                        intervalo_grafica = 5) {
  
  palabras <- diccionario[nchar(diccionario) == tamano_palabra]
  
  if (length(palabras) == 0) {
    stop("No hay palabras disponibles para la longitud especificada.")
  }
  
  longitudes <- c()
  palabras_iniciales_usadas <- c()
  
  mejor_secuencia <- c()
  mejor_inicio <- ""
  
  for (i in 1:num_iteraciones) {
    
    disponibles <- setdiff(palabras, palabras_iniciales_usadas)
    
    if (length(disponibles) == 0) {
      stop("Se han utilizado todas las palabras iniciales posibles.")
    }
    
    palabra_inicial <- sample(disponibles, 1)
    palabras_iniciales_usadas <- c(palabras_iniciales_usadas, palabra_inicial)
    
    cat("\nIteración", i, "- Palabra inicial:", palabra_inicial, "\n")
    
    sec <- camper(palabra_inicial, diccionario)
    longitudes <- c(longitudes, length(sec))
    
    cat("Longitud:", length(sec),
        "| Mejor hasta ahora:", max(longitudes), "\n")
    
    if (length(sec) > length(mejor_secuencia)) {
      mejor_secuencia <- sec
      mejor_inicio <- palabra_inicial
    }
  }
  
  # Gráfica (máximo acumulado)
  indices <- seq(0,
                 num_iteraciones,
                 by = intervalo_grafica)
  
  if (tail(indices, 1) != num_iteraciones) {
    indices <- c(indices, num_iteraciones)
  }
  
  longitudes_filtradas <- sapply(indices, function(k) {
    
    if (k == 0) {
      0
    } else {
      max(longitudes[1:k])
    }
    
  })
  
  plot(indices, longitudes_filtradas, type = "o",
       xlab = paste("Iteración (cada", intervalo_grafica, ")"),
       ylab = "Longitud máxima de secuencia",
       main = "Evolución de la longitud máxima de secuencia",
       las = 1)
  
  cat("\nMejor resultado:\n")
  cat("Mejor inicio:", mejor_inicio, "\n")
  cat("Mejor longitud:", length(mejor_secuencia), "\n")
  cat(paste(mejor_secuencia, collapse = " -> "), "\n")
}

#prueba
supercamper(tamano_palabra = 5, num_iteraciones = 10,
          diccionario = diccionario, intervalo_grafica = 1)
