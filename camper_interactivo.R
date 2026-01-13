# Cargar diccionario
diccionario <- scan("dic_es.txt", what = character(), quiet = TRUE)
diccionario <- tolower(diccionario)

# Filtrar palabras por longitud
filtro_palabras <- function(size) {
  diccionario[nchar(diccionario) == size]
}

# Cambiar exactamente una letra
cambiar_letra <- function(palabra, diccionario) {
  dicc <- diccionario[nchar(diccionario) == nchar(palabra)]
  dicc[
    sapply(dicc, function(p) {
      sum(strsplit(p, "")[[1]] != strsplit(palabra, "")[[1]]) == 1
    })
  ]
}

# Permutar letras
permutar_letras <- function(palabra, diccionario) {
  dicc <- diccionario[nchar(diccionario) == nchar(palabra)]
  dicc[
    sapply(dicc, function(p) {
      identical(
        sort(strsplit(p, "")[[1]]),
        sort(strsplit(palabra, "")[[1]])
      ) && p != palabra
    })
  ]
}

# Jugadas posibles desde una palabra
jugadas_posibles <- function(palabra, diccionario) {
  unique(c(
    cambiar_letra(palabra, diccionario),
    permutar_letras(palabra, diccionario)
  ))
}

# ¿Es palabra perdedora?
es_perdedora <- function(palabra, diccionario) {
  length(jugadas_posibles(palabra, diccionario)) == 0
}

# La máquina intenta jugar óptimamente
mejor_jugada <- function(palabra, diccionario) {
  opciones <- jugadas_posibles(palabra, diccionario)
  
  # Buscar jugada que deje al rival sin movimientos
  for (p in opciones) {
    if (es_perdedora(p, diccionario)) {
      return(p)  # palabra ganadora encontrada
    }
  }
  
  # Si no hay jugada ganadora, jugar cualquiera
  sample(opciones, 1)
}

# Juego interactivo
juego_camper_interactivo <- function(diccionario) {
  
  cat("Reglas del juego:\n")
  cat("1. La palabra debe estar en el diccionario.\n")
  cat("2. Debe cambiarse UNA letra o hacer una permutación.\n")
  cat("3. La longitud debe mantenerse.\n")
  cat("4. Escribe 'S' para salir.\n\n")
  
  palabra_inicial <- tolower(readline("Introduce la palabra inicial: "))
  diccionario_validado <- filtro_palabras(nchar(palabra_inicial))
  
  if (!(palabra_inicial %in% diccionario_validado)) {
    cat("Palabra inicial no válida. Fin del juego.\n")
    return(NULL)
  }
  
  palabra_actual <- palabra_inicial
  cat("Palabra inicial:", palabra_actual, "\n")
  
  while (TRUE) {
    
    # Turno del jugador
    cat("\nTu turno. Palabra actual:", palabra_actual, "\n")
    palabra_usuario <- tolower(readline("Nueva palabra (o 'S' para salir): "))
    
    if (palabra_usuario == "s") {
      cat("Juego terminado por el usuario.\n")
      break
    }
    
    opciones <- jugadas_posibles(palabra_actual, diccionario)
    
    if (!(palabra_usuario %in% opciones)) {
      cat("Movimiento inválido. Fin del juego.\n")
      break
    }
    
    palabra_actual <- palabra_usuario
    
    # Turno de la máquina
    opciones_maquina <- jugadas_posibles(palabra_actual, diccionario)
    
    if (length(opciones_maquina) == 0) {
      cat("La máquina no puede mover. ¡Has ganado!\n")
      break
    }
    
    palabra_maquina <- mejor_jugada(palabra_actual, diccionario)
    cat("La máquina juega:", palabra_maquina, "\n")
    palabra_actual <- palabra_maquina
  }
}

# Ejecutar el juego
juego_camper_interactivo(diccionario)