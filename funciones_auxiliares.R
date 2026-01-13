# Funciones auxiliares

permutar_letras <- function(palabra, diccionario) {
  permutaciones <- generar_permutaciones(palabra)
  permutaciones_validas <- permutaciones[permutaciones %in% diccionario & permutaciones != palabra]
  return(permutaciones_validas)
}


filtro_palabras <- function(size) {
  diccionario[which(nchar(diccionario) == size)]
}


generar_permutaciones <- function(palabra) {
  letras <- strsplit(palabra, NULL)[[1]]
  if (length(letras) == 1) {
    return(letras)
  }
  permutaciones <- c()
  for (i in seq_along(letras)) {
    letra_actual <- letras[i]
    resto <- letras[-i]
    sub_permutaciones <- generar_permutaciones(paste(resto, collapse = ""))
    for (perm in sub_permutaciones) {
      permutaciones <- c(permutaciones, paste0(letra_actual, perm))
    }
  }
  return(unique(permutaciones))
}


cambiar_letra <- function(palabra, diccionario) {
  letras <- letters  # Alfabeto
  palabras_validas <- c()
  
  for (i in 1:nchar(palabra)) {
    for (letra in letras) {
      nueva_palabra <- substr_replace(palabra, letra, i, i)
      if (nueva_palabra %in% diccionario && nueva_palabra != palabra) {
        palabras_validas <- c(palabras_validas, nueva_palabra)
      }
    }
  }
  return(unique(palabras_validas))
}

substr_replace <- function(string, replacement, start, end) {
  paste0(substr(string, 1, start - 1), replacement, substr(string, end + 1, nchar(string)))
}