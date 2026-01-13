options(expressions = 50000)
source("funciones_auxiliares.R")
diccionario <- readLines("dic_es.txt", encoding = "UTF-8")

mejor_anillo_encontrado <- list()
todos_anillos <- list() 
max_longitud <- 0

# La funcion es una funcion recursiva que intenta imitar una busqueda en profundidad
encontrar_anillos <- function(palabra_act, historial, profundidad_max) {
  # Si superamos la profundidad maxima paramos
  if (length(historial) >= profundidad_max) {
    return()
  }
  
  # Obtener opciones posibles
  opc_perm <- permutar_letras(palabra_act, diccionario)
  opc_cambio <- cambiar_letra(palabra_act, diccionario)
  opciones <- unique(c(opc_cambio, opc_perm))
  
  for (sig_palabra in opciones) {
    if (sig_palabra %in% historial) { #Si la siguiente palabra está em el historial significa que hay un anillo

      inicio_idx <- which(historial == sig_palabra)[1]
      anillo_actual <- historial[inicio_idx:length(historial)]
      longitud_anillo <- length(anillo_actual)
      palabra_inicio <- historial[inicio_idx]
      
      # Crear clave para la longitud (convertir a string para usar como nombre de lista)
      longitud_key <- as.character(longitud_anillo)
      
      # Verificar si ya existe un anillo de esta palabra con esta longitud
      ya_existe <- FALSE
      if (!is.null(todos_anillos[[palabra_inicio]])) {
        if (!is.null(todos_anillos[[palabra_inicio]][[longitud_key]])) {
          ya_existe <- TRUE
        }
      }
      
      # Solo guardar y mostrar si no existe ya un anillo de esta palabra con esta longitud
      if (!ya_existe) {
        if (is.null(todos_anillos[[palabra_inicio]])) {
          todos_anillos[[palabra_inicio]] <<- list()
        }
        
        # Guardamos el anillo
        todos_anillos[[palabra_inicio]][[longitud_key]] <<- anillo_actual
        cat("Anillo encontrado para la palabra ", palabra_inicio, " con una longitud de ", longitud_anillo, "\n")
        
        # Si es el más largo que hemos visto, lo guardamos como el anillo más grande
        if (longitud_anillo > max_longitud) {
          max_longitud <<- longitud_anillo
          mejor_anillo_encontrado <<- anillo_actual
          cat("\rNuevo anillo máximo hallado. Palabra: ", palabra_inicio, " Longitud", max_longitud, "\n")
        }
      }
    } else {
      # Si la palabra no está en el historial seguimos investingando
      encontrar_anillos(sig_palabra, c(historial, sig_palabra), profundidad_max)
    }
    
  }
}

# Función principal que prepara la búsqueda
buscar_anillos_y_maximo <- function(palabra_ini, profundidad_max = 20) {

  mejor_anillo_encontrado <<- list()
  todos_anillos <<- list()
  max_longitud <<- 0
  
  print(paste("Palabra incial", palabra_ini, "con profundidad máx:", profundidad_max))
  
  # Llamada inicial
  encontrar_anillos(palabra_ini, c(palabra_ini), profundidad_max)
  
  if (max_longitud == 0) {
    print("No se encontró ningún anillo en esta rama")
    return(NULL)
  }
  
  cat("\n\nResultado\n")
  cat("Anillo más grande encontrado:", paste(mejor_anillo_encontrado, collapse = " -> "), "\n")
  cat("Longitud:", max_longitud, "\n")
  
  cat("\nTodos los anillos encontrados\n")
  for (palabra in names(todos_anillos)) {
    for (longitud in names(todos_anillos[[palabra]])) {
      anillo <- todos_anillos[[palabra]][[longitud]]
      cat("Palabra:", palabra, " Longitud:", longitud, " Anillo:", paste(anillo, collapse = " -> "), "\n")
    }
  }
  
  return(mejor_anillo_encontrado)
}

#Ejecutar programa
tamano_palabra <- as.integer(readline(prompt = "Introduce la longitud de las palabras: "))
if (is.na(tamano_palabra)) { tamano_palabra <- 2 }

palabras <- filtro_palabras(tamano_palabra)
if (length(palabras) == 0) { stop("No hay palabras de esa longitud.") }

palabra_ini <- readline(prompt = "Introduce la palabra inicial (o Enter para aleatoria): ")
if (palabra_ini == "") {
  palabra_ini <- sample(palabras, 1)
}

# Ejecución
resultado <- buscar_anillos_y_maximo(palabra_ini, profundidad_max = 10000)
