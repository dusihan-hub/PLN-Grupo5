diccionario <- readLines("/Users/howdoudu/Desktop/CDIA_II/PLN/dic_es.txt", encoding = "UTF-8")
ordenada <- function(p) {
  paste(sort(strsplit(p, "")[[1]]), collapse = "")
} #ordeno las letras de la palabra,
#para luego sacar una lista de palabras con las mismas letras
camper <- function(palabra_inicial, diccionario) {
  palabra_actual<- tolower(palabra_inicial)
  n <- nchar(palabra_actual)
  Vn <- diccionario[nchar(diccionario) == n]
  Vu <- c(palabra_actual)
  
  seguir<- TRUE
  letras <- c(letters, "ñ", "á", "é", "í", "ó", "ú")
  
  while (seguir){
    opciones <- c()
    
    permutaciones <- Vn[
      sapply(Vn,ordenada) == ordenada(palabra_actual) &!(Vn %in% Vu)
    ] # regla de permutar
    if (length(permutaciones) > 0) {
      opciones <- c(opciones, permutaciones)
    } #concadenar todas las posibilidades
  
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
    } #regla de cambio de letra
    
    opciones <- unique(opciones)
    
    if(length(opciones) == 0) {
      seguir <- FALSE
    }
    else{
      palabra_actual <- opciones[1]
      Vu <- c(Vu,palabra_actual)
    }
  }
  return(Vu)
}
resultado <- camper("trapo",diccionario)
print(resultado)


