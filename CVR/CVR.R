CVR <- function(data) {
  # Transformar las respuestas: 3 -> 1 (essential), otros -> 0
  data_binaria <- as.data.frame(t(apply(data, 1, function(row) {
    ifelse(row == 3, 1, 0)
  })))
  
  # Crear un data.frame para almacenar los resultados
  resultados <- data.frame(
    Item = seq_len(nrow(data_binaria)),
    CVR = numeric(nrow(data_binaria))
  )
  
    
  # Iterar sobre las filas (?tems)
  for (i in seq_len(nrow(data_binaria))) {
    # Contar el n?mero de respuestas "essential" (ne) y total (N)
    ne <- sum(data_binaria[i, ])  # Respuestas 1 ("essential")
    N <- ncol(data_binaria)       # Total de respuestas (jueces)
    
    # Calcular CVR para el ?tem
    CVR <- (ne - (N / 2)) / (N / 2)
    
   
    # Almacenar los resultados
    resultados$CVR[i] <- round(CVR, 3)
    
  }
  
  # Retornar el data.frame con los resultados
  return(resultados)
}
