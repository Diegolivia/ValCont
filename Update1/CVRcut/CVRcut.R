CVRcut <- function(N_min, N_max, method, alpha = 0.05, tails = "one", prior = "Jeffreys", interpolacion = "none") {
  # Validar método
  if (!method %in% c("Lawshe", "Wilson", "Ayres", "Baghestani")) {
    stop("Método no válido. Use 'Lawshe', 'Wilson', 'Ayres' o 'Baghestani'.")
  }
  
  # Validar rango de N
  if (N_min >= N_max) {
    stop("'N_min' debe ser menor que 'N_max'.")
  }
  
  # Crear un data.frame vacío para los resultados
  resultados <- data.frame()
  
  # Iterar sobre el rango de N
  for (N in N_min:N_max) {
    if (method == "Lawshe") {
      res <- CVRcut_Lawshe(N, interpolacion = interpolacion, legend = FALSE)
    } else if (method == "Wilson") {
      res <- CVRcut_Wilson(N, alpha = alpha, tails = tails)
    } else if (method == "Ayres") {
      res <- CVRcut_Ayres(N)
    } else if (method == "Baghestani") {
      res <- CVRcut_Baghestani(N, prior = prior, alpha = alpha)
    }
    res$N <- N  # Agregar columna con el número de jueces
    resultados <- rbind(resultados, res)  # Combinar resultados
  }
  
  # Retornar tabla completa
  return(resultados)
}


