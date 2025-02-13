CVRcut_Ayres <- function(num_jueces, alpha = 0.05) {
  # Validar número de jueces
  if (num_jueces < 2) {
    stop("El número de jueces debe ser al menos 2 para un cálculo válido.")
  }
  
  # Calcular n_e crítico usando pbinom
  calcular_ne <- function(N, alpha) {
    for (ne in 0:N) {
      # Probabilidad acumulativa para la binomial
      p_cum <- 1 - pbinom(ne - 1, size = N, prob = 0.5)
      if (p_cum <= alpha) {
        return(ne)
      }
    }
    return(NA)  # En caso de error
  }
  
  # Calcular valores críticos
  min_jueces <- calcular_ne(num_jueces, alpha)
  if (!is.na(min_jueces)) {
    cutoff <- (2 * min_jueces - num_jueces) / num_jueces
  } else {
    cutoff <- NA
  }
  
  # Resultado
  result <- data.frame(
    Method = "Ayres",
    Alpha = alpha,
    CritVal = cutoff,
    MinJudges = min_jueces
  )
  
  return(result)
}
