CVRcut.Bag <- function(num_jueces, prior = "Jeffreys", alpha = 0.05, tails = "one") {
  # Validar número de jueces
  if (num_jueces < 2) {
    stop("El número de jueces debe ser al menos 2 para un cálculo válido.")
  }
  
  # Ajustar alpha si es una prueba de dos colas
  if (tails == "two") {
    alpha <- alpha / 2
  } else if (tails != "one") {
    stop("El argumento 'tails' debe ser 'one' o 'two'.")
  }
  
  # Parámetros de los priors
  if (prior == "Jeffreys") {
    a <- 0.5
    b <- 0.5
  } else if (prior == "Uniform") {
    a <- 1
    b <- 1
  } else {
    stop("El prior debe ser 'Jeffreys' o 'Uniform'.")
  }
  
  # Calcular n_e crítico
  calcular_ne <- function(N, alpha, a, b) {
    for (ne in 0:N) {
      # Probabilidad acumulada con pbeta
      p_null <- pbeta(0.5, ne + a, N - ne + b)
      if (p_null < alpha) {
        return(ne)
      }
    }
    return(NA)  # Si no encuentra un valor
  }
  
  min_jueces <- calcular_ne(num_jueces, alpha, a, b)
  
  # Calcular CVR crítico
  if (!is.na(min_jueces)) {
    cutoff <- (2 * min_jueces - num_jueces) / num_jueces
  } else {
    cutoff <- NA
  }
  
  # Resultado
  result <- data.frame(
    Method = "Baghestani",
    Prior = prior,
    Alpha = alpha,
    Tails = tails,
    CritVal = cutoff,
    MinJudges = min_jueces
  )
  
  return(result)
}
