CVRcut_Wilson <- function(num_jueces, alpha = 0.05, tails = "one") {
  # Validar número de jueces
  if (num_jueces < 2) {
    stop("El número de jueces debe ser al menos 2 para un cálculo válido.")
  }
  
  # Determinar z_alpha según el tipo de prueba
  if (tails == "one") {
    z_alpha <- qnorm(1 - alpha)  # Una cola
  } else if (tails == "two") {
    z_alpha <- qnorm(1 - alpha / 2)  # Dos colas
  } else {
    stop("El argumento 'tails' debe ser 'one' o 'two'.")
  }
  
  # Calcular CVR crítico y número mínimo de jueces
  cutoff <- z_alpha / sqrt(num_jueces)
  min_jueces <- ceiling(z_alpha * sqrt(num_jueces / 2) + num_jueces / 2)
  
  # Resultado
  result <- data.frame(
    Método = "Wilson",
    Alpha = alpha,
    Tails = tails,
    Cutoff = cutoff,
    MinJueces = min_jueces
  )
  
  return(result)
}
