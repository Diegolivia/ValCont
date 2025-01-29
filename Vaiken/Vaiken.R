Vaiken <- function(data, min, max, conf.level = 0.95) {
  # Validation: data
  if (!is.data.frame(data)) {
    stop("El argumento 'data' debe ser un data.frame.")
  }
  if (!is.numeric(as.matrix(data))) {
    stop("El 'data' debe contener solo valores numericos.")
  }
  
  # Validation: min, max
  if (!is.numeric(min) || !is.numeric(max)) {
    stop("'min' y 'max' deben ser valores numericos.")
  }
  if (min >= max) {
    stop("'min' debe ser menor que 'max'.")
  }
  
  # Validation: confidence level
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' debe estar entre 0 y 1 (excluyendo los extremos).")
  }
  
  # Verification: NA
  if (any(is.na(data))) {
    stop("'data' contiene valores NA. Por favor, limpiar los datos antes de usar la funcion.")
  }
  
  # Verification: valid range of data
  if (any(data < min | data > max)) {
    stop("Todas las puntuaciones en 'data' deben estar entre 'min' y 'max'.")
  }
  
  # N judges
  n <- ncol(data)
  
  # Critic value Z for confidence level
  z <- qnorm(1 - (1 - conf.level) / 2)
  
  # Calculate V Aiken and confidence interval
  calcular_valores <- function(puntajes) {
    M <- mean(puntajes) # Media de las calificaciones
    V <- (M - min) / (max - min) # C?lculo de V de Aiken
    
    # lower and upper limits, method Wilson's score
    nk <- n * (max - min)
    term1 <- 2 * nk * V + z^2
    term2 <- z * sqrt(4 * nk * V * (1 - V) + z^2)
    denom <- 2 * (nk + z^2)
    
    lwr.ci <- (term1 - term2) / denom
    upr.ci <- (term1 + term2) / denom
    
    return(c(V = V, lwr.ci = lwr.ci, upr.ci = upr.ci))
  }
  
  # Aiken V for every item (row in the data frame)
  resultados <- t(apply(data, 1, calcular_valores))
  
  # Data frame for the output
  resultados_df <- data.frame(
    Item = 1:nrow(data),
    V = round(resultados[, "V"], 3),
    lwr.ci = round(resultados[, "lwr.ci"], 3),
    upr.ci = round(resultados[, "upr.ci"], 3)
  )
  
  return(resultados_df)
}
