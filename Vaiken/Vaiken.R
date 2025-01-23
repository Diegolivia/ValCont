Vaiken <- function(data, min, max, conf.level = 0.95) {
  # Validacion de tipos de datos
  if (!is.data.frame(data)) {
    stop("El argumento 'data' debe ser un data.frame.")
  }
  if (!is.numeric(as.matrix(data))) {
    stop("El 'data' debe contener solo valores numericos.")
  }
  
  # Validación de min y max
  if (!is.numeric(min) || !is.numeric(max)) {
    stop("'min' y 'max' deben ser valores numericos.")
  }
  if (min >= max) {
    stop("'min' debe ser menor que 'max'.")
  }
  
  # Verificación del nivel de confianza
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' debe estar entre 0 y 1 (excluyendo los extremos).")
  }
  
  # Verificación de valores NA
  if (any(is.na(data))) {
    stop("El 'data' contiene valores NA. Por favor, limpia los datos antes de usar la funcion.")
  }
  
  # Verificación de que las puntuaciones estén en el rango válido
  if (any(data < min | data > max)) {
    stop("Todas las puntuaciones en 'data' deben estar entre 'min' y 'max'.")
  }
  
  # Número de jueces
  n <- ncol(data)
  
  # Valor crítico de Z para el nivel de confianza
  z <- qnorm(1 - (1 - conf.level) / 2)
  
  # Función para calcular V de Aiken y los intervalos para cada ítem
  calcular_valores <- function(puntajes) {
    M <- mean(puntajes) # Media de las calificaciones
    V <- (M - min) / (max - min) # Cálculo de V de Aiken
    
    # Cálculo de los límites inferior y superior usando el método score de Wilson
    nk <- n * (max - min)
    term1 <- 2 * nk * V + z^2
    term2 <- z * sqrt(4 * nk * V * (1 - V) + z^2)
    denom <- 2 * (nk + z^2)
    
    lwr.ci <- (term1 - term2) / denom
    upr.ci <- (term1 + term2) / denom
    
    return(c(V = V, lwr.ci = lwr.ci, upr.ci = upr.ci))
  }
  
  # Aplicar la función a cada ítem (fila del data frame)
  resultados <- t(apply(data, 1, calcular_valores))
  
  # Crear el data frame final con los resultados
  resultados_df <- data.frame(
    Item = 1:nrow(data),
    V = round(resultados[, "V"], 3),
    lwr.ci = round(resultados[, "lwr.ci"], 3),
    upr.ci = round(resultados[, "upr.ci"], 3)
  )
  
  return(resultados_df)
}