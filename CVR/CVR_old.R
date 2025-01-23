CVR <- function(data, conf.level = 0.95) {
  # Transformar las respuestas: 3 -> 1 (essential), otros -> 0
  data_binaria <- as.data.frame(t(apply(data, 1, function(row) {
    ifelse(row == 3, 1, 0)
  })))
  
  # Crear un data.frame para almacenar los resultados
  resultados <- data.frame(
    Item = seq_len(nrow(data_binaria)),
    CVR = numeric(nrow(data_binaria)),
    lwr.ci = numeric(nrow(data_binaria)),
    upr.ci = numeric(nrow(data_binaria))
  )
  
  # Función interna para calcular intervalo de confianza (Wilson)
  get_wilson_CI <- function(x, n, conf.level) {
    p_hat <- x / n  # Proporción (ne / N)
    SE_hat_sq <- p_hat * (1 - p_hat) / n
    crit <- qnorm(1 - (1 - conf.level) / 2)
    omega <- n / (n + crit^2)
    A <- p_hat + crit^2 / (2 * n)
    B <- crit * sqrt(SE_hat_sq + crit^2 / (4 * n^2))
    CI <- c('lower' = omega * (A - B), 
            'upper' = omega * (A + B))
    return(CI)
  }
  
  # Iterar sobre las filas (ítems)
  for (i in seq_len(nrow(data_binaria))) {
    # Contar el número de respuestas "essential" (ne) y total (N)
    ne <- sum(data_binaria[i, ])  # Respuestas 1 ("essential")
    N <- ncol(data_binaria)       # Total de respuestas (jueces)
    
    # Calcular CVR para el ítem
    CVR <- (ne - (N / 2)) / (N / 2)
    
    # Calcular intervalo de confianza
    CI <- get_wilson_CI(ne, N, conf.level)
    
    # Transformar los límites al marco del CVR
    lwr.ci <- (CI['lower'] - 0.5) / 0.5
    upp.ci <- (CI['upper'] - 0.5) / 0.5
    
    # Almacenar los resultados
    resultados$CVR[i] <- round(CVR, 3)
    resultados$lwr.ci[i] <- round(lwr.ci,3)
    resultados$upr.ci[i] <- round(upp.ci,3)
  }
  
  # Retornar el data.frame con los resultados
  return(resultados)
}