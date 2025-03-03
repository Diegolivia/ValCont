
# Función principal CVC
CVC <- function(data, max, conf.level) {
  # Verificar si el data.frame contiene solo valores numéricos
  if (!all(sapply(data, is.numeric))) {
    stop("El data.frame debe contener solo valores numéricos.")
  }
  
  # Número de jueces (columnas)
  num_jueces <- ncol(data)
  
  # Calcular Pe según la fórmula dada
  Pe <- (1 / num_jueces) ^ num_jueces
  
  # Calcular la media de cada ítem (fila)
  medias_items <- rowMeans(data)
  
  # Calcular el CVC para cada ítem
  CVC_items <- round((medias_items - Pe) / max, 3)
  
  # Calcular los intervalos de confianza de Wilson para cada CVC

  get_wilson_CI <- function(x, n, conf.level) {
    p_hat <- x
    SE_hat_sq <- p_hat * (1 - p_hat) / n
    crit <- qnorm(1 - (1 - conf.level) / 2)
    omega <- n / (n + crit^2)
    A <- p_hat + crit^2 / (2 * n)
    B <- crit * sqrt(SE_hat_sq + crit^2 / (4 * n^2))
    CI <- c('lower' = omega * (A - B), 
            'upper' = omega * (A + B))
    return(CI)
  }
    intervalos_CI <- t(sapply(CVC_items, get_wilson_CI, n = num_jueces, conf.level = conf.level))
  
  # Crear un data.frame con los resultados
  resultado_df <- data.frame(
    Item = 1:nrow(data),
    CVC = round(CVC_items, 3),
    lwr.ci = round(intervalos_CI[, "lower"],3),
    upr.ci = round(intervalos_CI[, "upper"],3)
  )
  return(resultado_df)
}