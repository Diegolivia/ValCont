CVI <- function(data, cut, conf.level) {
  # Función interna para calcular el intervalo de confianza de Wilson
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
  
  # Verificar si los datos son numéricos
  if (!all(sapply(data, is.numeric))) {
    stop("Todas las columnas deben contener datos numéricos.")
  }
  
  # Número total de jueces
  num_jueces <- ncol(data)
  
  # Inicializar listas para almacenar resultados
  CVI_vals <- numeric(nrow(data))
  lwr_cis <- numeric(nrow(data))
  upp_cis <- numeric(nrow(data))
  
  # Calcular CVI, porcentaje de acuerdo y CI de Wilson para cada ítem
  for (i in 1:nrow(data)) {
    Na <- sum(data[i, ] >= cut)
    Nna <- num_jueces - Na
    N <- Na + Nna
    
    CVI_vals[i] <- Na / N
      
    # Calcular intervalo de confianza de Wilson
    CI <- get_wilson_CI(CVI_vals[i], N, conf.level)
    lwr_cis[i] <- CI['lower']
    upp_cis[i] <- CI['upper']
  }
  
  # Crear un data frame con los resultados y redondear a 3 decimales
  resultado <- data.frame(
    Item = 1:nrow(data),
    CVI = round(CVI_vals, 3),
    lwr.ci = round(lwr_cis, 3),
    upr.ci = round(upp_cis, 3)
  )
  
  return(resultado)
}

