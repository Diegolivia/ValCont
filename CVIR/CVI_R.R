# Función principal para calcular el CVI con ajuste por acuerdo por azar
CVI_R <- function(data, cut, conf.level) {
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
  pctg_vals <- numeric(nrow(data))
  kappa_vals <- numeric(nrow(data))
  kappa_lwr_cis <- numeric(nrow(data))
  kappa_upp_cis <- numeric(nrow(data))
  
  # Calcular porcentaje de acuerdo, kappa de Lynn y CI de Wilson para cada ítem
  for (i in 1:nrow(data)) {
    Na <- sum(data[i, ] >= cut)
    Nna <- num_jueces - Na
    N <- Na + Nna
    
    CVI_val <- Na / N

    # Calcular P_e y kappa de Lynn
    p_e <- (mean(data >= cut))^2
    kappa_vals[i] <- (CVI_val - p_e) / (1 - p_e)
    
    # Calcular intervalo de confianza de Wilson para kappa usando el valor absoluto
    abs_kappa <- abs(kappa_vals[i])
    CI_kappa <- get_wilson_CI(abs_kappa, N, conf.level)
    
    # Si kappa es negativo, devolver el signo negativo a los límites del CI
    if (kappa_vals[i] < 0) {
      kappa_lwr_cis[i] <- -CI_kappa['upper']  # Intercambia el límite superior e inferior
      kappa_upp_cis[i] <- -CI_kappa['lower']
    } else {
      kappa_lwr_cis[i] <- CI_kappa['lower']
      kappa_upp_cis[i] <- CI_kappa['upper']
    }
  }
  
  # Crear un data frame con los resultados y redondear a 3 decimales
  resultado <- data.frame(
    Item = 1:nrow(data),
    cvipb = round(kappa_vals, 3),
    lwr.ci = round(kappa_lwr_cis, 3),
    upr.ci = round(kappa_upp_cis, 3)
  )
  
  return(resultado)
}

