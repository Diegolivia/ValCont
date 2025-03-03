# Función principal para calcular el CVI con ajuste por chance agreement
CVIR <- function(data, cut, conf.level) {
  
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
  
  # Función interna para calcular P_C usando la fórmula combinatoria
  calculate_pc <- function(N, A) {
    if (A > N || A < 0) {
      return(0) # Evita errores si A es mayor que N o menor que 0
    }
    # Aplicamos la fórmula combinatoria
    Pc <- (factorial(N) / (factorial(A) * factorial(N - A))) * (0.5^N)
    return(Pc)
  }
  
  # Verificar si los datos son numéricos
  if (!all(sapply(data, is.numeric))) {
    stop("Todas las columnas deben contener datos numéricos.")
  }
  
  # Número total de jueces
  num_jueces <- ncol(data)
  
  # Inicializar listas para almacenar resultados
  cvipb_vals <- numeric(nrow(data))
  lwr_cis <- numeric(nrow(data))
  upr_cis <- numeric(nrow(data))
  
  # Calcular CVI ajustado para cada ítem
  for (i in 1:nrow(data)) {
    Na <- sum(data[i, ] >= cut)  # Jueces en acuerdo
    Nna <- num_jueces - Na       # Jueces en desacuerdo
    N <- Na + Nna                # Total de jueces
    
    CVI_val <- Na / N
    
    # Calcular P_C usando la nueva fórmula combinatoria dentro de la función
    P_C <- calculate_pc(N, Na)
    
    # Calcular kappa de Lynn
    cvipb_vals[i] <- (CVI_val - P_C) / (1 - P_C)
    
    # Calcular intervalo de confianza de Wilson para kappa usando el valor absoluto
    abs_kappa <- abs(cvipb_vals[i])
    CI_kappa <- get_wilson_CI(abs_kappa, N, conf.level)
    
    # Si kappa es negativo, devolver el signo negativo a los límites del CI
    if (!is.na(cvipb_vals[i]) && cvipb_vals[i] < 0) {
      lwr_cis[i] <- -CI_kappa['upper']  # Intercambia el límite superior e inferior
      upr_cis[i] <- -CI_kappa['lower']
    } else {
      lwr_cis[i] <- CI_kappa['lower']
      upr_cis[i] <- CI_kappa['upper']
    }
  }
  
  # Crear un data frame con los resultados y redondear a 3 decimales
  resultado <- data.frame(
    Item = 1:nrow(data),
    CVIR = round(cvipb_vals, 3),
    lwr.ci = round(lwr_cis, 3),
    upr.ci = round(upr_cis, 3)
  )
  
  return(resultado)
}
