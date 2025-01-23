
Haiken <- function(data, ncat, conf.level) {
  
  sum_missing <- rowSums(is.na(data))
  n_subj <- nrow(data) - sum_missing
  
  # Número de filas
  num_filas <- nrow(data)
  
  # Calcular j según si ncol(data) es par o impar
  j <- ifelse(ncol(data) %% 2 == 0, 0, 1)
  
  # data frame para almacenar resultados
  resultados <- data.frame(
    Item = 1:num_filas,
    H = numeric(num_filas),
    lwr.ci = numeric(num_filas),
    upr.ci = numeric(num_filas),
    n.subj = numeric(num_filas)
  )
  
  # Calcular las diferencias por pares (diferencias absolutas) y los intervalos de confianza para cada fila
  for (i in 1:num_filas) {
    fila_actual <- data[i, ]
    
    # Calcular las diferencias por pares como diferencias absolutas y desempaquetarlas
    diff_abs <- combn(colnames(fila_actual), 2, function(pair) {
      col1 <- fila_actual[pair[1]]
      col2 <- fila_actual[pair[2]]
      diff_abs <- abs(col1 - col2)
      return(diff_abs)
    }, simplify = FALSE)  # Utilizar simplify = FALSE para obtener una lista
    
    # Desempaquetar las diferencias absolutas de la lista
    diff_abs_unlist <- unlist(diff_abs)
    
    # Calcular el resultado para la fila actual usando ncat, diff_abs_unlist y j
    resultado_actual <- 1 - (4 * sum(diff_abs_unlist) / ((ncat - 1) * (ncol(data)^2) - j))
    
    get_wilson_CI <- function(x, n, conf.level) {
      n <- n
      p_hat <- x
      SE_hat_sq <- p_hat * (1 - p_hat) / n
      crit <- qnorm(1 - conf.level / 2)
      omega <- n / (n + crit^2)
      A <- p_hat + crit^2 / (2 * n)
      B <- crit * sqrt(SE_hat_sq + crit^2 / (4 * n^2))
      CI <- c('lower' = omega * (A - B), 
              'upper' = omega * (A + B))
      return(CI)
    }
    # Calcular el intervalo de confianza con la función get_wilson_CI y el nivel de confianza alpha
    wilson_interval <- get_wilson_CI(resultado_actual, ncol(data), conf.level)
    
    # Almacenar los resultados en el data frame
    resultados[i, "H"] <- resultado_actual
    resultados[i, "lwr.ci"] <- wilson_interval['lower']
    resultados[i, "upr.ci"] <- wilson_interval['upper']
    resultados[i, "n.subj"] <- num_filas
    
  }
  
  return(round(resultados, 3))
}

