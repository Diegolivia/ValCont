CIR <- function(group1, group2, coef.col, lwr.col, upr.col) {
  # Validaciones iniciales
  if (!all(c(coef.col, lwr.col, upr.col) %in% colnames(group1))) {
    stop("group1 debe contener las columnas especificadas en coef.col, lwr.col y upr.col")
  }
  if (!all(c(coef.col, lwr.col, upr.col) %in% colnames(group2))) {
    stop("group2 debe contener las columnas especificadas en coef.col, lwr.col y upr.col")
  }
  
  # Identificar ítems comunes
  common_items <- intersect(group1$Item, group2$Item)
  
  if (length(common_items) < nrow(group1) || length(common_items) < nrow(group2)) {
    warning("Algunos items no coinciden entre group1 y group2. Se procesaran unicamente los items comunes.")
  }
  
  # Filtrar los data.frames por ítems comunes
  group1 <- group1[group1$Item %in% common_items, ]
  group2 <- group2[group2$Item %in% common_items, ]
  
  # Combinar ambos data.frames por Item
  combined <- merge(group1, group2, by = "Item", suffixes = c("_g1", "_g2"))
  
  # Inicializar las columnas de salida
  results <- data.frame(
    Item = combined$Item,
    R = NA,
    lwr.ci = NA,
    upr.ci = NA
  )
  
  # Calcular R y los intervalos de confianza para cada ítem
  for (i in 1:nrow(combined)) {
    coef_g1 <- combined[[paste0(coef.col, "_g1")]][i]
    coef_g2 <- combined[[paste0(coef.col, "_g2")]][i]
    lwr_g1 <- combined[[paste0(lwr.col, "_g1")]][i]
    upr_g1 <- combined[[paste0(upr.col, "_g1")]][i]
    lwr_g2 <- combined[[paste0(lwr.col, "_g2")]][i]
    upr_g2 <- combined[[paste0(upr.col, "_g2")]][i]
    
    R <- coef_g1 / coef_g2
    var_g1 <- (upr_g1 - lwr_g1)^2 / 4
    var_g2 <- (upr_g2 - lwr_g2)^2 / 4
    lwr.ci <- max(0, R - sqrt(var_g1 + var_g2))
    upr.ci <- R + sqrt(var_g1 + var_g2)
    
    # Guardar los resultados
    results$R[i] <- round(R, 3)
    results$lwr.ci[i] <- round(lwr.ci, 3)
    results$upr.ci[i] <- round(upr.ci,3)
  }
  
  return(results)
}
