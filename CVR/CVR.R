#' @title CVR: Content Validity Ratio
#' @description Calculate the CVR coefficient (content validity ratio)
#' @param data dataframe, with the columns assigned to each judge, and the rows assigned to each evaluated item.
#' 
#' @return
#' dataframe with CVR coefficients for all analyzed items
#' 
#' @details
#' The CVR function calculates the content validity ratio (CVR; Lawshe, 1975). CVR function that the items were
#' rated in three categories: Not important (1), useful but not essential (2), essential (3). CVR dichotomizes
#' each rating, and yields the proportion of judges who considered the item essential.
#' Under this model, the usual cut-off point for identifying essential items in the CVR should be located at
#' the highest possible rating, i.e., on a scale of 1 to 3, the cut-off point is 3.
#' 
#' **Note 1**: The function has not yet been prepared to resolve missing values, so the user must remove or impute
#' any missing values.
#' **Note 2**: The dataframe entered must have values between 1 and 3. If not, the user must transform 
#' the values before using this function.
#' 
#' @references
#' #' Lawshe, C. H. (1975). A quantitative approach to content validity. Personnel psychology, 28, 563–575.
#' https://doi.org/10.1111/j.1744-6570.1975.tb01393.x
#' 
#' @examples
#' #' ### Example 1 ----------
#' 
#' ## Random data
#' set.seed(123)
#' randata.CVR <- data.frame(replicate(n = 6,sample(1:3,12,rep=TRUE)))
#' CVR(data = randata.CVR)
#' 
#' ## Run
#' CVR(randata.CVR)
#' 
#' ### Example 2 ----------
#' set.seed(123)
#' randata2.CVR <- data.frame(replicate(n = 6,sample(2:3,12,rep=TRUE)))
#' 
#' ## Run
#' CVR(data = randata2.CVR)
#' 
#' @author
#' Cesar Merino-Soto (\email: {sikayax@yahoo.cam.ar})
#' 
#' @export
CVR <- function(data) {
  # Transformar las respuestas: 3 -> 1 (essential), otros -> 0
  data_binaria <- as.data.frame(t(apply(data, 1, function(row) {
    ifelse(row == 3, 1, 0)
  })))
  
  # Crear un data.frame para almacenar los resultados
  resultados <- data.frame(
    Item = seq_len(nrow(data_binaria)),
    CVR = numeric(nrow(data_binaria))
  )
  
    
  # Iterar sobre las filas (?tems)
  for (i in seq_len(nrow(data_binaria))) {
    # Contar el n?mero de respuestas "essential" (ne) y total (N)
    ne <- sum(data_binaria[i, ])  # Respuestas 1 ("essential")
    N <- ncol(data_binaria)       # Total de respuestas (jueces)
    
    # Calcular CVR para el ?tem
    CVR <- (ne - (N / 2)) / (N / 2)
    
   
    # Almacenar los resultados
    resultados$CVR[i] <- round(CVR, 3)
    
  }
  
  # Retornar el data.frame con los resultados
  return(resultados)
}
