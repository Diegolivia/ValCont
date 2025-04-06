#' Plot Content Validity Coefficients with Confidence Intervals
#'
#' This function creates an interval plot of content validity coefficients along with their confidence intervals.
#' It allows customization such as filtering selected items, sorting coefficients, and adding a reference line.
#'
#' @param data A `data.frame` containing the coefficients and confidence intervals.
#' @param item.col String. Name of the column containing item identifiers.
#' @param point.coeficient String. Name of the column with the point estimate of the coefficient.
#' @param lwr.ci String. Name of the column with the lower bound of the confidence interval.
#' @param up.ci String. Name of the column with the upper bound of the confidence interval.
#' @param selected.items Optional. A character vector of item names to include in the plot. Default is `NULL` (all items are included).
#' @param x.label String. Title for the x-axis of the graph. Default is `"Items"`.
#' @param y.label String. Title for the y-axis of the graph. Default is `"Coefficient"`.
#' @param title String. Main title for the graph. Default is `"Coefficient Plot"`.
#' @param sort.by.coef Logical. Whether to sort the items by their coefficient value in descending order. Default is `FALSE`.
#' @param reference.line Optional. A numeric value to add a horizontal reference line (e.g., `0.80`). Default is `NULL` (no line).
#' @param rotate.x.labels Logical. Whether to rotate the x-axis labels for better readability. Default is `FALSE`.
#'
#' @return
#' A `ggplot2` object displaying an interval plot of the content validity coefficients and their confidence intervals for the selected items.
#'
#' @details
#' The visualization of results can be more interpretable than a table of numerical values (Tufte, 2001, 2022).
#' This function produces interval plots for the content validity coefficients and their resulting confidence intervals.
#' However, for content validity analysis, tabulated numerical results and accompanying graphs may be a good choice (Hink, Wogalter, & Eustace, 1996).
#'
#' These values are usually obtained from one of the functions of the `ValCont` package.
#' However, this function can also be used with any `data.frame` that includes point estimates and the corresponding lower and upper confidence limits.
#'
#' **Note**: The function has not yet been prepared to resolve missing values, so the user must remove or impute any `NA`s before plotting.
#'
#' @references
#' Hink, J. K., Wogalter, M. S., & Eustace, J. K. (1996). Display of Quantitative Information: Are Grables better than Plain Graphs or Tables? *Proceedings of the Human Factors and Ergonomics Society Annual Meeting*, 40(23), 1155–1159. https://doi.org/10.1177/154193129604002302
#'
#' Tufte, E. R. (2001). *The Visual Display of Quantitative Information*. Cheshire, CT: Graphics Press.
#'
#' Tufte, E. R. (2022). *Seeing with Fresh Eyes: Meaning, Space, Data, Truth*. Cheshire, CT: Graphics Press LLC.
#'
#' @import ggplot2
#' 
#' @examples
#' ### Example 1 ----------
#' 
#' \dontrun{
#' df <- data.frame(
#'   Item = paste0("Item", 1:5),
#'   Coef = c(0.60, 0.72, 0.55, 0.80, 0.49),
#'   CI_low = c(0.45, 0.60, 0.40, 0.65, 0.30),
#'   CI_up = c(0.75, 0.85, 0.70, 0.95, 0.68)
#' )
#'
#' plot_coefficients(
#'   data = df,
#'   item.col = "Item",
#'   point.coeficient = "Coef",
#'   lwr.ci = "CI_low",
#'   up.ci = "CI_up",
#'   reference.line = 0.80,
#'   sort.by.coef = TRUE,
#'   rotate.x.labels = TRUE
#' )
#' }
#' 
### Example 2 ----------
#'\dontrun{
#'  randata.CVIR <- data.frame(replicate(n = 6,sample(1:7,8,rep=TRUE)))
#'  output.CVIR <- CVIR(data = randata.CVIR, cut = 5, conf.level = .90)
#'  CVplot(data = output.CVIR, 
#'item.col = "Item", 
#'point.coeficient = "CVIR", 
#'lwr.ci = "lwr.ci",
#'up.ci = "upr.ci",
#'sort.by.coef = TRUE, 
#'reference.line = .80)
#'}
#' 
#' @export
CVplot <- function(data, item.col, point.coeficient, lwr.ci, up.ci, 
                              selected.items = NULL, 
                              x.label = "Items", y.label = "Coefficient", 
                              title = "Coefficient Plot",
                              sort.by.coef = FALSE,
                              reference.line = NULL,
                              rotate.x.labels = FALSE) {
  
  required_cols <- c(item.col, point.coeficient, lwr.ci, up.ci)
  if (!all(required_cols %in% colnames(data))) {
    stop("The data.frame does not contain all required columns.")
  }
  
  if (!is.null(selected.items)) {
    data <- data[data[[item.col]] %in% selected.items, ]
    if (nrow(data) == 0) {
      stop("None of the selected items are found in the data.")
    }
  }
  
  if (sort.by.coef) {
    data[[item.col]] <- factor(data[[item.col]], 
                               levels = data[[item.col]][order(data[[point.coeficient]], decreasing = TRUE)])
  } else {
    data[[item.col]] <- factor(data[[item.col]], levels = unique(data[[item.col]]), ordered = TRUE)
  }
  
  p <- ggplot(data, aes_string(x = item.col, y = point.coeficient)) +
    geom_errorbar(aes_string(ymin = lwr.ci, ymax = up.ci), width = 0.2) +
    geom_point(size = 3) +
    labs(title = title, x = x.label, y = y.label) +
    theme_minimal()
  
  if (!is.null(reference.line)) {
    p <- p + geom_hline(yintercept = reference.line, linetype = "dashed", color = "red")
  }
  
  if (rotate.x.labels) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  return(p)
}
