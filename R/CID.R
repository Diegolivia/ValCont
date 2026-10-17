#'@title Confidence interval for difference of content validity coefficients
#'@description Calculates confidence interval for the difference of content validity
#'  coefficients (MOVER; Zou & Donner, 2008) and, optionally, the standardized
#'  difference via Cohen's h (arcsine transformation).
#'
#'@param group1 Output dataframe 1 of one of the 'ValCont' functions.
#'@param group2 Output dataframe 2 of one of the 'ValCont' functions.
#'@param coef.col Name of the column storing the coefficient.
#'@param lwr.col Name of the column storing the lower CI bound.
#'@param upr.col Name of the column storing the upper CI bound.
#'@param n1 Optional. Number of judges in group 1 (single positive number).
#'  Required, together with \code{n2}, to compute the CI of Cohen's h.
#'@param n2 Optional. Number of judges in group 2 (single positive number).
#'@param conf.level Confidence level for the CI of Cohen's h (default .95).
#'  Should match the level used to build the input CIs (e.g., .90 or .95).
#'  Ignored when \code{n1} or \code{n2} are \code{NULL}.
#'@param na.rm Logical. If FALSE (default) the function stops when missing
#'  values are detected in the required columns. If TRUE, those rows are removed.
#'
#'@return A data.frame with columns:
#'  \item{Delta}{Difference between the coefficients (group1 - group2).}
#'  \item{lwr.ci, upr.ci}{MOVER CI for Delta.}
#'  \item{h}{Cohen's h (standardized difference, arcsine scale).}
#'  \item{h.lwr, h.upr}{CI for h. Only present if \code{n1} and \code{n2} are supplied.}
#'  Row names preserve the item labels.
#'
#'@details
#'\code{CID} uses the Method of Variance Estimates Recovery (MOVER; Zou & Donner, 2008)
#'for the difference of two content validity coefficients.
#'
#'When \code{n1} and \code{n2} are supplied, the function also returns Cohen's h
#'(Cohen, 1988), the arcsine-transformed standardized difference:
#'\deqn{h = 2\arcsin(\sqrt{p_1}) - 2\arcsin(\sqrt{p_2})}
#'with variance approximated by \eqn{1/n_1 + 1/n_2}. The factor 2 makes the
#'effect size comparable to Cohen's benchmarks (0.2 small, 0.5 medium, 0.8 large).
#'Cohen's h is especially appropriate for proportions near 0 or 1, where the
#'raw difference and its CI become unstable. The benchmarks are generic;
#'interpret them in the substantive context of the instrument.
#'
#'@references
#'Cohen, J. (1988). \emph{Statistical power analysis for the behavioral sciences}
#'(2nd ed.). Lawrence Erlbaum.
#'
#'Merino-Soto, C. (2018). Confidence interval for difference between coefficients
#'of content validity (Aiken's V): a SPSS syntax. \emph{Anales de Psicologia, 34}(3),
#'587-590. \doi{10.6018/analesps.34.3.283481}
#'
#'Merino-Soto, C. (2023). Coeficientes V de Aiken: diferencias en los juicios de
#'validez de contenido. \emph{MHSalud, 20}(1), 23-32. \doi{10.15359/mhs.20-1.3}
#'
#'Zou, G.Y., & Donner, A. (2008). Construction of confidence limits about effect
#'measures: A general approach. \emph{Statistics in Medicine, 27}, 1693-1702.
#'\doi{10.1002/sim.3095}
#'
#'@author Cesar Merino-Soto (\email{sikayax@yahoo.cam.ar})
#'
#'@seealso \code{\link[ratesci:moverci]{ratesci::moverci}}
#'
#'@examples
#'Vgroup1 <- data.frame(
#'  V       = c(0.85, 0.78, 0.90),
#'  lwr.ci  = c(0.80, 0.75, 0.88),
#'  upr.ci  = c(0.90, 0.82, 0.92)
#')
#'rownames(Vgroup1) <- c("Item1", "Item2", "Item3")
#'
#'Vgroup2 <- data.frame(
#'  V       = c(0.80, 0.75),
#'  lwr.ci  = c(0.76, 0.70),
#'  upr.ci  = c(0.84, 0.78)
#')
#'rownames(Vgroup2) <- c("Item1", "Item2")
#'
#'## Without n: Delta + h (point estimate only)
#'CID(Vgroup1, Vgroup2, "V", "lwr.ci", "upr.ci")
#'
#'## With n: Delta + h + CI for h
#'CID(Vgroup1, Vgroup2, "V", "lwr.ci", "upr.ci", n1 = 10, n2 = 12, conf.level = .90)
#'
#'@export
CID <- function(group1, group2,
                coef.col = "coef", lwr.col = "lwr.ci", upr.col = "upr.ci",
                n1 = NULL, n2 = NULL, conf.level = .95, na.rm = FALSE) {

  # ---- 1. Basic validation ----
  if (!is.data.frame(group1) || !is.data.frame(group2)) {
    stop("Both 'group1' and 'group2' must be data.frames.")
  }

  required_cols <- c(coef.col, lwr.col, upr.col)
  miss_g1 <- setdiff(required_cols, colnames(group1))
  miss_g2 <- setdiff(required_cols, colnames(group2))
  if (length(miss_g1) > 0) {
    stop("'group1' is missing required columns: ", paste(miss_g1, collapse = ", "))
  }
  if (length(miss_g2) > 0) {
    stop("'group2' is missing required columns: ", paste(miss_g2, collapse = ", "))
  }

  # ---- 2. Validate n1 / n2 ----
  has_n1 <- !is.null(n1)
  has_n2 <- !is.null(n2)
  if (xor(has_n1, has_n2)) {
    stop("Provide both 'n1' and 'n2', or neither.")
  }
  compute_h_ci <- has_n1 && has_n2

  if (compute_h_ci) {
    if (!is.numeric(n1) || length(n1) != 1 || is.na(n1) || n1 <= 0) {
      stop("'n1' must be a single positive number.")
    }
    if (!is.numeric(n2) || length(n2) != 1 || is.na(n2) || n2 <= 0) {
      stop("'n2' must be a single positive number.")
    }
    if (!is.numeric(conf.level) || length(conf.level) != 1 ||
        conf.level <= 0 || conf.level >= 1) {
      stop("'conf.level' must be a number between 0 and 1 (exclusive).")
    }
  }

  # ---- 3. Missing values (only in required columns) ----
  na_g1 <- any(is.na(group1[, required_cols, drop = FALSE]))
  na_g2 <- any(is.na(group2[, required_cols, drop = FALSE]))

  if (na_g1 || na_g2) {
    if (!na.rm) {
      stop("Missing values detected. Use na.omit() first, or set na.rm = TRUE.")
    }
    group1 <- group1[complete.cases(group1[, required_cols, drop = FALSE]), , drop = FALSE]
    group2 <- group2[complete.cases(group2[, required_cols, drop = FALSE]), , drop = FALSE]
    warning("Rows with missing values in the required columns were removed.")
  }

  # ---- 4. Merge by item label (rownames) ----
  combined_data <- merge(
    group1, group2,
    by = "row.names",
    suffixes = c("_1", "_2")
  )
  rownames(combined_data) <- combined_data$Row.names
  combined_data$Row.names <- NULL

  if (nrow(combined_data) < nrow(group1) || nrow(combined_data) < nrow(group2)) {
    warning("Some items do not match across groups and were excluded.")
  }

  # ---- 5. MOVER CI for Delta ----
  coef_1 <- paste0(coef.col, "_1")
  coef_2 <- paste0(coef.col, "_2")
  lwr_1  <- paste0(lwr.col, "_1")
  upr_1  <- paste0(upr.col, "_1")
  lwr_2  <- paste0(lwr.col, "_2")
  upr_2  <- paste0(upr.col, "_2")

  combined_data$Delta <- round(
    combined_data[[coef_1]] - combined_data[[coef_2]], 3)

  combined_data$lwr.ci <- round(
    combined_data$Delta -
      sqrt((combined_data[[coef_1]] - combined_data[[lwr_1]])^2 +
             (combined_data[[upr_2]]  - combined_data[[coef_2]])^2),
    3)

  combined_data$upr.ci <- round(
    combined_data$Delta +
      sqrt((combined_data[[upr_1]]  - combined_data[[coef_1]])^2 +
             (combined_data[[coef_2]] - combined_data[[lwr_2]])^2),
    3)

  # ---- 6. Cohen's h (point estimate, always) ----
  p1 <- pmin(pmax(combined_data[[coef_1]], 0), 1)
  p2 <- pmin(pmax(combined_data[[coef_2]], 0), 1)
  h_raw <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
  combined_data$h <- round(h_raw, 3)

  # ---- 7. CI for Cohen's h (optional) ----
  if (compute_h_ci) {
    z   <- qnorm(1 - (1 - conf.level) / 2)
    se  <- sqrt(1 / n1 + 1 / n2)
    combined_data$h.lwr <- round(h_raw - z * se, 3)
    combined_data$h.upr <- round(h_raw + z * se, 3)
    out_cols <- c("Delta", "lwr.ci", "upr.ci", "h", "h.lwr", "h.upr")
  } else {
    out_cols <- c("Delta", "lwr.ci", "upr.ci", "h")
  }

  # Preserve item labels as rownames (consistent with the documented return)
  return(combined_data[, out_cols, drop = FALSE])
}
