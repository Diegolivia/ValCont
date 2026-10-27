# Helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Robinson's A Agreement Coefficient (Generalized and Diagnostic)
#'
#' Computes Robinson's (1957) agreement coefficient A, generalized to K judges
#' via its linear relation to the intraclass correlation coefficient for
#' absolute agreement, ICC(A,1). Optionally returns bootstrap confidence
#' intervals and leave-one-out (LOO) diagnostics by judge and by item.
#'
#' @details
#' Robinson (1957) defined agreement as *correlation without regression*:
#' perfect agreement has only one form (X1 = X2), whereas correlation allows
#' any linear (or functional) adjustment. His coefficient is:
#'
#'     A = 1 - D / D.max
#'
#' where D is the total within-family sum of squares (disagreement) and
#' D.max is the sum of squares of all observations around the grand mean.
#' A = 1 indicates perfect agreement; A = 0 indicates maximum disagreement.
#'
#' For K > 2 judges, Robinson (Eq. 9-10) showed that A relates linearly to the
#' intraclass correlation r.i:
#'
#'     r.i = (K * A - 1) / (K - 1)   <=>   A = ((K - 1) / K) * r.i + 1 / K
#'
#' This function uses ICC(A,1) -- the absolute-agreement form of the two-way
#' random-effects intraclass correlation (McGraw & Wong, 1996) -- because
#' Robinson's A penalizes both mean (level) and variance (scale) differences
#' between judges. Consistency-only forms (e.g., ICC(C,1)) would lose this
#' property and are therefore not appropriate here.
#'
#' For K = 2, ICC(A,1) = 2A - 1, recovering Robinson's Eq. (7).
#'
#' @section Bootstrap resampling scheme:
#' Three bootstrap procedures are implemented, each resampling the unit that
#' defines the relevant population of generalization:
#'
#'   \itemize{
#'     \item CI for A (global): resamples items (rows). The population of
#'       interest is the universe of items from which the observed items
#'       were drawn.
#'     \item CI for Diff.A at the judge level: resamples items (rows). Each
#'       judge's LOO effect is generalized across the item universe, holding
#'       the set of judges fixed (the judges are the actual raters, not a
#'       random sample).
#'     \item CI for Diff.A at the item level: resamples judges (columns, via
#'       transposition). Each item's LOO effect is generalized across the
#'       universe of raters, holding the set of items fixed.
#'   }
#'
#' @section Leave-One-Out (LOO) diagnostics:
#' The LOO strategy re-estimates A after removing each judge (column) or each
#' item (row). A positive `Diff.A` (A.omit - Am) indicates that omitting that
#' judge/item *increases* agreement, flagging it as a source of disagreement.
#' `Ratio = A.omit / Am` is also reported for interpretability. Judge-level
#' LOO is usually the most informative in content-validity applications, since
#' it reflects each judge's rating behavior across items; item-level LOO
#' complements it by flagging items with low inter-judge agreement.
#'
#' @section Why the CI on Diff.A is exploratory:
#' The bootstrap interval around `Diff.A` is reported as an *exploratory*
#' uncertainty band, not as a formal hypothesis test, for three reasons:
#'
#'   \itemize{
#'     \item A.omit and Am share N-1 items (or K-1 judges), so the two
#'       estimates are positively correlated by construction. The bootstrap
#'       does capture this dependence (both quantities are recomputed on the
#'       *same* resampled dataset), but the resulting interval is not
#'       centered under any well-defined null hypothesis.
#'     \item No null distribution is available for "judge j is exchangeable
#'       with the other judges" or "item i is exchangeable with the other
#'       items". A formal test would require a permutation scheme (e.g.,
#'       randomly permuting judge labels within items) that is outside the
#'       scope of this function.
#'     \item The interval should therefore be read as "how much does the LOO
#'       effect fluctuate under resampling?", not as "is this judge/item
#'       significantly different from the rest?".
#'   }
#'
#' @param data Data frame or matrix with subjects/items in rows and judges in
#'   columns.
#' @param ci Logical. Compute bootstrap confidence intervals? Default FALSE.
#' @param ci.type Bootstrap CI type: "perc", "bca", or "norm".
#' @param LOO Character. LOO diagnostic: "none", "judges", "items", or "both".
#' @param B Number of bootstrap replications. Default 500.
#'
#' @return A list with:
#'   \item{global}{Single-row data frame (row name "A") with columns
#'     Estimate, lwr.ci, upr.ci. CI columns are NA when ci = FALSE.}
#'   \item{diagnostics.judges}{(if requested) LOO by judge with columns
#'     Judge.Omit, A.omit, Diff.A, lwr.ci, upr.ci, Ratio.}
#'   \item{diagnostics.items}{(if requested) LOO by item with columns
#'     Item.Omit, A.omit, Diff.A, lwr.ci, upr.ci, Ratio.}
#'
#' @references
#' Robinson, W. S. (1957). The statistical measurement of agreement.
#'   *American Sociological Review*, 22(1), 17-25.
#'   \doi{10.2307/2088760}
#'
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#'   intraclass correlation coefficients. *Psychological Methods*, 1(1), 30-46.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' mat <- matrix(sample(1:5, 40, replace = TRUE), nrow = 8, ncol = 5)
#' RobinsonA(mat, LOO = "both", ci = TRUE, B = 200)
#' }
#'
RobinsonA <- function(data, ci = FALSE, ci.type = "perc",
                               LOO = "none", B = 500) {

  # ---- 0. Input validation ------------------------------------------------
  data <- as.matrix(na.omit(data))  # listwise deletion for ANOVA consistency
  n <- nrow(data)
  m <- ncol(data)
  if (n < 2 || m < 2) stop("At least 2 items and 2 judges are required.")

  # ---- 1. Internal: Robinson's generalized A (A_m) ------------------------
  # Implements Robinson (1957) Eq. 10:
  #     A = ((K - 1) / K) * r.i + 1 / K
  # where r.i is the intraclass correlation for absolute agreement, ICC(A,1).
  calc.Am <- function(mat) {
    N <- nrow(mat)
    K <- ncol(mat)

    # --- Two-way ANOVA decomposition (Robinson, 1957, k-variable case) ---
    grand.mean <- mean(mat)

    # D.max: SS of all K*N observations around the grand mean (Eq. 4)
    SS.total   <- sum((mat - grand.mean)^2)

    # Between-subject SS: K * sum of squared subject-mean deviations
    row.means   <- rowMeans(mat)
    SS.subjects <- K * sum((row.means - grand.mean)^2)
    MS.subjects <- SS.subjects / (N - 1)

    # Between-judge SS: N * sum of squared judge-mean deviations.
    # This is the component that makes A penalize LEVEL differences
    # between judges (Robinson's key conceptual point, pp. 21-22).
    col.means   <- colMeans(mat)
    SS.judges   <- N * sum((col.means - grand.mean)^2)
    MS.judges   <- SS.judges / (K - 1)

    # Residual SS / MS (judge x subject interaction confounded with error)
    SS.error  <- SS.total - SS.subjects - SS.judges
    MS.error  <- SS.error / ((N - 1) * (K - 1))

    # --- ICC(A,1): two-way random effects, absolute agreement ---
    # Numerator: estimated true between-subject variance component
    icc.num <- MS.subjects - MS.error
    # Denominator includes judge-related terms:
    #   (K - 1) * MS.error                -> interaction / error
    #   (K / N) * (MS.judges - MS.error)  -> judge LEVEL differences
    # The judge-level term is what makes A penalize mean differences,
    # consistent with Robinson's Eq. (8) decomposition.
    icc.den <- MS.subjects + (K - 1) * MS.error +
      (K / N) * (MS.judges - MS.error)

    icc.a1 <- if (icc.den == 0) 0 else icc.num / icc.den
    # Theoretical range of r.i for K judges: [-1 / (K - 1), 1]
    # (Robinson, 1957, p. 22). Clip to protect against numerical drift.
    icc.a1 <- max(-1 / (K - 1), min(1, icc.a1))

    # --- Transform to Robinson's A (Eq. 10) ---
    # For K = 2 this reduces to (1 + r.i) / 2, recovering Eq. (7).
    Am <- ((K - 1) / K) * icc.a1 + 1 / K
    return(Am)
  }

  # ---- 2. Global estimate + CI (merged into a single row) ----------------
  Am <- calc.Am(data)

  lwr.ci <- upr.ci <- NA_real_
  if (ci) {
    if (!requireNamespace("boot", quietly = TRUE)) {
      warning("Package 'boot' is required for CIs. ",
              "Install with install.packages('boot').")
    } else {
      # Resample rows (items): population of generalization = item universe.
      boot.wrapper <- function(d, idx) calc.Am(d[idx, , drop = FALSE])
      boot.obj <- boot::boot(data = data, statistic = boot.wrapper, R = B)

      b.type <- switch(ci.type, "perc" = "perc", "bca" = "bca",
                       "norm" = "norm", "perc")
      boot.ci.res <- tryCatch({
        b.ci <- boot::boot.ci(boot.obj, type = b.type)
        if (b.type == "norm")      c(b.ci$normal[2],  b.ci$normal[3])
        else if (b.type == "bca")  c(b.ci$bca[4],     b.ci$bca[5])
        else                       c(b.ci$percent[4], b.ci$percent[5])
      }, error = function(e) c(NA, NA))
      lwr.ci <- boot.ci.res[1]
      upr.ci <- boot.ci.res[2]
    }
  }

  res.global <- data.frame(
    Estimate = Am,
    lwr.ci   = lwr.ci,
    upr.ci   = upr.ci,
    row.names = "A",
    stringsAsFactors = FALSE
  )

  # ---- 3. LOO diagnostics -------------------------------------------------
  diag.judges <- NULL
  diag.items  <- NULL

  # --- 3a. By judge (remove columns) ---
  # Bootstrap: resample ITEMS (rows) -- judges are fixed raters, items are
  # a sample. The LOO effect of each judge generalizes across the item universe.
  if (LOO %in% c("judges", "both")) {
    v.judges <- numeric(m)
    names(v.judges) <- colnames(data) %||% paste0("Judge.", seq_len(m))
    for (j in seq_len(m)) {
      v.judges[j] <- calc.Am(data[, -j, drop = FALSE])
    }
    diff.j  <- v.judges - Am
    ratio.j <- v.judges / Am

    lwr.j <- upr.j <- rep(NA_real_, m)
    if (ci && requireNamespace("boot", quietly = TRUE)) {
      boot.loo.j <- boot::boot(
        data = data,                         # rows = items
        statistic = function(d, idx) {
          d.b <- d[idx, , drop = FALSE]      # resampled items
          A.full <- calc.Am(d.b)
          sapply(seq_len(ncol(d.b)), function(j)
            calc.Am(d.b[, -j, drop = FALSE]) - A.full)
        },
        R = B)
      ci.mat <- t(apply(boot.loo.j$t, 2, function(x)
        quantile(x, c(0.025, 0.975), na.rm = TRUE)))
      lwr.j <- ci.mat[, 1]
      upr.j <- ci.mat[, 2]
    }

    diag.judges <- data.frame(
      Judge.Omit = names(v.judges),
      A.omit     = v.judges,
      Diff.A     = diff.j,
      lwr.ci     = lwr.j,
      upr.ci     = upr.j,
      Ratio      = ratio.j,
      stringsAsFactors = FALSE
    )
  }

  # --- 3b. By item (remove rows) ---
  # Bootstrap: resample JUDGES (columns, via transposition) -- items are the
  # fixed stimuli, judges are a sample from the rater universe. Each item's
  # LOO effect generalizes across the universe of raters.
  if (LOO %in% c("items", "both")) {
    v.items <- numeric(n)
    names(v.items) <- rownames(data) %||% paste0("Item.", seq_len(n))
    for (i in seq_len(n)) {
      v.items[i] <- calc.Am(data[-i, , drop = FALSE])
    }
    diff.i  <- v.items - Am
    ratio.i <- v.items / Am

    lwr.i <- upr.i <- rep(NA_real_, n)
    if (ci && requireNamespace("boot", quietly = TRUE)) {
      boot.loo.i <- boot::boot(
        data = t(data),                      # rows = judges (transposed)
        statistic = function(dt, idx) {
          d.b <- t(dt[idx, , drop = FALSE])  # back to items x judges
          A.full <- calc.Am(d.b)
          sapply(seq_len(nrow(d.b)), function(i)
            calc.Am(d.b[-i, , drop = FALSE]) - A.full)
        },
        R = B)
      ci.mat <- t(apply(boot.loo.i$t, 2, function(x)
        quantile(x, c(0.025, 0.975), na.rm = TRUE)))
      lwr.i <- ci.mat[, 1]
      upr.i <- ci.mat[, 2]
    }

    diag.items <- data.frame(
      Item.Omit = names(v.items),
      A.omit    = v.items,
      Diff.A    = diff.i,
      lwr.ci    = lwr.i,
      upr.ci    = upr.i,
      Ratio     = ratio.i,
      stringsAsFactors = FALSE
    )
  }

  # ---- 4. Structured output ----------------------------------------------
  out <- list(global = res.global)
  if (!is.null(diag.judges)) out$diagnostics.judges <- diag.judges
  if (!is.null(diag.items))  out$diagnostics.items  <- diag.items
  return(out)
}
