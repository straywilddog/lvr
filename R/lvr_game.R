#' Compute the Expected Effect on an Individual Based on the Other Person's Response
#'
#' @param X An object of class \code{lvr}, representing the individual making the confession.
#' @param pY Numeric. The probability that the other person accepts the confession.
#' @param response Logical. Indicates whether the other person accepts the confession.
#'        Use \code{TRUE} for acceptance and \code{FALSE} for rejection.
#'
#' @return A numeric value indicating the expected effect on the individual, based on the other person's response.
#'
#' @details
#' The effect matrix \code{X$score} must be a \eqn{2 \times 2} matrix:
#'
#' \deqn{
#' \begin{bmatrix}
#' s_{11} & s_{12} \\
#' s_{21} & s_{22}
#' \end{bmatrix}
#' }
#'
#' - Row 1: Other person accepts the confession
#' - Row 2: Other person rejects the confession
#' - Column 1: Individual chooses to confess
#' - Column 2: Individual chooses not to confess
#'
#' See \code{\link{lvrCreate}} for the detailed construction and meaning of each element in the matrix.
#'
#' The expected effect is calculated as:
#'
#' \itemize{
#'   \item If \code{response = TRUE} (acceptance): \eqn{p_Y \cdot (p_X \cdot s_{11} + (1 - p_X) \cdot s_{12})}
#'   \item If \code{response = FALSE} (rejection): \eqn{(1 - p_Y) \cdot (p_X \cdot s_{21} + (1 - p_X) \cdot s_{22})}
#' }
#'
#' @examples
#' someone = lvrCreate(Name = "Romeo", Inilv = 1, P = 0.5,
#'                     Lvsef = function(x, y) {5 * y - .97 * x * y},
#'                     Lvoth = function(x, y) {1.2 * y},
#'                     Score = c(1.7, 0.8, -1.5, 0))
#' ev_Yeffect(someone, pY = 0.8, response = TRUE)  # acceptance
#' ev_Yeffect(someone, pY = 0.8, response = FALSE) # rejection
#'
#' @seealso \code{\link{evfun_Yeffect}}, \code{\link{mev_Yeffect}}
#' @export
ev_Yeffect <- function(X, pY, response = TRUE) {
  if (!inherits(X, "lvr")) stop("X must be an object of class 'lvr'.")
  if (missing(pY)) stop("Missing argument: 'pY'")
  if (!is.matrix(X$score) || any(dim(X$score) != c(2, 2))) stop("'score' must be a 2x2 matrix in X.")
  if (!is.logical(response) || length(response) != 1) stop("'response' must be a single TRUE/FALSE value.")

  pX <- X$p
  s <- X$score

  if (response) {
    return(pY * (pX * s[1, 1] + (1 - pX) * s[1, 2]))
  } else {
    return((1 - pY) * (pX * s[2, 1] + (1 - pX) * s[2, 2]))
  }
}

#' Generate a Function to Compute the Expected Effect of the Other Person's Response
#'
#' @param Score A 2×2 numeric matrix. Represents the effect matrix of individual choices and responses after a confession.
#' @param pY Numeric. The probability that the other person accepts the confession.
#'
#' @return A function of two variables: \code{pX}, the probability of confessing, and \code{response}, a logical value.
#' If \code{TRUE}, computes the effect under acceptance; if \code{FALSE}, under rejection.
#' The returned function uses the fixed values of \code{pY} and \code{Score}.
#'
#' @details
#' This is a function factory that returns a unary function of \code{pX}.
#' The returned function evaluates the expected effect for a given probability \code{pX} of making a confession, assuming the other person's response probability \code{pY} and response type (\code{response}) are fixed.
#'
#' The effect matrix \code{Score} is structured as follows:
#'
#' \deqn{
#' \begin{bmatrix}
#' s_{11} & s_{12} \\
#' s_{21} & s_{22}
#' \end{bmatrix}
#' }
#'
#' - Row 1: Other person accepts the confession
#' - Row 2: Other person rejects the confession
#' - Column 1: Individual chooses to confess
#' - Column 2: Individual chooses not to confess
#'
#' See \code{\link{lvrCreate}} for the detailed construction and meaning of each element in the matrix.
#'
#' @examples
#' score <- matrix(c(3, 1, -1, 0), nrow = 2, byrow = TRUE)
#' f_accept <- evfun_Yeffect(score, pY = 0.8)
#' f_accept(pX = 0.6, response = TRUE)  # Expected effect if other accepts
#'
#' f_reject <- evfun_Yeffect(score, pY = 0.8)
#' f_reject(pX = 0.6, response = FALSE)  # Expected effect if other rejects
#'
#' @seealso \code{\link{ev_Yeffect}}, \code{\link{mev_Yeffect}}
#' @export
evfun_Yeffect <- function(Score, pY) {
  if (missing(pY)) stop("Missing argument: 'pY'")
  if (!is.matrix(Score) || any(dim(Score) != c(2, 2))) stop("'Score' must be a 2x2 matrix.")

  return(function(pX, response = TRUE) {
    if (response) {
      pY * (pX * Score[1, 1] + (1 - pX) * Score[1, 2])
    } else {
      (1 - pY) * (pX * Score[2, 1] + (1 - pX) * Score[2, 2])
    }
  })
}

#' Calculate the Mean Expected Effect on an Individual Based on the Other Person's Response
#'
#' @param X An object of class \code{lvr}, representing the individual making the confession.
#' @param pX Numeric. The probability that the individual chooses to confess.
#' @param pY Numeric. The probability that the other person accepts the confession.
#' @param Score A 2×2 numeric matrix. Represents the effect matrix of individual choices and responses after a confession.
#'
#' @return A numeric value indicating the overall expected effect on the individual, integrating both the acceptance and rejection outcomes.
#'
#' @details
#' This function evaluates the expected average effect on an individual by weighting both possible outcomes:
#' the other person accepts or rejects the confession.
#'
#' The interpretation of the \code{score} matrix is consistent with the definitions in \code{\link{lvrCreate}} and \code{\link{ev_Yeffect}}.
#' Specifically, the effect matrix is:
#'
#' \deqn{
#' \begin{bmatrix}
#' s_{11} & s_{12} \\
#' s_{21} & s_{22}
#' \end{bmatrix}
#' }
#'
#' See \code{\link{lvrCreate}} for the detailed construction and meaning of each element in the matrix.
#'
#' @examples
#' score <- matrix(c(3, 1, -1, 0), nrow = 2, byrow = TRUE)
#' X <- lvrCreate(Name = "Someone", Inilv = 0.5, P = 0.6,
#'                Lvsef = function(x, y) x, Lvoth = function(x, y) y,
#'                Score = score)
#' mev_Yeffect(X, pY = 0.8)
#'
#' @seealso \code{\link{ev_Yeffect}}, \code{\link{evfun_Yeffect}}
#' @export
mev_Yeffect <- function(X = NULL, pX = NULL, pY, Score = NULL) {
  if (missing(pY)) stop("Missing argument: 'pY'")
  if (!is.null(X)) {
    if (!inherits(X, "lvr")) stop("X must be an object of class 'lvr'.")
    pX <- X$p
    s <- X$score
  } else if (!is.null(pX)) {
    if (!is.numeric(pX) || length(pX) != 1) stop("'pX' must be a numeric scalar.")
    if (!is.matrix(Score) || any(dim(Score) != c(2, 2))) stop("'Score' must be a 2x2 matrix.")
    s <- Score
  } else {
    stop("Must provide either an 'lvr' object X, or both 'pX' and 'Score'.")
  }

  pY * (pX * s[1, 1] + (1 - pX) * s[1, 2]) + (1 - pY) * (pX * s[2, 1] + (1 - pX) * s[2, 2])
}

#' Solve for the confession probability that equalizes expected payoffs under two opposing responses
#'
#' @param Score A 2×2 numeric matrix.
#' See \code{\link{lvrCreate}} for the detailed construction and meaning of each element in the matrix.
#' @return A named vector with optimal confession probability `p` and the corresponding expected effect.
#'
#' @details
#' This function constructs and solves a linear equation to identify the confession probability \code{p}
#' at which the individual receives the same expected effect whether the other person always accepts
#' or always rejects the confession.
#'
#' Specifically, it sets up the equation:
#'
#' \deqn{
#' p \cdot s_{11} + (1 - p) \cdot s_{12} = p \cdot s_{21} + (1 - p) \cdot s_{22}
#' }
#'
#' Here:
#' \itemize{
#'   \item \eqn{s_{11}} is the effect when one confesses and the other accepts.
#'   \item \eqn{s_{12}} is the effect when one does not confess but the other would have accepted.
#'   \item \eqn{s_{21}} is the effect when one confesses but the other rejects.
#'   \item \eqn{s_{22}} is the effect when one does not confess and the other would have rejected.
#' }
#'
#' Solving this equation yields the probability \eqn{p} at which the two opposing strategies yield
#' equal expected outcomes, providing a rational, balanced decision point when the other's response
#' is uncertain.
#'
#' @examples
#' score <- matrix(c(3, 2.9, 1.5, 0), nrow = 2, byrow = TRUE)
#' ev_solve(score)
#'
#' @export
ev_solve <- function(Score) {
  if (!is.matrix(Score) || any(dim(Score) != c(2, 2))) stop("'Score' must be a 2x2 matrix.")

  # 设：p * s11 + (1 - p) * s12 = p * s21 + (1 - p) * s22
  # => 解得 p = (s22 - s12) / [(s11 - s21) + (s22 - s12)]

  num <-  Score[2, 2] - Score[1, 2]
  denom <- Score[1, 1] - Score[2, 1] +  Score[2, 2] - Score[1, 2]

  if (denom == 0) {
    warning("Denominator is zero: no unique solution exists.")
    return(c(NA, NA))
  }
  p <- num / denom

  # 限制 p 在 [0, 1] 范围内
  if (p < 0 || p > 1) warning("Solution p is outside [0, 1]: no intersection within valid range.")

  # 计算交点的效应值（代入其中一个策略即可）
  effect <- p * Score[1, 1] + (1 - p) * Score[1, 2]

  c(p = p, effect = effect)
}
