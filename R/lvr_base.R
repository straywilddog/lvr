#' Create an `lvr` Object
#'
#' @description
#' This function creates an object of class `lvr` representing a person in a two-variable dynamical system.
#' The object contains initial values and self- and other-variable influence functions that describe the dynamics of the system.
#' These dynamics are modeled as a set of coupled first-order differential equations. The `lvr` object is used for
#' simulating the behavior of a person within the system.
#'
#' @param Name Character; the name of the person.
#' @param Inilv Numeric; the initial favorability (initial value of the variable representing the person's favorability).
#' @param P Numeric. Represents the individual's probability of confessing their feelings, or the probability that the other person accepts the confession.
#' @param Lvsef An function.
#' A self-influence function which must include two variables. This function defines how the person's favorability is influenced by their own state and the other person's state.
#' @param Lvoth An function.
#' An other-variable influence function which must include two variables. This function defines how the person's favorability is influenced by the other person's state.
#' @param Score A 2×2 numeric matrix or a numeric vector of four values.
#' If a matrix is provided, it represents the effect matrix of individual choices after a confession.
#' The first column corresponds to the effects of one's own decision to confess, depending on the other's responses (accept or reject).
#' The first row reflects the influence of the other person accepting the confession, depending on whether one confesses or not.
#'
#' If a numeric vector is provided, the four values represent, in order:
#' \enumerate{
#'   \item \eqn{s_{11}} — one confesses and the other accepts;
#'   \item \eqn{s_{12}} — one does not confess but the other would have accepted;
#'   \item \eqn{s_{21}} — one confesses and the other rejects;
#'   \item \eqn{s_{22}} — one does not confess and the other would have rejected.
#' }
#'
#' The corresponding effect matrix is:
#' \deqn{
#' \begin{bmatrix}
#' s_{11} & s_{12} \\
#' s_{21} & s_{22}
#' \end{bmatrix}
#' }
#'
#' @return An object of class `lvr` containing:
#'   \itemize{
#'     \item \code{name}: Name of the person.
#'     \item \code{inilv}: Initial favorability value.
#'     \item \code{lvsef}: Self-influence function.
#'     \item \code{lvoth}: Other-variable influence function.
#'     \item \code{score}: An effect matrix of individual choice after confession.
#'   }
#'
#' @export
#' @examples
#' someone = lvrCreate(Name = "Stray Wild Dog", Inilv = 0.9,
#'                     Lvsef = function(x, y) {0},
#'                     Lvoth = function(x, y) {0.5 * y},
#'                     Score = c(1.2, 0.3, -1.3, 0))
#'
lvrCreate = function(Name = character(), Inilv = 0, P = 0,
                     Lvsef = function(x, y) {x}, Lvoth = function(x, y) {y},
                     Score = matrix(data = c(1, 0, -1, 0), ncol = 2, byrow = T))
{
  stopifnot(is.character(Name))
  stopifnot(is.numeric(Inilv))
  stopifnot(is.numeric(P))
  stopifnot(is.function(Lvsef))
  stopifnot(is.function(Lvoth))

  if (is.vector(Score)) {
    Score = matrix(Score[1 : 4], nrow = 2, byrow = T)
  } else if (!is.matrix(Score) || any(dim(Score) != c(2, 2))) {
    stop("'Score' must be a 2x2 matrix or vector.")
  }

  colnames(Score) <- c('Y', 'N')
  row.names(Score) <- c('y', 'n')

  structure(list(
    name = Name,
    inilv = Inilv,
    p = P,
    lvsef = Lvsef,
    lvoth = Lvoth,
    score = Score[c(1, 2), c(1, 2)]
  ),
  class = "lvr")
}

#' Check if an Object is of Class 'lvr'
#'
#' @description
#' Tests whether the given object is an instance of class \code{lvr}.
#' This function performs a strict class equality check.
#' For a more flexible check (including subclasses), consider using \code{inherits(x, "lvr")}.
#'
#' @param x An object to be tested.
#' @return logical
#' @export
#' @examples
#' SWD = lvrCreate(Name = "Stray Wild Dog", Inilv = 0.9,
#'                 Lvsef = function(x, y) {0},
#'                 Lvoth = function(x, y) {0.5 * y})
#' is.lvr(SWD)
#'
is.lvr <- function(x) {
  return(class(x) == 'lvr') #inherits(x, "lvr")
}

#' Print method for objects of class 'lvr'.
#'
#' @param x An object of class \code{lvr}.
#' @param ... Further arguments passed to or from other methods.
#' @return Invisibly returns the input object.
#' @method print lvr
#' @export
#' @examples
#' SWD = lvrCreate(Name = "Stray Wild Dog", Inilv = 0.9,
#'                 Lvsef = function(x, y) {0},
#'                 Lvoth = function(x, y) {0.5 * y},
#'                 Score = c(1.1, 0.5, -2, 0))
#' print(SWD)
#'
print.lvr <- function(x, ...) {
  stopifnot(class(x) == 'lvr')
  out = paste0(
    " Name: ", x$name, '\n',
    " Initial favorability: ", x$inilv, '\n',
    " Effect matrix:\n",
    " \t", 'Y', '\t', "N\n",
    "y\t", round(x$score[1, 1], 2), '\t', round(x$score[1, 2], 2), '\n',
    "n\t", round(x$score[2, 1], 2), '\t', round(x$score[2, 2], 2), '\n'
  )
  cat(out)
  invisible(x)
}

#' Ops group's binary operations for class \code{lvr}.
#'
#' @description
#' These binary operators perform arithmetic operations on the initial favorability of \code{lvr}.
#' @param e1,e2 An object of class \code{lvr} or a numeric.
#' @return An object of class \code{lvr}.
#' @method Ops lvr
#' @export
#' @note
#' At least one of e1 and e2 is numeric and the other is object of class \code{lvr}.
#' @examples
#' R <- lvrCreate(Name = "Romeo", Inilv = 0.9,
#'                Lvsef = function(x, y) {0},
#'                Lvoth = function(x, y) {0.5 * y})
#' J <- lvrCreate(Name = "Juliet", Inilv = 0.9,
#'                Lvsef = function(x, y) {0},
#'                Lvoth = function(x, y) {0.5 * y})
#'
#' R + 0.2
#' 0.2 + R
#' R - 0.2
#' 1 - R
#' J * 1.1
#' 1 / J
#'
Ops.lvr <- function(e1, e2)
{
  if (nargs() == 2L) {
    switch(.Generic, `+` = {}, `-` = {}, `*` = {}, `/` = {},
           stop(gettextf("unary '%s' not defined for \"lvr\" objects",
                         .Generic), domain = NA, call. = FALSE))
    return(e1)
  }

  if (inherits(e1, "lvr") & inherits(e2, "lvr")) {
    stop("It is not allowed to add, subtract, multiply, or divide by two lvr objects.")
    return(0)
  }

  if (.Generic == '+') {
    if (inherits(e1, "lvr") & is.numeric(e2)) {
      e1$inilv = e1$inilv + e2
      return(e1)
    }
    if (inherits(e2, "lvr") & is.numeric(e1)) {
      e2$inilv = e2$inilv + e1
      return(e2)
    }
  } else if (.Generic == '-') {
    if (inherits(e1, "lvr") & is.numeric(e2)) {
      e1$inilv = e1$inilv - e2
      return(e1)
    }
    if (inherits(e2, "lvr") & is.numeric(e1)) {
      e2$inilv = e1 - e2$inilv
      return(e2)
    }
  } else if (.Generic == '*') {
    if (inherits(e1, "lvr") & is.numeric(e2)) {
      e1$inilv = e1$inilv * e2
      return(e1)
    }
    if (inherits(e2, "lvr") & is.numeric(e1)) {
      e2$inilv = e2$inilv * e1
      return(e2)
    }
  } else if (.Generic == '/') {
    if (inherits(e1, "lvr") & is.numeric(e2)) {
      e1$inilv = e1$inilv / e2
      return(e1)
    }
    if (inherits(e2, "lvr") & is.numeric(e1)) {
      e2$inilv = e1 / e2$inilv
      return(e2)
    }
  }
  else {
    stop(gettextf("'%s' not defined for \"lvr\" objects",
                  .Generic), domain = NA)
  }
}
