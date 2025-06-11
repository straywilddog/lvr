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
#' @param Lvsef Function; a self-influence function which must include two variables. This function defines how the person's favorability
#'              is influenced by their own state and the other person's state.
#' @param Lvoth Function; an other-variable influence function which must include two variables. This function defines how the person's favorability
#'               is influenced by the other person's state.
#'
#' @return An object of class `lvr` containing:
#'   \itemize{
#'     \item \code{name}: Name of the person.
#'     \item \code{inilv}: Initial favorability value.
#'     \item \code{lvsef}: Self-influence function.
#'     \item \code{lvoth}: Other-variable influence function.
#'   }
#'
#' @export
#' @examples
#' someone = lvrCreate(Name = "Stray Wild Dog", Inilv = 0.9,
#'                     Lvsef = function(x, y) {0},
#'                     Lvoth = function(x, y) {0.5 * y})
#'
lvrCreate = function(Name = character(), Inilv = 0,
                     Lvsef = function(x, y) {x}, Lvoth = function(x, y) {y})
{
  stopifnot(is.character(Name))
  stopifnot(is.numeric(Inilv))
  stopifnot(is.function(Lvsef))
  stopifnot(is.function(Lvoth))

  structure(list(
    name = Name,
    inilv = Inilv,
    lvsef = Lvsef,
    lvoth = Lvoth
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
#'                 Lvoth = function(x, y) {0.5 * y})
#' print(SWD)
#'
print.lvr <- function(x, ...) {
  stopifnot(class(x) == 'lvr')
  out = paste0(
    " Name: ", x$name, '\n',
    " Initial favorability: ", x$inilv, '\n'
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
