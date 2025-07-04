% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lvr_game.R
\name{ev_Yeffect}
\alias{ev_Yeffect}
\title{Compute the Expected Effect on an Individual Based on the Other Person's Response}
\usage{
ev_Yeffect(
  X = NULL,
  pX = 0,
  s = matrix(rep(0, 4), nrow = 2),
  pY,
  response = TRUE
)
}
\arguments{
\item{X}{An object of class \code{lvr}, representing the individual making the confession.}

\item{pX}{Numeric. The probability that the individual makes the confession.}

\item{s}{A 2×2 numeric matrix.
it represents the effect matrix of individual choices after a confession.}

\item{pY}{Numeric. The probability that the other person accepts the confession.}

\item{response}{Logical. Indicates whether the other person accepts the confession.
Use \code{TRUE} for acceptance and \code{FALSE} for rejection.}
}
\value{
A numeric value indicating the expected effect on the individual, based on the other person's response.
}
\description{
Compute the Expected Effect on an Individual Based on the Other Person's Response
}
\details{
The effect matrix \code{X$score} must be a \eqn{2 \times 2} matrix:

\deqn{
\begin{bmatrix}
s_{11} & s_{12} \\
s_{21} & s_{22}
\end{bmatrix}
}

- Row 1: Other person accepts the confession
- Row 2: Other person rejects the confession
- Column 1: Individual chooses to confess
- Column 2: Individual chooses not to confess

See \code{\link{lvrCreate}} for the detailed construction and meaning of each element in the matrix.

The expected effect is calculated as:

\itemize{
  \item If \code{response = TRUE} (acceptance): \eqn{p_Y \cdot (p_X \cdot s_{11} + (1 - p_X) \cdot s_{12})}
  \item If \code{response = FALSE} (rejection): \eqn{(1 - p_Y) \cdot (p_X \cdot s_{21} + (1 - p_X) \cdot s_{22})}
}
}
\examples{
someone = lvrCreate(Name = "Romeo", Inilv = 1, P = 0.5,
                    Lvsef = function(x, y) {5 * y - .97 * x * y},
                    Lvoth = function(x, y) {1.2 * y},
                    Score = c(1.7, 0.8, -1.5, 0))
ev_Yeffect(someone, pY = 0.8, response = TRUE)  # acceptance
ev_Yeffect(someone, pY = 0.8, response = FALSE) # rejection

}
\seealso{
\code{\link{evfun_Yeffect}}, \code{\link{mev_Yeffect}}
}
