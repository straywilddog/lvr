#' Solver for a system of coupled first-order differential equations
#'
#' @importFrom stats rnorm
#' @description
#' S3 method for class \code{lvr}, simulating the trajectory of a two-variable dynamical system using the Euler method.
#'
#' @param Lvr1,Lvr2 An object of class \code{lvr}.
#' @param step_t A numeric value for a short period of time (Δt).
#' @param Times A numeric value of iterations.
#' @param noise A numeric value indicating the weight or proportion of noise to apply.
#' @param m A numeric value for the vector of means to generate noise.
#' @param s A numeric value for the vector of standard deviations to generate noise.
#' @return A matrix with columns representing time, trajectory, and the changing rate of the trajectory.
#' @export
#'
#' @details
#' This function numerically solves a system of coupled first-order differential equations
#' defined by two \code{lvr} objects (\code{Lvr1} and \code{Lvr2}), each representing a variable's dynamics.
#' The simulation uses the Euler integration method with a small time step (\code{step_t}) and a specified number of iterations (\code{Times}).
#'
#' At each time step, the change in each variable (\code{dl1} and \code{dl2}) is computed using user-supplied self and other influence functions
#' (\code{lvsef} and \code{lvoth} within each \code{lvr} object), optionally adding Gaussian noise controlled by parameters \code{noise}, \code{m}, and \code{s}.
#'
#' The Euler update is applied as:
#' \deqn{x(t + \Delta t) = x(t) + \Delta x = x(t) + f(x(t)) \cdot \Delta t}
#' where \eqn{f(x(t))} is the rate of change at time \eqn{t}, defined by the combination of self- and cross-variable dynamics plus noise.
#'
#' The returned matrix has five columns:
#' \itemize{
#'   \item \code{t}: Time steps
#'   \item \code{lv1}, \code{lv2}: Variable trajectories
#'   \item \code{dl1}, \code{dl2}: Their corresponding rate of change at each time step
#' }
#'
#' @examples
#' R = lvrCreate(Name = "Romeo", Inilv = 1,
#'               Lvsef = function(x, y) {5 * y - .97 * x * y},
#'               Lvoth = function(x, y) {1.2 * y})
#' J = lvrCreate(Name = "Juliet", Inilv = 0.5,
#'               Lvsef = function(x, y) {-3 * y + 1.01 * x * y},
#'               Lvoth = function(x, y) {-1.1 * y})
#' library(ggplot2)
#' df = trajlvr(R, J, step_t = 0.01, Times = 700)
#' df = as.data.frame(df)
#'
#' p = ggplot(df, aes(x = lv1, y = lv2)) + geom_path() +
#'   annotate("segment", x = df$lv1[nrow(df)], y = df$lv2[nrow(df)],
#'            xend = df$lv1[nrow(df)] + df$dl1[nrow(df)] * 0.01,
#'            yend = df$lv2[nrow(df)] + df$dl2[nrow(df)] * 0.01,
#'            arrow = arrow(length = unit(.5 , "cm")), color = "red") +
#'   xlab("R") + ylab("J") +
#'   theme_bw()
#' p
#'
#' df = trajlvr(Lvr1 = R, Lvr2 = J, step_t = 0.01, Times = 700, noise = 1.1, m = 0.5, s = 2)
#' df = reshape2::melt(as.data.frame(df), id.vars = 't', measure.vars = c('lv1', 'lv2'))
#'
#' p = ggplot(df, aes(x = t, y = value)) +
#'   geom_path(aes(color = variable)) +
#'   scale_color_manual(name = ' ',
#'                      breaks = c('lv1', 'lv2'),
#'                      values = c("blue", "red"),
#'                      label = c('R', 'J')) +
#'   xlab("t") + ylab("Lv") +
#'   theme_bw()
#' p
#'
trajlvr <- function(Lvr1, Lvr2, step_t = 0.0001, Times = 100, noise = 0, m = 0, s = 1)
{
  stopifnot(class(Lvr1) == "lvr")
  stopifnot(class(Lvr2) == "lvr")

  lvrdata <- matrix(NA_real_, nrow = Times + 1, ncol = 5)
  colnames(lvrdata) <- c("t", "lv1", "lv2", "dl1", "dl2")
  lvrdata[, "t"] <- seq(0, step_t * Times, by = step_t)

  lvrdata[1, "lv1"] = Lvr1$inilv
  lvrdata[1, "lv2"] = Lvr2$inilv
  lvrdata[1, "dl1"] = noise * rnorm(1, mean = m, sd = s) +
    Lvr1$lvsef(lvrdata[1, "lv1"], lvrdata[1, "lv2"]) +
    Lvr1$lvoth(lvrdata[1, "lv1"], lvrdata[1, "lv2"])
  lvrdata[1, "dl2"] = noise * rnorm(1, mean = m, sd = s) +
    Lvr2$lvsef(lvrdata[1, "lv2"], lvrdata[1, "lv1"]) +
    Lvr2$lvoth(lvrdata[1, "lv2"], lvrdata[1, "lv1"])

  for (i in 2 : (Times + 1)) {

    lvrdata[i, "lv1"] = lvrdata[i - 1, "dl1"] * step_t + lvrdata[i - 1, "lv1"]
    lvrdata[i, "lv2"] = lvrdata[i - 1, "dl2"] * step_t + lvrdata[i - 1, "lv2"]

    lvrdata[i, "dl1"] = noise * rnorm(1, mean = m, sd = s) +
      Lvr1$lvsef(lvrdata[i, "lv1"], lvrdata[i, "lv2"]) +
      Lvr1$lvoth(lvrdata[i, "lv1"], lvrdata[i, "lv2"])
    lvrdata[i, "dl2"] = noise * rnorm(1, mean = m, sd = s) +
      Lvr2$lvsef(lvrdata[i, "lv2"], lvrdata[i, "lv1"]) +
      Lvr2$lvoth(lvrdata[i, "lv2"], lvrdata[i, "lv1"])

  }

  lvrdata
}

#' Phase Field Simulation for Coupled First-Order Dynamical Systems
#'
#' @description
#' Simulates the phase field (vector field) of a two-variable dynamical system over a grid of initial conditions.
#' Each simulation is based on a short trajectory using the Euler method, incorporating user-defined self and other interaction functions.
#' This is useful for visualizing the phase plane and understanding local system dynamics.
#'
#' @param data A numeric matrix or data frame with two columns representing initial values for the two variables.
#'             If \code{NULL}, a regular grid over the specified \code{xlim} and \code{ylim} will be used.
#' @param xlim,ylim Numeric vectors of length 2 specifying the range of initial values for the two variables (if \code{data = NULL}).
#' @param breaks A numeric value indicating the step size for generating the grid (used if \code{data = NULL}).
#' @param lvparam A named list containing four functions:
#'   \describe{
#'     \item{\code{Lvsef1}}{Self-influence function for variable 1 (e.g., \code{function(x, y)})}
#'     \item{\code{Lvoth1}}{Other-variable influence function for variable 1}
#'     \item{\code{Lvsef2}}{Self-influence function for variable 2}
#'     \item{\code{Lvoth2}}{Other-variable influence function for variable 2}
#'   }
#' @param step_t A small numeric value specifying the Euler integration time step (\eqn{\Delta t}).
#' @param Times An integer specifying the number of time steps for each trajectory.
#' @param noise A numeric value controlling the weight of Gaussian noise added to the rate of change.
#' @param m A numeric mean of the noise distribution.
#' @param s A numeric standard deviation of the noise distribution.
#'
#' @return A numeric matrix with \code{(Times + 1) * N} rows, where \code{N} is the number of sampled initial conditions.
#'         The matrix has five columns:
#'   \itemize{
#'     \item \code{t} — Time point
#'     \item \code{lv1} — Value of variable 1
#'     \item \code{lv2} — Value of variable 2
#'     \item \code{dl1} — Rate of change for variable 1 at time \code{t}
#'     \item \code{dl2} — Rate of change for variable 2 at time \code{t}
#'   }
#'
#' @examples
#' lvp = list(Lvsef1 = function(x, y) {-0.67 * x}, Lvoth1 = function(x, y) {0.82 * y},
#' Lvsef2 = function(x, y) {-0.67 * x}, Lvoth2 = function(x, y) {0.82 * y})
#'
#' inid = expand.grid(x = unique(c(seq(-7, -2, 0.3), seq(-2, 2, 0.15), seq(2, 7, 0.3))),
#'                    y = unique(c(seq(-7, -2, 0.3), seq(-2, 2, 0.15), seq(2, 7, 0.3))))
#' df = phaselvr(data = inid, lvparam = lvp, step_t = 0.001, Times = 1)
#' df = as.data.frame(df)
#' #define a regular grid
#' df = df[which(df$t == 0), ]
#'
#' library(ggplot2)
#' library(ggquiver)
#' library(metR)
#'
#' p = ggplot(df, aes(x = lv1, y = lv2)) +
#'   geom_streamline(aes(dx = dl1, dy = dl2), color = "blue", linewidth = .2) +
#'   scale_x_continuous(limits = c(min(inid[, 1]), max(inid[, 1])), expand = c(0, 0)) +
#'   scale_y_continuous(limits = c(min(inid[, 2]), max(inid[, 2])), expand = c(0, 0)) +
#'   xlab("R") + ylab("J") +
#'   theme_bw()
#' p
#'
#' @export
#'
phaselvr <- function(data = NULL, xlim = c(-5, 5), ylim = c(-5, 5), breaks = 0.5,
                     lvparam = list(Lvsef1 = function(x, y) x,
                                    Lvoth1 = function(x, y) y,
                                    Lvsef2 = function(x, y) x,
                                    Lvoth2 = function(x, y) y),
                     step_t = 0.0001, Times = 1, noise = 0, m = 0, s = 1)
{

  stopifnot(length(xlim) == 2)
  stopifnot(length(ylim) == 2)
  stopifnot(is.numeric(xlim) & is.numeric(ylim))
  stopifnot(is.numeric(breaks))
  if (!is.null(data)) {
    stopifnot(ncol(data) == 2)
  } else {
    data = expand.grid(x = seq(xlim[1], xlim[2], breaks), y = seq(ylim[1], ylim[2], breaks))
  }
  nsample = nrow(data)
  stopifnot(is.numeric(step_t))
  stopifnot(is.numeric(Times))
  stopifnot(is.numeric(noise) & is.numeric(m) & is.numeric(s))

  phased = matrix(
    NA_real_, nrow = (Times + 1) * nsample, ncol = 5
  )
  colnames(phased) <- c('t', "lv1", "lv2", "dl1", "dl2")

  Lvsef1 = lvparam$Lvsef1
  Lvoth1 = lvparam$Lvoth1
  Lvsef2 = lvparam$Lvsef2
  Lvoth2 = lvparam$Lvoth2
  for (sam in 1 : nsample) {

    lvr1 = lvrCreate(Name = '',
                     Inilv = data[sam, 1],
                     Lvsef = Lvsef1,
                     Lvoth = Lvoth1)
    lvr2 = lvrCreate(Name = '',
                     Inilv = data[sam, 2],
                     Lvsef = Lvsef2,
                     Lvoth = Lvoth2)

    traDATA = trajlvr(lvr1, lvr2, step_t = step_t, Times = Times,
                     noise = noise, m = m, s = s)
    idx = ((sam - 1) * (Times + 1) + 1) : (sam * (Times + 1))
    phased[idx, 1 : 5] <- traDATA

  }

  phased
}
