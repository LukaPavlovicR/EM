#p=0.7, m_1=-1, m_2=3

#' Using EM algorithm
#'
#' @param p Probability
#' @param m_1 Expected value of first
#' @param m_2 Expected value of second
#'
#' @return Iterations towards convergence
#' @export
#'
#' @examples
#' EM2N(0.7, -1, 3)
#'
EM2N <- function(p,m_1,m_2){


  dmix <- function(x, mi1, mi2) {
    (1-p)* stats::dnorm(x, mi1, 1) +
      p * stats::dnorm(x, mi2, 1)
  }
  # Uzmemo konkretne vrednosti m_1 = -1 i m_2 = 3
  # curve(dmix(x, -1, 3), xlim = c(-10, 10))

  rmix <- function(n) {
    u <- stats::runif(n)
    (u < 1-p) * stats::rnorm(n, m_1) +
      (u >= 1-p) * stats::rnorm(n, m_2)
  }


  uzorak <- rmix(1000)

  # za p smo uzeli 0.7
  expectation <- function(mi1, mi2, xs) {
    p*stats::dnorm(xs, mi2) /
      ((1-p)*stats::dnorm(xs, mi1) +
         p*stats::dnorm(xs, mi2))
  }

  # logaritam verodostojnosti
  logLxw <- function(mi1, mi2, xs, ws) {
    sum((1-ws)*log(stats::dnorm(xs, mi1)) +
          ws*log(stats::dnorm(xs, mi2)))
  }

  # trazimo argmax(L) tako sto nadjemo argmin(-L) pomocu funkcije nlm
  maximization <- function(Ew, xs, mi_0) {
    stats::nlm(function(mi) -logLxw(mi[1], mi[2], xs, Ew), mi_0)$estimate
  }

  EM_estimate_mix <- function(mi_0, xs, tol=1e-6, maxiter=100) {
    iter <- 0
    Ew <- expectation(mi_0[1], mi_0[2], xs)
    mi <- maximization(Ew, xs, mi_0)
    print(mi)

    while(any(abs(mi - mi_0) > tol)){
      if(iter == maxiter)
        break

      mi_0 <- mi

      Ew <- expectation(mi_0[1], mi_0[2], xs)
      mi <- maximization(Ew, xs, mi_0)

      print(mi)
      iter <- iter + 1
    }
    mi
  }

  EM_estimate_mix(c(0,1), uzorak)
}

# primer:
# EM2N(0.7, -1, 3)
