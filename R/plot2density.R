# sets the starting number used to generate a sequence of random numbers
#– it ensures that you get the same result if you start with that same
#seed each time you run the same process.
 #set.seed(1)

# library(ggplot2)


# m_1<-1
# m_2<-3


#ctrl+shift+c komentarisanje vise linija od jednom

   # df <- data.frame(
   #  sex=factor(rep(c("F", "M"), each=200)),
   #  weight=round(c(rnorm(200, mean=m_1, sd=1),
   #               rnorm(200, mean=m_2, sd=1)))
   #  )


#detach("package:dplyr", unload=TRUE)
#install.packages("plyr")
#library(plyr)

#head(mu)

#mu<-0
#' Plot density
#'
#' @param df Two factor data frame each 200 from rnorm(m_1,1).
#'
#' @return Density plot over histogram
#' @export
#'
#' @examples  set.seed(1)
#' library(ggplot2)
#' m_1<-1
#' m_2<-3
#' df <- data.frame(
#' sex=factor(rep(c("F", "M"), each=200)),
#'  weight=round(c(rnorm(200, mean=m_1, sd=1),
#'                 rnorm(200, mean=m_2, sd=1)))
#' )
#'plot2density(df)
plot2density <- function(df) {
weight<-..density..<-sex<-grp.mean<-0
# Histogram with density plot
  ggplot2::ggplot(df, ggplot2::aes(x=weight)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="white")+
    ggplot2::geom_density(alpha=.2, fill="#FF6666")
# Color by groups
  # Change density plot fill colors by groups
p<-ggplot2::ggplot(df, ggplot2::aes(x=weight, color=sex, fill=sex)) +
  # Use semi-transparent fill
  ggplot2::geom_histogram(ggplot2::aes(y=..density..), alpha=0.2,
                 position="identity")+
  ggplot2::geom_density(alpha=.8)
#calculating means
mu <- plyr::ddply(df, "sex", plyr::summarise, grp.mean=mean(weight))
# Add mean lines
p+ggplot2::geom_vline(data=mu, ggplot2::aes(xintercept=grp.mean, color=sex),
             linetype="dashed")
}

#
#
# plot2density(df,split)
#
#
#
# set.seed(1)
# # napravicemo histogram uzorka iz iz nepoznate eksponencijalne raspodele
# h <- hist(rexp(1000, rate = rexp(1, 0.1)))
#
# dmix <- function(x, mi1, mi2) {
#   0.3 * dnorm(x, mi1, 1) +
#     0.7 * dnorm(x, mi2, 1)
# }
# # Uzmemo konkretne vrednosti m_1 = -1 i m_2 = 3
# curve(dmix(x, -1, 3), xlim = c(-10, 10))
#
# rmix <- function(n) {
#   u <- runif(n)
#   (u < 0.3) * rnorm(n, -1) +
#     (u >= 0.3) * rnorm(n, 3)
# }
#
# curve(dmix(x, -1, 3), xlim = c(-10, 10))
# lines(density(rmix(100000)), col="green")
#
# uzorak <- rmix(1000)
#
# curve(dnorm(x, -1, 1), xlim = c(-10, 10))
# lines(density(dnorm(x,3,1)), col="green")
#
# plot2density()
