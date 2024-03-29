% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelObjFit.R
\name{fit}
\alias{fit}
\alias{fit,modelObj,data.frame-method}
\title{Obtain parameter estimates}
\usage{
fit(object, data, response, ...)

\S4method{fit}{modelObj,data.frame}(object, data, response, ...)
}
\arguments{
\item{object}{An object of class \code{modelObj} as returned by the 
buildModelObj function.}

\item{data}{An object of class data.frame containing the variables in the 
model.}

\item{response}{An object of class vector containing the response variable.}

\item{...}{ignored}
}
\value{
An object of class \code{modelObjFit}, which contains the
  object returned by the modeling function and the method to be used to 
  obtain predictions.
}
\description{
Performs specified regression analysis.
}
\details{
If defined by the modeling function, the following methods can be applied 
  to the value object returned: \code{coef}, \code{plot}, \code{predict},
  \code{print}, \code{residuals}, \code{show}, and \code{summary}.
}
\examples{
   # generate data
   X <- matrix(rnorm(1000,0,1),
               ncol=4,
               dimnames=list(NULL,c("X1","X2","X3","X4")))

   Y <- X \%*\% c(0.1, 0.2, 0.3, 0.4) + rnorm(250)

   X <- data.frame(X)

   # create modeling object using a formula
   mo <- buildModelObj(model=Y ~ X1 + X2 + X3 + X4,
                  solver.method='lm')

   # fit model
   fit.obj <- fit(object=mo, data=X, response=Y)

   coef(fit.obj)
   head(residuals(fit.obj))
   plot(fit.obj)
   head(predict(fit.obj,X))
   summary(fit.obj)

}
