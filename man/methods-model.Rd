\name{model}
\alias{model}
\alias{model,modelObj-method}
\alias{model,modelObjXY-method}
\alias{model,modelObjFormula-method}
\alias{model,modelObjFit-method}

\title{ Retrieve model Object }

\description{ Retrieve the model slot from an object of class modelObj. }

\usage{
       \S4method{model}{modelObj}(object, ...)
}

\arguments{

  \item{object }{an object of class modelObj.}

  \item{\dots  }{ignored.}

}

\value{
       Returns an object of class "formula."
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
    #----------------------------------------------------#
    # Create modeling object using a formula
    #----------------------------------------------------#
    mo <- buildModelObj(model=Y ~ X1 + X2 + X3 + X4,
                        solver.method='lm')


    model(mo)
}
