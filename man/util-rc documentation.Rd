\name{util_rc}
\alias{util_rc}
\title{
Risky Choice Model Fitting Function
}
\description{
Fits one of several models to binary risky choice data. Models include expected utility theory, risk-return, Weber, and hyperbolic discounting.
}
\usage{
util_rc(modeltype, choice, amt1, prob1, amt2, prob2)
}
\arguments{
  \item{modeltype}{
    A character string indicating which model to fit. Options include:
    \itemize{
      \item{"E"}: Expected utility, where ( U = p A^ alpha)
      \item{"R"}: Risk-return, where (U = EV - b *Var)
      \item{"W"}: Weber, where ( U = EV - b*CV), and ( CV = sqrt(Var)/EV)
      \item{"H"}: Hyperbolic, where ( U = A / (1 + h * theta)), with ( theta = (1 - p) / p)
    }
  }
  \item{choice}{
    A numeric vector (length n) with binary values (1 or 0) indicating choice. 1 indicates choice of option 1; 0 indicates option 2.
  }
  \item{amt1}{
    A numeric vector (length n) with non-negative values representing the payoffs of option 1.
  }
  \item{prob1}{
    A numeric vector (length n) with values in [0, 1], representing the probabilities associated with option 1.
  }
  \item{amt2}{
    A numeric vector (length n) with non-negative values representing the payoffs of option 2.
  }
  \item{prob2}{
    A numeric vector (length n) with values in [0, 1], representing the probabilities associated with option 2.
  }
}
\details{
Validates the inputs and fits the chosen model to the binary risky choice data using maximum likelihood estimation.
The function uses Limited-memory BFGS for optimization. If all data is one-sided (i.e., all choices are the same),
parameter fitting is restricted to the boundary and handled separately.
}
\value{
A list with the following components:
  \item{output}{A list containing:
    \itemize{
      \item{fitted_param: }{Estimated model parameter (alpha, b, or h depending on modeltype)}
      \item{inv_temp: }{Estimated inverse temperature, capturing choice randomness}
    }
  }
  \item{fit_metrics}{Optimization result object from L-BFGS-B}
  \item{modeltype}{The model type used}
  \item{num_observations}{The number of observations (n)}
}
