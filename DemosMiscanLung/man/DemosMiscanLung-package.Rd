\name{DemosMiscanLung-package}
\alias{DemosMiscanLung-package}
\alias{DemosMiscanLung}
\docType{package}
\title{Microsimulation Model for Lung Cancer Screening}
\description{
DemosMiscanLung provides a high-performance discrete event microsimulation for
evaluating lung cancer screening strategies and cost-effectiveness in a population
setting. The package relies on C++ core code (via Rcpp) for efficient large-scale
simulations and supplies R scripts for configuration, running, and post-processing.
}
\details{
Key features:
\itemize{
  \item Smoking behavior simulation with multi-state transitions.
  \item Lung cancer natural history (onset, stage progression, dwell times).
  \item Screening modules supporting multiple strategies and eligibility rules.
  \item Economic evaluation (costs and QALYs) and PSA support.
}

Before first use, compile and install the package from the project root:
\preformatted{R CMD INSTALL -preclean DemosMiscanLung}
}
\author{Maintainer: Yichen He \email{e0732876@u.nus.edu}}
\keyword{package}
\seealso{\code{\link{schedule_population}}}
\examples{
\dontrun{
  # After installing the package, run a simple simulation via the provided script
  setwd("simulation/")
  source("main.R")
}
}
