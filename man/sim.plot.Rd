% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{sim.plot}
\alias{sim.plot}
\title{Plot simulated data from a GLM model}
\usage{
sim.plot(
  model,
  xpred = NULL,
  seed = NULL,
  fit.line = TRUE,
  ci = 0.95,
  npoints = "same",
  orig_jitter = 0.1,
  sim_jitter = 0.1
)
}
\arguments{
\item{model}{a model object, from (eg) lm glm (Poisson, Negative binomial)}

\item{xpred}{the predictor to be plotted on the x axis}

\item{seed}{random seed so that simulation results are replicable}

\item{fit.line}{if TRUE (default) adds fit line with SE}

\item{ci}{passes confidence interval width for fit curve (defaults
to 0.95)}

\item{npoints}{number of data points to for fit line. Either specify
a number, or "same" will return a simulation of the same size
as the original dataset.}

\item{orig_jitter}{amount of jitter to apply to original dataset (default 0.10)}

\item{sim_jitter}{amount of jitter to apply to simulated data (default 0.10)}
}
\value{
ggplot object with simulated data plotted with original
}
\description{
Plot simulated data from a GLM model
}
\examples{
## Anwar M, Green JA, Norris P, et al 
## Prospective daily diary study reporting of any and all symptoms in healthy 
## adults in Pakistan: prevalence and #' response
## BMJ Open 2017;7:e014998
data(symptom)
glm.symptom <- glm(actual_help_days ~ symp_days_reported, 
     family = "poisson", data = symptom)
sim.plot(glm.symptom)

}
