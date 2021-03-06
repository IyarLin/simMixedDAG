% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_mixed_dag.R
\name{sim_mixed_dag}
\alias{sim_mixed_dag}
\title{sim_mixed_dag}
\usage{
sim_mixed_dag(dag_model, ...)
}
\arguments{
\item{dag_model}{An object of either class 'paramteric_dag_model' or
'non_paramteric_dag_model'.}

\item{...}{additional arguments to dispatched method.}
}
\value{
A data.frame simulated from \code{dag_model}.
}
\description{
\code{sim_mixed_dag} is a generic function used to simulate
datasets from paramteric and non paramteric DAG models.
}
\seealso{
\code{\link{sim_mixed_dag.non_parametric_dag_model}}, \code{\link{sim_mixed_dag.parametric_dag_model}}
}
