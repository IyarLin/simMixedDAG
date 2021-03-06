% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parametric_dag_model.R
\name{parametric_dag_model}
\alias{parametric_dag_model}
\title{Specify a parametric DAG model}
\usage{
parametric_dag_model(dag, f.args = NULL)
}
\arguments{
\item{dag}{An object of class "dagitty" representing the DAG.}

\item{f.args}{A list specifying the different node function arguments.}
}
\value{
An object of class "parametric_dag_model" which is essentially a list containing the following
elements:
\enumerate{
\item dag: The model DAG
\item f.args: The f.args input, with all non specified arguments set using defaults.
}
}
\description{
\code{parametric_dag_model} is used to specify paramteric DAG models.
Any none specified function arguments are filled in using defaults.
The resulting obejct of class "parametric_dag_model" has methods to simulate
observations from the model specified and get it's \eqn{ATE}s.
}
\details{
Every node \eqn{j} in a parametric DAG model is simulated from a function
of the node parents \code{PA_{j}} and additional arguments. Denoting by
\eqn{lp = \sum_{i in PA_{j}} \beta_{ij}x_i} the linear combination of the parent nodes, the function for a
continuous node looks like the following:

\deqn{f(PA_{node}) = g(lp + \epsilon)}

and the following for a discrete node:

\deqn{f(PA_{node}) = cat(g(lp + \epsilon))}

\code{f.args} is a list where each element is in itself a list
specifying the additional function arguments relating to the node
after which that element is named after (this is a long sentence, see examples below).

The f.args elements can include the following optional elements:
\enumerate{
\item \code{levels}: The number of levels a discrete node takes. \code{levels} = 1 means
the node is continuous.
\item \code{betas}: A list. Each element is a vector/scalar denoting the \eqn{\beta} coefficients
of the parent node after which it is named.
\item \code{link}: The link function \code{g}. One of "identity", "quadratic", "exp" or "cosin".
\item \code{sinr}: Signal to ratio. This ensures that \eqn{var(lp) / var(\epsilon) = sinr}
}
}
\examples{
require(dagitty)
g <- dagitty("dag {
sales [pos=\\"0,0\\"]
             mkt [pos=\\"2,0\\"]
             comp [pos=\\"1,1\\"]
             visits [pos=\\"1,0\\"]
             visits -> sales
             mkt -> visits
             comp -> mkt
             comp -> sales
             }")

plot(g)

param_dag_model <- parametric_dag_model(
  dag = g,
  f.args = list(
    sales = list(betas = list(visits = 0.3, comp = -0.9)),
    visits = list(betas = list(mkt = 0.5)),
    mkt = list(betas = list(comp = 0.6))
  )
)

sim_data <- sim_mixed_dag(dag_model = param_dag_model)

plot(sim_data$mkt, sim_data$sales) # confounded relation
}
\seealso{
\code{\link{non_parametric_dag_model}} for non parametric DAG model specification. Methods for
the parametric_dag_model class include \code{\link{sim_mixed_dag.parametric_dag_model}} for
simulating datasets and \code{\link{get_ate.parametric_dag_model}} for getting \eqn{ATE}s.
}
