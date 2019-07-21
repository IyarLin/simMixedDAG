#' @title sim_mixed_dag
#'
#' @description \code{sim_mixed_dag} is a generic function used to simulate
#'   datasets from paramteric and non paramteric DAG models.
#'
#' @param dag_model An object of either class 'paramteric_dag_model' or
#'   'non_paramteric_dag_model'.
#' @param ... additional arguments to dispatched method.
#' @return A data.frame simulated from \code{dag_model}.
#' @seealso \code{\link{sim_mixed_dag.non_parametric_dag_model}}, \code{\link{sim_mixed_dag.parametric_dag_model}}
#' @export

sim_mixed_dag <- function(dag_model, ...) UseMethod("sim_mixed_dag")
