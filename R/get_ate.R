#' @title get_ate
#'
#' @description \code{get_ate} is a generic function used to
#'   get average treatment effects (\eqn{ATE}) in a DAG model.
#'
#' @param dag_model An object of either class "paramteric_dag_model" or
#'   "non_paramteric_dag_model".
#' @param ... Additional arguments to dispatched method.
#' @return A data.frame with 3 columns:
#'   1. From: The baseline treatment value.
#'   1. To: The value a treatment was changed to.
#'   1. ATE: The average treatment effect of changing the treatment
#'   value from "From" to "To.
#' @seealso \code{\link{get_ate.non_parametric_dag_model}}, \code{\link{get_ate.parametric_dag_model}}
#' @export

get_ate <- function(dag_model, ...) UseMethod("get_ate")
