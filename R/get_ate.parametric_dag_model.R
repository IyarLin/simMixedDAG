#' @title Get \eqn{ATE} for a paramteric DAG model
#'
#' @description \code{get_ate.parametric_dag_model} uses simulations
#'   to calculate the average treatment effect (\eqn{ATE}) of
#'   a treatment variable on an exposure variable given
#'   a parametric DAG model.
#'
#' @param dag_model An object of class "parametric_dag_model".
#' @param treatment Name of a **single** treatment variable.
#' @param treatment_vals A vector of treatment values to be considered.
#' @param exposure Name of a **single** exposure variable.
#' @param M Number of simulations to run. Each simulation dataset consists of 1000 observations.
#' @return A data.frame with 3 columns:
#'   1. From: The baseline treatment value.
#'   1. To: The value a treatment was changed to.
#'   1. ATE: The average treatment effect of changing the treatment
#'   value from "From" to "To.
#' @example examples/example_get_ate.parametric_dag_model.R
#' @importFrom dagitty parents
#' @importFrom stats quantile
#' @seealso \code{\link{get_ate.non_parametric_dag_model}} for non parametric dag model \eqn{ATE} calculation.
#' @export

get_ate.parametric_dag_model <- function(dag_model, treatment, treatment_vals = NULL, exposure, M = 1000) {
  N <- 1000
  dag <- dag_model$dag
  f.args <- dag_model$f.args
  if(length(treatment) > 1) stop("get_ate supports ate calculation for a single treatment only")
  if(length(exposure) > 1) stop("get_ate supports ate calculation for a single exposure only")
  if (f.args[[exposure]]$levels > 2) stop("Exposure must be either numeric (levels = 1) or binary (levels = 2)")

  if (is.null(treatment_vals)) {
    if (f.args[[treatment]]$levels == 1) {
      sample_treatment <- sim_mixed_dag(dag_model)[[treatment]]
      treatment_vals <- unname(quantile(sample_treatment, seq(0.05, 0.95, by = 0.3)))
    } else {
      treatment_vals <- factor(f.args[[treatment]]$labels, labels = f.args[[treatment]]$labels)
    }
  }

  results <- matrix(ncol = length(treatment_vals), nrow = M)
  ans <- data.frame(from = head(treatment_vals, -1), to = tail(treatment_vals, -1), ATE = NA)
  vars <- names(dag)
  vars <- vars[vars != treatment]
  parents <- setNames(lapply(vars, function(var) dagitty::parents(dag, var)), vars)

  env <- environment()

  for (i in 1:length(treatment_vals)) {
    treatment_ls <- setNames(list(rep(treatment_vals[i], N)), nm = treatment)
    for (m in 1:M) {
      sim_data <- do.call(sim_mixed_dag, list(dag_model = dag_model, N = N, treatment_list = treatment_ls))
      results[m, i] <- mean(sim_data[[exposure]])
    }
  }
  ans$ATE <- diff(apply(results, 2, mean))

  return(ans)
}
