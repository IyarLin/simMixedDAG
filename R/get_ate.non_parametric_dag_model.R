#' @title Get \eqn{ATE} for a non paramteric DAG model
#'
#' @description \code{get_ate.non_parametric_dag_model} uses simulations
#'   to calculate the average treatment effect (\eqn{ATE}) of
#'   a treatment variable on an exposure variable given
#'   a non parametric DAG model.
#'
#' @param dag_model An object of class "non_parametric_dag_model".
#' @param treatment Name of a **single** treatment variable.
#' @param treatment_vals A vector of treatment values to be considered.
#' @param exposure Name of a **single** exposure variable.
#' @param M Number of simulations to run. Each simulation dataset consists of 1000 observations.
#' @return A data.frame with 3 columns:
#'   1. From: The baseline treatment value.
#'   1. To: The value a treatment was changed to.
#'   1. ATE: The average treatment effect of changing the treatment
#'   value from "From" to "To.
#' @example examples/example_get_ate.non_parametric_dag_model.R
#' @seealso \code{\link{get_ate.parametric_dag_model}} for parametric DAG model \eqn{ATE} calculation.
#' @importFrom dagitty parents
#' @importFrom stats quantile
#' @importFrom utils head tail
#' @export

get_ate.non_parametric_dag_model <- function(dag_model, treatment, treatment_vals = NULL, exposure, M = 1000) {
  N <- 1000
  dag <- dag_model$dag
  gam_fits <- dag_model$gam_fits
  if(length(treatment) > 1) stop("get_ate supports ate calculation for a single treatment only")
  if(length(exposure) > 1) stop("get_ate supports ate calculation for a single exposure only")
  if (gam_fits[[exposure]]$node_type != "continuous") stop("Exposure must be continuous")

  if (is.null(treatment_vals)) {
    if(gam_fits[[treatment]]$node_type == "continuous"){
    sample_treatment <- sim_mixed_dag(dag_model)[[treatment]]
    treatment_vals <- unname(quantile(sample_treatment, seq(0.05, 0.95, by = 0.3)))
    } else if (gam_fits[[treatment]]$node_type == "discrete"){
      treatment_vals <- factor(non_param_dag_model$gam_fits[[treatment]]$target_levels)
    }
  }

  results <- matrix(ncol = length(treatment_vals), nrow = M)
  ans <- data.frame(from = head(treatment_vals, -1), to = tail(treatment_vals, -1), ATE = NA)
  vars <- names(dag)
  vars <- vars[vars != treatment]
  parents <- setNames(lapply(vars, function(var) dagitty::parents(dag, var)), vars) # TODO don't simulate detached nodes after removing treatment

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
