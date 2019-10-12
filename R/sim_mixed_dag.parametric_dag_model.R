#' @title Simulate a dataset from a parametric DAG model
#'
#' @description Simulate a dataset from a parametric DAG model.
#'
#' @param dag_model An object of class "parametric_dag_model".
#' @param N Number of observations to simulate.
#' @param ... Additional arguments.
#' @return A dataset simulated from the input \code{dag_model}.
#' @example examples/example_sim_mixed_dag.parametric_dag_model.R
#' @seealso \code{\link{sim_mixed_dag.non_parametric_dag_model}} for non parametric dag simulation.
#' @importFrom dagitty parents
#' @export

sim_mixed_dag.parametric_dag_model <- function(dag_model, N = 1000, ...) {
  dag <- dag_model$dag
  f.args <- dag_model$f.args
  if (sum(names(f.args) %in% names(dag)) < length(f.args)) stop("some variable entries in f.args don't match node names in supplied DAG")

  env <- environment()
  if (length(list(...) == 1)) list2env(list(...)[[1]], envir = env)
  vars <- names(dag)
  parents <- setNames(lapply(vars, function(var) parents(dag, var)), vars)
  if (sum(duplicated(names(f.args))) > 0) stop("duplicate f.args variable entries")

  f <- function(vars, link = "identity", levels = 1L, betas = NULL, labels = NULL, sinr = 1, env = NULL, result_var = NULL) {
    if (!link %in% c("identity", "quadratic", "exp", "cosin")) stop(paste0("link argument in f.args for variable \"", result_var, "\" has to be one of identity, quadratic, exp, cosin"))
    lp <- rep(0, N)
    for (var in vars) {
      var_val <- try(eval(parse(text = var), envir = parent.frame(1L)), silent = T)
      if (class(var_val) == "try-error") {
        assign(var, do.call(f, append(list(vars = parents[[var]], result_var = var), f.args[[var]])), envir = pos.to.env(-1L))
        assign(var, eval(parse(text = var)), envir = env)
        var_val <- eval(parse(text = var))
      }
      if (is.null(betas[[var]])) {
        beta <- as.matrix(if (is.factor(var_val)) rnorm(length(levels(var_val)) - 1) else rnorm(1))
      } else {
        beta <- as.matrix(betas[[var]])
      }
      X <- model.matrix(~ var_val - 1)
      if (is.factor(var_val)) X <- as.matrix(X[, -1])
      if (ncol(X) != nrow(beta)) stop(paste0("Number of levels in \"", var, "\" does not match number of input betas for that variable"))
      lp <- lp + as.numeric(X %*% beta)
    }

    if (link == "quadratic") lp <- sign(lp) * lp^2
    if (link == "cosin") lp <- cos(lp * 2)
    if (link == "exp") lp <- exp(lp) - 1

    if (var(lp) == 0) {
      lp <- lp + rnorm(length(lp), sd = 1) # add noise
    } else {
      lp <- lp + rnorm(length(lp), sd = sqrt(var(lp) / sinr)) # add noise
    }

    if (levels > 1) {
      if (is.null(labels)) labels <- LETTERS[1:levels]
      if (length(labels) != levels) stop(paste0("labels do not match levels argument for variable \"", result_var, "\""))
      thresh <- qnorm(p = seq(0, 1, length.out = levels + 1))
      lp <- cut(lp, breaks = thresh, labels = labels, include.lowest = T)
    }
    return(lp)
  }

  for (var in vars) {
    if (!exists(var, envir = env, mode = "numeric")) {
      assign(var, do.call(f, append(list(vars = parents[[var]], env = env, result_var = var), f.args[[var]])), envir = env)
    }
  }
  return(setNames(data.frame(lapply(vars, function(var) eval(parse(text = var)))), vars))
}
