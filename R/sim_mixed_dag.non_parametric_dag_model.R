#' @title Simulate a dataset from a non parametric DAG model
#'
#' @description Simulate a dataset from a non parametric DAG model.
#'
#' @param dag_model An object of class "non_parametric_dag_model".
#' @param N Number of observations to simulate.
#' @param ... Additional arguments
#' @return A dataset simulated from the input \code{dag_model}.
#' @example examples/example_sim_mixed_dag.non_parametric_dag_model.R
#' @seealso \code{\link{sim_mixed_dag.parametric_dag_model}} for parametric dag simulation.
#' @import gam
#' @importFrom mgcv multinom
#' @importFrom stats predict rbinom rmultinom
#' @export

sim_mixed_dag.non_parametric_dag_model <- function(dag_model, N = 1000, ...) {
  dag <- dag_model$dag
  gam_fits <- dag_model$gam_fits
  if (mean(names(gam_fits) %in% names(dag)) != 1 | length(names(dag)) != length(names(gam_fits))) stop("some variable entries in gam_fits don't match node names in supplied DAG")
  
  env <- environment()
  if(length(list(...)==1)) list2env(list(...)[[1]], envir = env)
  vars <- names(dag)
  
  f_discrete <- function(gam_model, target_levels, parents, N, env) {
    if(length(parents) == 0){
      newdata <- data.frame(const = rep(1,N))
    } else {
      newdata <- list()
      for (parent in parents) {
        newdata[[parent]] <- try(eval(parse(text = parent), envir = env), silent = T)
        if (class(newdata[[parent]]) == "try-error") {
          if (gam_fits[[parent]]$node_type == "discrete") {
            assign(parent, do.call(f_discrete, list(
              gam_model = gam_fits[[parent]]$gam_model,
              parents = gam_fits[[parent]]$parents,
              target_levels = gam_fits[[parent]]$target_levels,
              N = N,
              env = env
            ), envir = env), pos = env)
          } else if (gam_fits[[parent]]$node_type == "continuous") {
            assign(parent, do.call(f_continuous, list(
              gam_model = gam_fits[[parent]]$gam_model,
              parents = gam_fits[[parent]]$parents,
              target_levels = gam_fits[[parent]]$target_levels,
              N = N,
              env = env
            ), envir = env), pos = env)
          }
          newdata[[parent]] <- eval(parse(text = parent), envir = env)
        }
      }
      
      newdata <- as.data.frame(lapply(newdata, function(x) replace(x, is.na(x), sample(x = x[!is.na(x)], sum(is.na(x)), replace = T)))) # TODO: figure out why this is needed
    }
    pred <- predict(gam_model, newdata, type = "response")
    if (length(target_levels) == 2) {
      return(target_levels[sapply(pred, function(p) rbinom(1, 1, p)) + 1])
    } else {
      return(factor(target_levels[apply(pred, 1, function(p_vec) which(rmultinom(1, 1, p_vec) == 1))], levels = target_levels))
    }
  }
  
  f_continuous <- function(gam_model, parents, N, env) {
    if(length(parents) == 0) {
      newdata <- data.frame(const = rep(1, N))
    } else {
      newdata <- list()
      for (parent in parents) {
        newdata[[parent]] <- try(eval(parse(text = parent), envir = env), silent = T)
        if (class(newdata[[parent]]) == "try-error") {
          if (gam_fits[[parent]]$node_type == "discrete") {
            assign(parent, do.call(f_discrete, list(
              gam_model = gam_fits[[parent]]$gam_model,
              parents = gam_fits[[parent]]$parents,
              target_levels = gam_fits[[parent]]$target_levels,
              N = N,
              env = env
            ), envir = env), pos = env)
          } else if (gam_fits[[parent]]$node_type == "continuous") {
            assign(parent, do.call(f_continuous, list(
              gam_model = gam_fits[[parent]]$gam_model,
              parents = gam_fits[[parent]]$parents,
              N = N,
              env = env
            ), envir = env), pos = env)
          }
          newdata[[parent]] <- eval(parse(text = parent), envir = env)
        }
      }
      newdata <- as.data.frame(newdata)
    }
    return(as.vector(predict(gam_model, newdata, type = "response")) + rnorm(n = nrow(newdata), sd = gam_model$sd))
  }
  
  for (var in vars) {
    if (!exists(var, envir = env)) {
      if (gam_fits[[var]]$node_type == "discrete") {
        assign(var, do.call(f_discrete, list(
          gam_model = gam_fits[[var]]$gam_model,
          parents = gam_fits[[var]]$parents,
          target_levels = gam_fits[[var]]$target_levels,
          N = N,
          env = env
        )), pos = env)
      } else if (gam_fits[[var]]$node_type == "continuous") {
        assign(var, do.call(f_continuous, list(
          gam_model = gam_fits[[var]]$gam_model,
          parents = gam_fits[[var]]$parents,
          N = N,
          env = env
        )), pos = env)
      }
    }
  }
  return(setNames(data.frame(lapply(vars, function(var) eval(parse(text = var)))), vars))
}
