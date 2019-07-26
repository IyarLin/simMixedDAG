#' @title Specify a non parametric DAG model
#'
#' @description \code{non_parametric_dag_model} is used to specify non paramteric
#'   DAG models. The non parametric node functions are obtained by fitting generalized
#'   additive models to a supplied (usually "real life") dataset.
#'   The resulting obejct of class "non_parametric_dag_model" has methods to simulate
#'   observations from the model specified and get \eqn{ATE}s.
#'
#' @details Every node \eqn{j} in a non parametric DAG model is simulated from a
#'   non paramteric function of the node parents \code{PA_{j}}:
#'
#'   \deqn{f_j(PA_j) = g(\sum_{i \in PA_j}s_{ij}(x_i) + \epsilon_j)}
#'
#'   The smooth functions \eqn{s_{ij}(.)} are obtained by fitting generalized
#'   additive model to the node (as a reponse) and it's parents (as predictors).
#'
#' @param dag An object of class "dagitty" representing the DAG.
#' @param data A data.frame to be used for fitting the GAMs (no missing values allowed).
#' @return An object of class "non_parametric_dag_model" which is essentially a list containing the following
#'   elements:
#'   1. dag: The model DAG
#'   1. gam_fits: All smoothing functions and other information required for simulating
#'     new datasets from the specified dag model
#' @example examples/example_sim_mixed_dag.non_parametric_dag_model.R
#' @seealso \code{\link{parametric_dag_model}} for parametric DAG model specification. Methods for
#'   the non_parametric_dag_model class include \code{\link{sim_mixed_dag.non_parametric_dag_model}} for
#'   simulating datasets and \code{\link{get_ate.non_parametric_dag_model}} for getting \eqn{ATE}s.
#' @importFrom dagitty parents
#' @import gam
#' @importFrom mgcv gam gam.fit s gam.control
#' @importFrom stats as.formula sd
#' @export


non_parametric_dag_model <- function(dag, data) {
  if (mean(names(dag) %in% names(data)) != 1) stop("Some DAG nodes not found in data")
  if (any(is.na(data))) stop("data contains missing values")
  data <- data[names(dag)]
  character_variables <- which(sapply(data, is.character))
  data[character_variables] <- lapply(data[character_variables], function(x) factor(x))
  two_level_variables <- which(sapply(data, function(x) length(unique(x)) == 2))
  data[two_level_variables] <- lapply(data[two_level_variables], function(x) factor(x)) # code 2 value variables as binary
  
  vars <- names(dag)
  ans <- setNames(object = vector(mode = "list", length = length(vars)), nm = vars)
  
  for (var in vars) {
    var_parents <- parents(dag, var)
    ans[[var]]$parents <- var_parents
    
    if (class(data[[var]]) == "factor") {
      ans[[var]]$node_type <- "discrete"
      ans[[var]]$target_levels <- levels(data[[var]])
      num_levels <- length(levels(data[[var]]))
      if(length(var_parents) == 0){
        forms <- " ~ 1"
      } else {
        forms <- paste0(" ~ ", paste0(sapply(var_parents, function(var_parent) {
          if (class(data[[var_parent]]) == "factor") {
            return(var_parent)
          } else {
            return(paste0("s(", var_parent, ")"))
          }
        }), collapse = " + "))
      }
      forms <- lapply(unlist(strsplit(paste0(var, paste0(rep(forms, num_levels - 1), collapse = ", ")), ",")), as.formula)
      if(length(forms) == 1) forms <- forms[[1]]
      dat <- data[c(var, var_parents)]
      dat[[var]] <- as.integer(dat[[var]]) - 1
      ans[[var]]$gam_model <- gam(forms, family = if (num_levels == 2) "binomial" else multinom(K = num_levels - 1), data = dat)
    } else {
      ans[[var]]$node_type <- "continuous"
      form <- as.formula(paste0(var, " ~ ", paste0(sapply(var_parents, function(var_parent) {
        if (class(data[[var_parent]]) == "factor") {
          return(var_parent)
        } else {
          return(paste0("s(", var_parent, ")"))
        }
      }), collapse = " + ")))
      dat <- data[c(var, var_parents)]
      ans[[var]]$gam_model <- gam(formula = form, family = "gaussian", data = dat)
      ans[[var]]$gam_model$sd <- sd(predict(ans[[var]]$gam_model) - dat[[var]])
    }
  }
  ans <- list(dag = dag, gam_fits = ans)
  class(ans) <- "non_parametric_dag_model"
  return(ans)
}
