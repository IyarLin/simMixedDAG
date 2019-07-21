#' @title Specify a parametric DAG model
#'
#' @description \code{parametric_dag_model} is used to specify paramteric DAG models.
#'   Any none specified function arguments are filled in using defaults.
#'   The resulting obejct of class "parametric_dag_model" has methods to simulate
#'   observations from the model specified and get it's \eqn{ATE}s.
#'
#' @details Every node \eqn{j} in a parametric DAG model is simulated from a function
#'   of the node parents \code{PA_{j}} and additional arguments. Denoting by
#'   \eqn{lp = \sum_{i in PA_{j}} \beta_{ij}x_i} the linear combination of the parent nodes, the function for a
#'   continuous node looks like the following:
#'
#'   \deqn{f(PA_{node}) = g(lp + \epsilon)}
#'
#'   and the following for a discrete node:
#'
#'   \deqn{f(PA_{node}) = cat(g(lp + \epsilon))}
#'
#'
#' \code{f.args} is a list where each element is in itself a list
#'   specifying the additional function arguments relating to the node
#'   after which that element is named after (this is a long sentence, see examples below).
#'
#'   The f.args elements can include the following optional elements:
#'
#'   1. \code{levels}: The number of levels a discrete node takes. \code{levels} = 1 means
#'   the node is continuous.
#'   1. \code{betas}: A list. Each element is a vector/scalar denoting the \eqn{\beta} coefficients
#'   of the parent node after which it is named.
#'   1. \code{link}: The link function \code{g}. One of "identity", "quadratic", "exp" or "cosin".
#'   1. \code{sinr}: Signal to ratio. This ensures that \eqn{var(lp) / var(\epsilon) = sinr}
#'
#' @param dag An object of class "dagitty" representing the DAG.
#' @param f.args A list specifying the different node function arguments.
#' @return An object of class "parametric_dag_model" which is essentially a list containing the following
#'   elements:
#'   1. dag: The model DAG
#'   1. f.args: The f.args input, with all non specified arguments set using defaults.
#' @example examples/example_sim_mixed_dag.parametric_dag_model.R
#' @seealso \code{\link{non_parametric_dag_model}} for non parametric DAG model specification. Methods for
#'   the parametric_dag_model class include \code{\link{sim_mixed_dag.parametric_dag_model}} for
#'   simulating datasets and \code{\link{get_ate.parametric_dag_model}} for getting \eqn{ATE}s.
#' @importFrom dagitty parents
#' @export

parametric_dag_model <- function(dag, f.args = NULL) {
  if (is.null(f.args)) f.args <- setNames(vector(mode = "list", length = length(names(dag))), nm = names(dag))
  if (sum(names(f.args) %in% names(dag)) < length(f.args)) stop("some variable entries in f.args don't match node names in supplied DAG")
  if (sum(duplicated(names(f.args))) > 0) stop("duplicate f.args variable entries")

  vars <- names(dag)
  parents <- setNames(lapply(vars, function(var) parents(dag, var)), vars)

  for (var in vars) {
    if (is.null(f.args[[var]]$levels)) f.args[[var]]$levels <- 1
    if (is.null(f.args[[var]]$labels) & f.args[[var]]$levels > 1) f.args[[var]]$labels <- LETTERS[1:f.args[[var]]$levels]
    for (parent in parents[[var]]) {
      parent_levels <- f.args[[parent]]$levels
      if (is.null(parent_levels)) f.args[[parent]]$levels <- 1
      parent_betas <- f.args[[var]]$betas[[parent]]
      if (is.null(parent_betas)) {
        f.args[[var]]$betas[parent] <- setNames(list(if (f.args[[parent]]$levels > 1) rnorm(f.args[[parent]]$levels - 1) else rnorm(1)),
          nm = parent
        )
      }
    }
    if (is.null(f.args[[var]]$link)) f.args[[var]]$link <- "identity"
    if (!f.args[[var]]$link %in% c("identity", "quadratic", "exp", "cosin")) stop(paste0("link argument in f.args for variable ,", var, " has to be one of identity, quadratic, exp, cosin"))
    if (is.null(f.args[[var]]$sinr)) f.args[[var]]$sinr <- 1
  }

  ans <- list(dag = dag, f.args = f.args)
  class(ans) <- "parametric_dag_model"
  return(ans)
}
