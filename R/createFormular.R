#' Create formula from character vector
#'
#' This function create formula from character vector of predicted (independent) variable using the additive process of a model
#' that can be excluded when not needed
#'
#' @param dep_var a string
#' @param indep_vars a character vector (independent variables)
#' @param exclude an optional character that will be excluded from the independent vars
#' @return a formula
#' @export
create_formular <- function(dep_var,
                            indep_vars,
                            exclude = NULL) {
  get_var <- function(var) deparse(substitute(var))

  dep_var_name <- dep_var

  if (is.data.frame(dep_var) || tibble::is_tibble(dep_var)) {
    dep_var_name <- get_var(dep_var)
  }

  if (!missing(exclude)) {
    indep_vars <- indep_vars[-which(indep_vars %in% exclude)]
  }


  full_formular <- paste(c(dep_var_name,
                           paste(indep_vars, collapse = " + ")),
                         collapse = " ~ ")

  return(stats::as.formula(full_formular))
}
