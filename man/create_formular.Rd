% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createFormular.R
\name{create_formular}
\alias{create_formular}
\title{Create formula from character vector}
\usage{
create_formular(dep_var, indep_vars, exclude = NULL)
}
\arguments{
\item{dep_var}{a string}

\item{indep_vars}{a character vector (independent variables)}

\item{exclude}{an optional character that will be excluded from the independent vars}
}
\value{
a formula
}
\description{
This function create formula from character vector of predicted (independent) variable using the additive process of a model
that can be excluded when not needed
}
