#'  Converts long biological names to their short version
#'
#' This function converts long botanical names to their short variant.
#'
#' @param names a character vector to be converted
#' @return a character vector
#' @export
abbreviate_species <- function(names) {
  regx <- '(?<!\\w)[\\w]{1,1}|(\\s|\\.)[\\w]+'

  sapply(names, function(name) {
    strs_vec <- stringi::stri_extract_all(name, regex = regx)
    strs_vec <- lodaR::strip_white_spaces(unlist(strs_vec))
    strs_vec <- lodaR::capitalize(strs_vec, all = FALSE)

    ## check if there is a dot in the species names
    ## the name might be formatted already or contains dot for no reason
    ## before getting to this function
    ## revert the formatting before proceeding
    if (stringi::stri_detect_fixed(name, pattern = ".")) {
      strs_vec <- unlist(stringi::stri_split_fixed(extract_char(strs_vec),
                                         pattern = '.'))
    }

    ## check if there is a variant name after species name
    if (length(strs_vec) > 2) {
      strs_vec <- c(strs_vec[1], paste(strs_vec[2:3], collapse = " "))
    }

    return(paste(strs_vec, collapse = "."))
  })
}
