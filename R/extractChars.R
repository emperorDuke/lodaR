#' Extract characters of interest
#'
#' This function extracts characters matching the reg-ex pattern.
#' It is generally a simple wrapper around the stringi::extract_all
#' package.
#'
#' @param chrs_vec a character vector
#' @param regx a reg-ex pattern
#' @param bind_with an optional character that will be used to bind the fragments
#' @return a character vector
#' @export
extract_chars <- function(chrs_vec,
                         regx = '[a-zA-Z]+',
                         bind_with = '.') {
  sapply(chrs_vec, function(chr) {
    chrs <- chr

    if (!is.na(chr) && stringi::stri_detect_regex(chr, pattern = regx)) {
      chrs <- unlist(stringi::stri_extract_all(chr, regex = regx))
      chrs <- paste(chrs, collapse = bind_with)
    }

    return(chrs)
  })
}
