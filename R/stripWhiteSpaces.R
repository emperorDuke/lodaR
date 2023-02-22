#' Strip white spaces from characters
#'
#' This function removes white spaces from characters contained in a vector
#'
#' @param chr_vec a character vector
#' @param replace_with the character that will be used to replace the white spaces
#' @return a character vector
#' @export
strip_white_spaces <- function(chr_vec, replace_with = '') {
  chr_vec <- chr_vec[chr_vec != '']

  sapply(chr_vec, function(chr) {
    chrs <- unlist(stringi::stri_split_fixed(chr, pattern = ' '))
    chrs <- paste(chrs, collapse = replace_with)
    return(chrs)
  })
}
