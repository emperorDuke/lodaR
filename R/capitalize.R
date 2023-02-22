#' Transform first letter of a word or words to uppercase
#'
#' This function converts the first letter on a word to uppercase.
#'
#' @param chr_vec a character vector to be converted
#' @param all.fragments a logical value indicating if all the word fragments should be converted. It default to TRUE
#' @return a character vector
#' @export
capitalize <- function(char_vec, all.fragments = TRUE) {
  regex <- "[^a-zA-Z0-9\\(\\)]+"

  ## convert first letter of a word to uppercase
  transform <- function(str) {
    if (nchar(str) > 1) {
      letters <- unlist(stringi::stri_extract_all(str, regex = "[\\w\\(\\)]"))
      letters <- c(toupper(letters[1]), letters[2:length(letters)])
    } else {
      letters <- toupper(str)
    }
    return(paste(letters, collapse = ""))
  }


  ## sort and transform word fragments
  sort_fragments <- function(char_vec, all_fragments) {
    if (all_fragments) {
      char_vec <- sapply(char_vec, transform)
    } else {
      char_vec <- c(transform(char_vec[1]),
                    char_vec[2:length(char_vec)])
    }

    return(char_vec)
  }

  ## get character separating words
  get_seperator <- function(str) {
    stringi::stri_extract(str, regex = regex)
  }

  char_vec <- sapply(char_vec, function(chrs) {
    if (stringi::stri_detect(chrs, regex = regex)) {
      str <- unlist(stringi::stri_split(chrs, regex = regex))
      str <- paste(sort_fragments(str, all.fragments),
                   collapse = get_seperator(chrs))
    } else {
      str <- transform(chrs)
    }
  })

  return(char_vec)
}
