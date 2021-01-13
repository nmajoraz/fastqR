#' Converts quality values to Phred score
#'
#' Converts a scalar string of quality values into a vector of Phred scores.
#'
#' @param qualities A character vector of qualities
#' @param offset Must be a value of 33 or 64
#'
#' @return A vector of Phred scores
#' @export
#'
#' @examples decode_qualities("???#;ABAAAH")
decode_qualities <- function(qualities, offset = 33){
  assertthat::assert_that(assertthat::is.scalar(offset))
  assertthat::assert_that(assertthat::is.number(offset))
  if(offset == 33 || offset == 64){
    phred_score <- base::as.integer(base::charToRaw(qualities)) - offset
    if(base::any(phred_score < 1)){
      stop("Negative phred score(s) produced. Please check offset")
    } else {
      return(phred_score)
    }
  } else {
    stop("Invalid offset. Choose value of 33 or 64")
  }
}
