decode_qualities <- function(qualities, offset = 33){
  assertthat::assert_that(assertthat::is.scalar(offset))
  assertthat::assert_that(assertthat::is.number(offset))
  if(offset == 33 || offset == 64){
    phred_score <- as.integer(charToRaw(qualities)) - offset
    if(any(phred_score < 1)){
      stop("Negative phred score(s) produced. Please check offset")
    } else {
      return(phred_score)
    }
  } else {
    stop("Invalid offset. Choose value of 33 or 64")
  }
}