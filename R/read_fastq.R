gc_content <- function(seq){
  if (is.character(seq)){
    seq <- toupper(seq)
    if(any(stringr::str_detect(seq, "[^GATC]"))){
      warning("Only GATC bases are accepted; non GATC characters found")
    }

    gc <-stringr::str_replace_all(seq,"[^GC]","")
    return((nchar(gc) / nchar(seq)) * 100)

  } else {
    stop("Please pass in a character vector")
  }
}

# Write read_fastq
read_fastq <- function(file){
  assertthat::assert_that(assertthat::is.readable(file))
  assertthat::assert_that(assertthat::has_extension(file, "fq"))

  lines <- scan(file, character())
  ids <- lines[c(TRUE, FALSE, FALSE, FALSE)]
  bases <- lines[c(FALSE, TRUE, FALSE, FALSE)]
  qualities <- lines[c(FALSE, FALSE, FALSE, TRUE)]

  # ID verification
  if(!all(startsWith(ids, "@"))){
    stop("ID(s) missing the starting '@'")
  }

  ids <- stringr::str_sub(ids, 2)

  if(any(duplicated(ids))){
    stop("Duplicated IDs exist in file")
  }

  # Base/Qualities verification
  if(any(nchar(bases) != nchar(qualities))){
    stop("Length(s) of bases and qualities don't align")
  }

  gc <- gc_content(bases)

  return(tibble::tibble(ID = ids, Bases = bases, Qualities = qualities, GC = gc))
}
