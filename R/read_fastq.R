#' % of GC values in a DNA sequence
#'
#' Provides the percentage of bases with values of G or C in a DNA sequence.
#'
#' @param seq A vector of DNA sequence strings
#'
#' @return Percentage of bases that are G or C as a vector.
#' @export
#'
#' @examples
#' gc_content("GAGAGCGGCTT")
gc_content <- function(seq){
  if (base::is.character(seq)){
    seq <- base::toupper(seq)
    if(base::any(stringr::str_detect(seq, "[^GATC]"))){
      warning("Only GATC bases are accepted; non GATC characters found")
    }

    gc <-stringr::str_replace_all(seq,"[^GC]","")
    return((base::nchar(gc) / base::nchar(seq)) * 100)

  } else {
    stop("Please pass in a character vector")
  }
}


#' Reads fastq file and puts into tibble
#'
#' Reads a fastq file and puts into a tibble with one row per fastq entry.
#'
#' @param file The fastq file to read from. Requires extension of .fq.
#'
#' @return A tibble with 4 columns. \cr
#'         1) ID: The sequence from the first line minus the "@" \cr
#'         2) Bases: The bases from the second line \cr
#'         3) Qualities: The quality string from the fourth line \cr
#'         4) GC: The GC content of the bases \cr
#' @export
#'
#' @examples
#' read_fastq(system.file("good.fq", package = "fastqR"))
read_fastq <- function(file){
  assertthat::assert_that(assertthat::is.readable(file))
  assertthat::assert_that(assertthat::has_extension(file, "fq"))

  lines <- base::scan(file, base::character())
  ids <- lines[c(TRUE, FALSE, FALSE, FALSE)]
  bases <- lines[c(FALSE, TRUE, FALSE, FALSE)]
  qualities <- lines[c(FALSE, FALSE, FALSE, TRUE)]

  # ID verification
  if(!base::all(base::startsWith(ids, "@"))){
    stop("ID(s) missing the starting '@'")
  }

  ids <- stringr::str_sub(ids, 2)

  if(base::any(base::duplicated(ids))){
    stop("Duplicated IDs exist in file")
  }

  # Base/Qualities verification
  if(base::any(base::nchar(bases) != base::nchar(qualities))){
    stop("Length(s) of bases and qualities don't align")
  }

  gc <- gc_content(bases)

  return(tibble::tibble(ID = ids, Bases = bases, Qualities = qualities, GC = gc))
}
