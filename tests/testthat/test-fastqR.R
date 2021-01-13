library(fastqR)
library(testthat)

test_that("GC Content", {
  expect_error(gc_content(c(2, 5, 12)))
  expect_warning(gc_content(c("GAGCTG", "GGNNAGAT", "gattaggac","gttgatgat")))

  expect_equal(round(gc_content(c("GAGCTG", "GGTAGAT", "gattaggac","gttgatgat")), 1),
               c(66.7, 42.9, 44.4, 33.3))
})

test_that("Reading fastq file", {
  expect_output(str(
    suppressWarnings(read_fastq(system.file("good.fq", package = "fastqR"))),
    "tibble"))

  expect_warning(read_fastq(system.file("good.fq", package = "fastqR")))
  expect_error(read_fastq(system.file("broken_format.fq", package = "fastqR")))
  expect_error(read_fastq(system.file("duplicate_ids.fq", package = "fastqR")))
  expect_error(read_fastq(system.file("mismatched_lengths.fq", package = "fastqR")))
  expect_error(read_fastq(system.file("wrong_extension.fa", package = "fastqR")))
})

test_that("Quality decoding", {
  expect_equal(decode_qualities("???#;ABAAAH"),
               c(30, 30, 30, 2, 26, 32, 33, 32, 32, 32, 39))
  expect_equal(decode_qualities("ABAAAH", 64),
               c(1, 2, 1, 1, 1, 8))


  expect_error(decode_qualities("???#;ABAAAH", offset = "sanger"))
  expect_error(decode_qualities("???#;ABAAAH", offset = c(33, 64)))
  expect_error(decode_qualities("???#;ABAAAH", offset = 63))
})
