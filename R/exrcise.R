#' exrcise: A package to selectively excise code chunks for exercises.
#'
#'
#' @docType package
#' @name exrcise
NULL



#' Read an Rmd file and write a version with selective code chunks replaced
#'
#' @param infile A file path or file handle of an .Rmd file
#'     from which code chunks will be removed
#' @param outfile A file path or file handle to which to
#'     write the .Rmd file with replacements.
#' @param replace_flags A character vector of code chunk options
#'     that if set to TRUE will have their code replaced.
#' @param replacement A character vector that code chunks will be
#'     replaced with, one element per line.
#' @param comment TRUE or FALSE; whether to include comments.
#'     Default is TRUE
#'
#'
#' @return A character vector of the output lines, invisibly.
#' @export
#'
#' @examples
#' exrcise(infile, outfile,
#'     replace_flags = "live",
#'     replacement = "")
#'
#' exrcise(infile, outfile,
#'     replace_flags = "solution",
#'     replacement = "### Your Code Here",
#'     comment = FALSE)
#'
exrcise <- function(infile, outfile,
                    replace_flags,
                    replacement = "",
                    comment = TRUE){
  lines <- readr::read_lines(infile)
  replaced_lines <- replace_code_chunks(lines,
                                        replace_flags,
                                        replacement,
                                        comment)
  readr::write_lines(replaced_lines, outfile)
}
