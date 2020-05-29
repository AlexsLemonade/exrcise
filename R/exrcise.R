#' exrcise: A package to selectively excise code chunks for exercises.
#'
#'
#' @docType package
#' @name exrcise
NULL



#' Read an Rmd file and write a version with selective code chunks replaced
#'
#' @param infile a file string or file handle of a .Rmd file
#'     from which code chunks will be removed
#' @param outfile a file string or file handle of a .Rmd file
#'     to write the excised file
#' @param replace_flags a character vector of code chunk options
#'     that if set to TRUE will have their code replaced.
#' @param replacement a character vector that code chunks will be
#'     replaced with, one element per line.
#'
#'
#' @return a character vector of the output lines, invisibly.
#' @export
#'
#' @examples
#' exrcise(infile, outfile,
#'         replace_flags = "solution",
#'         replacement = "### Your Code Here")
exrcise <- function(infile, outfile, replace_flags, replacement = ""){
  lines <- readr::read_lines(infile)
  replaced_lines <- replace_code_chunks(lines, replace_flags, replacement = "")
  readr::write_lines(replaced_lines, outfile)
}
