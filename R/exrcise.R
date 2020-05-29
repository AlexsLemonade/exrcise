
#' Read an Rmd file and write a version with selective code chunks replaced
#'
#' @param infile
#' @param outfile
#' @param replace_flags
#' @param replacement
#'
#' @return
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
