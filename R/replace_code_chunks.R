#' Replace code from an rmarkdown document as a vector of lines
#'
#' @param lines
#' @param replace_flags
#' @param replacement
#' @param comment
#'
#' @return A vector of lines that forms the content of a new .Rmd document
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' replace_code_chunks(lines,
#'     replace_flags = "solution",
#'     replacement = "### Your Code Here")
replace_code_chunks <- function(lines,
                                replace_flags,
                                replacement = "",
                                comment = TRUE) {
  chunk.begin <- knitr::all_patterns$md$chunk.begin
  chunk.end <- knitr::all_patterns$md$chunk.end

  starts <- grepl(chunk.begin, lines)
  ends <- grepl(chunk.end, lines) # this will also include fenced blocks not part of a chunk
  ends <- filter_ends(starts, ends)
  # mark the starting lines of a code/text block
  tmp <- starts | head(c(TRUE, ends), -1)
  blocks <- unname(split(lines, cumsum(tmp)))
  
  purrr::map(blocks, replace_block,
             replace_flags = replace_flags,
             replacement = replacement,
             comment = comment) %>%
    unlist()
}

# filter a boolean vector of potential chunk ends to only the real ones.
filter_ends <- function(starts, ends){
  start_locs <- which(starts)
  end_locs <- which(ends)
  filtered_end_locs <- purrr::map_int(start_locs, function(s){
    as.integer(min(end_locs[end_locs > s]))
  })
  filtered_ends <- rep(FALSE, length(ends))
  filtered_ends[filtered_end_locs] <- TRUE
  return(filtered_ends)
}


#' Conditionally replace code in a block of lines
#'
#' @param block
#' @param replace_flags
#' @param replacement
#' @param comment
#'
#' @return
#'
#' @importFrom magrittr %>%
#' @export
replace_block <- function(block, replace_flags, replacement, comment) {
  chunk.begin <- knitr::all_patterns$md$chunk.begin
  is_chunk = grepl(chunk.begin, block[1])
  if (is_chunk) {
    params <- block[1] %>%
      stringr::str_replace_all(chunk.begin, '\\1') %>%
      stringr::str_trim() %>%
      stringr::str_replace('^([a-zA-Z0-9_]+)', '') %>% # strip engine
      stringr::str_replace_all('^\\s*,*|,*\\s*$', '') %>% # remove empty options
      knitr:::parse_params()
    # if any of the replace_flags are found
    # replace the chunk code with the replacement string
    if (any(unlist(params[replace_flags]))){
      block <- c(block[1], # chunk header
                 # chunk body, skipping header and footer
                 replace_code(head(block[-1], -1),
                              replacement = replacement,
                              comment = comment),
                 tail(block, 1)) # chunk footer
    }
  }
  return(block)
}

# replace the R code, optionally preserving comments
replace_code <- function(code, replacement = "", comment = TRUE){
  if (!comment) {
    return(replacement)
  } else {
    comment_lines <- grepl("^\\s*#", code)
    code_lines <- !comment_lines
    code[code_lines] <- replacement
    # code lines to be included, one after each set of comments
    code_after_comment <- code_lines & head(c(TRUE, comment_lines), - 1)
    return(code[comment_lines | code_after_comment])
  }
}
