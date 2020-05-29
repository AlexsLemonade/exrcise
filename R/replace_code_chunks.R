#' Strip an rmarkdown represented as a vector of lines of solution code
#'
#' @param lines 
#' @param replace_flags 
#' @param replacement 
#'
#' @return A vector of lines that forms a new .Rmd document
#' @export
#'
#' @examples replace_code_chunks(lines, replace_flags = "solution", replacement = "### Your Code Here")

replace_code_chunks <- function(lines, replace_flags, replacement = "") {
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
             replacement = replacement) %>%
    unlist()
}

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


#' Title
#'
#' @param block 
#' @param replace_flags 
#' @param replacement 
#'
#' @return
#' 
#' @importFrom magrittr %>%
#' @export
replace_block <- function(block, replace_flags, replacement = "") {
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
      block <- c(block[1], 
                replacement,
                block[length(block)]) #return only the boundaries of the blocks
    }
  } 
  return(block)
}
