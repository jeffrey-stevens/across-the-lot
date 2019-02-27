library(stringr)

well_names <- 
  expand.grid(AssayCol=1:12, AssayRow=1:8) %>%
  mutate(Well=paste0(chartr(paste0(1:8, collapse=""),
                            paste0(LETTERS[1:8], collapse=""),
                            AssayRow),
                     AssayCol),
         WellOrder=as.numeric(seq_len(n())))



parse_group_list <- function(expr) {
  # Split the string
  exprs <- str_trim(str_split(expr, ",")[[1]])
  
  # Parse the groups
  groups <- 
    lapply(exprs,
           function(e) {
             parse_group(e)
           })
  
  return(groups)
}


parse_group <- function(expr) {
  
}


parse_expr <- function(expr) {
  expr <- str_trim(expr)
  
  # Is it a term?
  
  
}


is_term <- function(expr) {
  expr <- str_trim(expr)
  
}


# Not finished!!!
tokenize <- function(expr) {
  tokens <- list()
  
  n <- str_length(expr)
  i <- 1L
  while (i <= n) {
    ch <- substr(expr, i, i)
    if (ch %in% c("+", "-", ":", "(", ")")) {
      tokens <- append(tokens, ch)
    } else if (ch %in% LETTERS[1:8]) {
      # Need to look ahead
      if (identical(i, n)) {
        stop("Invalid expression")
      }
      i <- i + 1
      ch2 <- substr(expr, i, i)
      tokens <- append(tokens, paste0())
    }
    i <- i + 1
  }
}