
#' Checks if x is NULL or NA
#'
#' @param x A object.
#' @return TRUE/FALSE.
#' @examples
#' isNULLorNA(NULL)
isNULLorNA <- function(x){
  return(
    is.null(x)|(if(length(is.na(x)) == 0){FALSE}else{is.na(x)})
  )
}

#' Pastes the names of a object into a string, optionally quoting the names.
#'
#' @param x A named object (vector, list, data.frame)
#' @param collapse A string to separate the collapsed names.
#' @param quote TRUE/FALSE, if TRUE, adds quotes to the names.
#' @return A string.
#' @examples
#' namesToString(c("test" = 1, "this" = 2))
namesToString <- function(x, collapse = ",", quote = FALSE){
  return(
    if(quote){
      paste0("'", names(x), "'", collapse = collapse)
    }else{
      paste0(names(x), collapse = collapse)
    }
  )
}

#' Generates (pseudo)random strings of the specified char length
#'
#' @param n_char A integer, the number of chars to include in the output string.
#' @param sample_chars A vector of characters to sample from. Includes the lowercase and uppercase English alphabet and 0-9 by default.
#' @return A string.
#' @examples
#' sampleStr(10)
sampleStr <- function(n_char, sample_chars = c(letters, LETTERS, 0:9)){
  x <- c()
  for(i in seq_len(n_char)){
    x <- c(x, sample(sample_chars, 1))
  }
  return(
    paste0(x, collapse = "")
  )
}

#' Add single quotes to strings, useful for converting R strings into SQL formatted strings.
#'
#' @param x A string.
#' @param char_only TRUE/FALSE, if TRUE, adds quotes only if is.character(x) is TRUE.
#' @param excluded_chars A character vector, will not add quotes if a value is in excluded_chars.
#' @param null_or_na_as_NULL TRUE/FALSE, if TRUE, NULL and NA values are replaced with the string "NULL".
#' @return A string, with single quotes added to match PostgreSQL string formatting.
#' @examples
#' quoteText("Sample quotes.")
quoteText <- function(
  x,
  char_only = TRUE,
  excluded_chars = c("NULL"),
  null_or_na_as_NULL = TRUE
){
  if(char_only == TRUE){
    x[is.character(x) & !(x %in% excluded_chars) & !(is.null(x) == TRUE|is.na(x) == TRUE)] <- paste0("'", gsub("\'", "''", x[is.character(x) & !(x %in% excluded_chars) & !(is.null(x) == TRUE|is.na(x) == TRUE)]), "'")
  }else{
    x[!(x %in% excluded_chars) & !(is.null(x) == TRUE|is.na(x) == TRUE)] <- paste0("'", gsub("\'", "''", x[!(x %in% excluded_chars) & !(is.null(x) == TRUE|is.na(x) == TRUE)]), "'")
  }
  if(null_or_na_as_NULL){
    x[is.null(x) == TRUE|is.na(x) == TRUE] <- "NULL"
  }
  return(
    x
  )
}

#' Add double quotes to strings.
#'
#' @param x A string.
#' @param char_only TRUE/FALSE, if TRUE, adds quotes only if is.character(x) is TRUE.
#' @param excluded_chars A character vector, will not add quotes if a value is in excluded_chars.
#' @param null_or_na_as_NULL TRUE/FALSE, if TRUE, NULL and NA values are replaced with the string "NULL".
#' @return A string, with double quotes added.
#' @examples
#' doubleQuoteText("Sample quotes.")
doubleQuoteText <- function(
  x,
  char_only = TRUE,
  excluded_chars = c("NULL"),
  null_or_na_as_NULL = TRUE
){
  if(char_only == TRUE){
    x[is.character(x) & !(x %in% excluded_chars) & !(is.null(x) == TRUE|is.na(x) == TRUE)] <- paste0('"', gsub('\"', '""', x[is.character(x) & !(x %in% excluded_chars) & !(is.null(x) == TRUE|is.na(x) == TRUE)]), '"')
  }else{
    x[!(x %in% excluded_chars) & !(is.null(x) == TRUE|is.na(x) == TRUE)] <- paste0('"', gsub('\"', '""', x[!(x %in% excluded_chars) & !(is.null(x) == TRUE|is.na(x) == TRUE)]), '"')
  }
  if(null_or_na_as_NULL){
    x[is.null(x) == TRUE|is.na(x) == TRUE] <- "NULL"
  }
  return(
    x
  )
}

#' Format data as a JSON object (like this: {"x": "120"}).
#'
#' @param name A string, the name of the JSON entry
#' @param val A string, the value to associate with the JSON entry.
#' @return A string, data formatted as a JSON object.
#' @examples
#' jsonStr(name = "var1", val = "Blue")
jsonStr <- function(name, val){
  return(paste0("{", doubleQuoteText(name), ":", doubleQuoteText(val), "}"))
}

#' Convert strings to numeric if possible, otherwise remains as is.
#'
#' @param x A string.
#' @return A string, converted to numeric if possible.
#' @examples
#' castNumeric("100")
castNumeric <- function(x){
  if(length(x) > 0){
    suppressWarnings(
      if(length(x[isNULLorNA(as.numeric(x)) == TRUE]) == 0){
        x <- as.numeric(x)
      }
    )
  }else{
    suppressWarnings(
      if(isNULLorNA(as.numeric(x)) == FALSE){
        x <- as.numeric(x)
      }
    )
  }
  return(x)
}

#' Convert strings to logical.
#'
#' @param x A string.
#' @return A string, converted to logical.
#' @examples
#' castLogical("1")
castLogical <- function(x){
  return(
    as.logical(castNumeric(x))
  )
}

#' Format a date string as "%Y-%m-%d" (YYYY-MM-DD), useful for converting dates selected
#' from a SQL database to a format compatible with a HTML date input value.
#'
#' @param x A string.
#' @return A string, formatted YYYY-MM-DD.
#' @examples
#' castDateString(Sys.time())
castDateString <- function(x){
  return(
    if(nchar(x) > 0){
      format(x, "%Y-%m-%d")
    }else{
      x
    }
  )
}

#' Get the names of the arguments to a function
#'
#' @param x A function or string naming a function.
#' @return A vector of the names of the arguments to a function.
#' @examples
#' argNames("readLines")
argNames <- function(x){
  return(
    names(formals(match.fun(x)))
  )
}

#' Group items of a list by name
#'
#' @param x A named list, likely with names repeating for different positions.
#' @return A list with items consolidated by name.
#' @examples
#' consolidateList(list("col1" = "Test", "col2" = "Hello", "col1" = "Repeated Name"))
consolidateList <- function(x){
  x_names <- names(x)
  ux_names <- unique(x_names)
  if(length(ux_names) < length(x_names)){
    x <- lapply(ux_names, function(y){
      return(unlist(x[names(x) %in% y]))
    })
    names(x) <- ux_names
  }
  return(x)
}

#' Create a named list of length 1 using a name stored in a variable as the name.
#'
#' @param name The name for the item in the list.
#' @param x The item to put in the list.
#' @return A named list.
#' @examples
#' argumentNamedList("test_name", 1)
argumentNamedList <- function(name, x){
  x <- list(x)
  names(x) <- name
  return(x)
}

#' Extract the values from each entry in a list of vectors at a specific index
#'
#' @param x A list, each item of the list should have equal length.
#' @param pos A integer, the position to extract from each entry in the list.
#' @return A list.
#' @examples
#' listExtract(list(col1 = c(1, 2, 3, 4, 5), col2 = c("a", "b", "c", "d", "e")), 3)
listExtract <- function(x, pos){
  return(
    lapply(x, `[[`, pos)
  )
}

#' Filters the argument list to match the arguments in what and then calls do.call.
#'
#' @param what See do.call.
#' @param args Argument list, gets filtered to match arguments of what. See do.call.
#' @param quote See do.call.
#' @param envir See do.call.
#' @return See do.call.
#' @seealso do.call
#' @examples
#' do.call2(intersect, list(x = c(1, 2, 3), y = c(2)))
do.call2 <- function(what, args, quote = FALSE, envir = parent.frame()){
  args <- args[names(args) %in% names(formals(match.fun(what)))|("..." %in% names(formals(match.fun(what))))]
  return(do.call(what = what, args = args, quote = quote, envir = envir))
}

#' Paste together columns of a list/data frame
#'
#' @param x A list or data frame.
#' @param sep A character sting to separate the terms.
#' @param collapse An optional character string to separate the results.
#' @param use_paste0 Boolean, if TRUE, will call paste0 instead of paste.
#' @param cols An optional vector of column positions or names to paste together. If passing column names, set by_name to TRUE.
#' The order of items in cols determines the order of the paste result.
#' @param by_name Boolean, if TRUE, it quotes the items in cols to properly index the list by name (x[[1]] vs x[["col_a"]]).
#' @return A string with the values in each column pasted together.
#' @examples
#' pasteCols(list("x" = c(1, 2, 3), "y" = c("a", "b", "c")))
pasteCols <- function(
  x,
  sep = " ",
  collapse = NULL,
  use_paste0 = FALSE,
  cols = NULL,
  by_name = FALSE
){
  if(is.null(cols) == TRUE){
    cols <- seq_len(length(x))
  }else{
    if(by_name == TRUE){
      cols <- shQuote(cols)
    }
  }
  if(use_paste0 == FALSE){
    str <- paste0(
      "paste(", paste0("x[[", cols, "]]", collapse = ", "), ", sep = ", shQuote(sep), if(is.null(collapse) == FALSE){paste0(", collapse = ", shQuote(collapse))}else{""}, ")"
    )
  }else{
    str <- paste0(
      "paste0(", paste0("x[[", cols, "]]", collapse = ", "), if(is.null(collapse) == FALSE){paste0(", collapse = ", shQuote(collapse))}else{""}, ")"
    )
  }
  return(
    eval(str2expression(str))
  )
}

#' Combine columns of a list/data frame into a list by row
#'
#' @param x A list or data frame.
#' @param cols An optional vector of column positions or names to combine together. If passing column names, set by_name to TRUE.
#' The order of items in cols determines the order of the combined result.
#' @param by_name Boolean, if TRUE, it quotes the items in cols to properly index the list by name (x[[1]] vs x[["col_a"]]).
#' @param parallel Boolean, if TRUE, attempts to use mclapply.
#' @param cores An integer, the number of cores to use if parallel is TRUE.
#' @return A list of the values in each column combined together for each row.
#' @examples
#' combineCols(list("x" = c(1, 2, 3), "y" = c("a", "b", "c")))
combineCols <- function(
  x,
  cols = NULL,
  by_name = FALSE,
  parallel = FALSE,
  cores = 1
){
  rows <- unique(unlist(lapply(x, length)))
  if(length(rows) > 1){
    stop("columns must be of equal length")
  }
  if(is.null(cols) == TRUE){
    cols <- seq_len(length(x))
  }else{
    if(by_name == TRUE){
      cols <- shQuote(cols)
    }
  }
  if(parallel == FALSE){
    str <- paste0(
      "lapply(seq_len(", rows, "), function(i, l = x){return(c(", paste0("l[[", cols, "]][i]", collapse = ", "), "))})"
    )
  }else{
    parallel::mclapply
    str <- paste0(
      "parallel::mclapply(seq_len(", rows, "), function(i, l = x){return(c(", paste0("l[[", cols, "]][i]", collapse = ", "), "))}, mc.cores = ", cores, ")"
    )
  }
  return(
    eval(str2expression(str))
  )
}

#' Paste parts of file paths/urls separated with single forward-slashes
#'
#' @param ... Text strings to combine into a file path
#' @return A string.
#' @examples
#' pastePaths("/home/", "/files")
pastePaths <- function(...){
  return(
    gsub("/+", "/", paste0(c(...), collapse = "/"))
  )
}
