# Adopted from vcr
`%||%` <- function(x, y) {
  if (is.null(x) || all(nchar(x) == 0) || length(x) == 0) y else x
}

# Convert base pairs to megabases and back
to_mb <- function(x) ceiling(x * 1e-6)
to_bp <- function(x) x * 1e6


# Time-stamped messages
log_msg <- function(message) {
  message(
    paste(Sys.time(), message, sep = ": ")
  )
}
