## Internal variable names
##
## Every variable name is replaced internally by a code, so that any name (with
## spaces, accents, punctuation, ...) is a valid name in data frames, formulas
## and lavaan syntax. A code is
##
##     B64_SYMBOL + jmvcore::toB64(name) + B64_END_SYMBOL
##     "score~"  ->  "..b_b_b..Xc2NvcmV.__e_e_e__"
##
## Factor contrasts and interactions are built from codes with FACTOR_SYMBOL,
## LEVEL_SYMBOL and INTERACTION_SYMBOL, e.g. code(prime) + FACTOR_SYMBOL + "1".
## INTERACTION_SYMBOL must not contain ":", which lavaan cannot take in every model.
##
## fromb64() turns every code found in any text back into the name. A code can
## contain any character allowed in a name, including ".", ".." and "_", so it is
## delimited by an end marker that can never occur inside it: B64_END_SYMBOL
## contains "__". In jmvcore's codes "_" is the Base64 value 63 (binary 111111);
## "__" would need a byte 0xFC-0xFF, which never occurs in UTF-8 text.

FACTOR_SYMBOL <- "..f_f_f.."
LEVEL_SYMBOL <- "..l_l_l.."
INTERACTION_SYMBOL <- "..i_i_i.."
B64_SYMBOL <- "..b_b_b.."
B64_REGEX <- "\\.\\.b_b_b\\.\\."
B64_END_SYMBOL <- "__e_e_e__"
B64_END_REGEX <- "__e_e_e__"


########### names ###########

## `ref` (in tob64() and fromb64()) is a legacy argument: older callers passed
## the list of variable names, which the previous, marker-less decoder needed to
## find codes in a text. Codes are now delimited by markers, so it is accepted
## for compatibility and ignored.
##
## tob64() and fromb64() choose their method with a plain if/else rather than
## UseMethod(".tob64"). A generic whose method prefix (".tob64") differs from its
## own name ("tob64") only dispatches when called by name: it fails with "no
## applicable method" when called from the console or passed as a function, as
## in lapply(x, tob64). See AI/dotted-s3-dispatch-pitfall.md (Fix A). These work
## the same in every context.

tob64 <- function(x, ...) {
  if (is.list(x)) .tob64.list(x, ...) else .tob64.default(x, ...)
}

.tob64.default <- function(obj, ref = NULL) {
  # NULL and empty vectors stay empty: paste0(B64_SYMBOL, character(0)) would
  # return the bare marker.
  if (length(obj) == 0) {
    return(obj)
  }
  if (is.numeric(obj)) {
    return(obj)
  }
  encoded <- paste0(B64_SYMBOL, jmvcore::toB64(obj), B64_END_SYMBOL)
  encoded[is.na(obj)] <- NA
  encoded
}

.tob64.list <- function(obj, ref = NULL) {
  lapply(obj, tob64)
}

fromb64 <- function(x, ...) {
  if (inherits(x, "formula")) {
    .fromb64.formula(x, ...)
  } else if (is.list(x)) {
    .fromb64.list(x, ...)
  } else {
    .fromb64.default(x, ...)
  }
}

.fromb64.default <- function(obj, ref = NULL) {
  if (!is.something(obj)) {
    return(obj)
  }

  if (length(obj) > 1) {
    return(unlist(sapply(obj, fromb64, USE.NAMES = F)))
  }
  if (!is.character(obj)) {
    return(obj)
  }

  # Split the text into codes and the text between them. Codes are decoded;
  # the symbols are turned into their display form only between codes, so a
  # decoded name is never altered.
  spans <- gregexpr(paste0(B64_REGEX, "(.*?)", B64_END_REGEX), obj, perl = TRUE)[[1]]
  if (spans[[1]] == -1) {
    return(.b64_unsymbol(obj))
  }
  starts <- as.integer(spans)
  ends <- starts + attr(spans, "match.length") - 1
  pieces <- character(0)
  position <- 1
  for (i in seq_along(starts)) {
    pieces <- c(pieces, .b64_unsymbol(substr(obj, position, starts[i] - 1)))
    span <- substr(obj, starts[i], ends[i])
    code <- substr(span, nchar(B64_SYMBOL) + 1, nchar(span) - nchar(B64_END_SYMBOL))
    pieces <- c(pieces, jmvcore::fromB64(code))
    position <- ends[i] + 1
  }
  pieces <- c(pieces, .b64_unsymbol(substr(obj, position, nchar(obj))))
  paste(pieces, collapse = "")
}

# Display form of the text between codes: factor and level symbols disappear
# ("prime" + FACTOR_SYMBOL + "1" -> "prime1"), interactions become ":".
.b64_unsymbol <- function(text) {
  text <- gsub(B64_SYMBOL, "", text, fixed = TRUE)
  text <- gsub(B64_END_SYMBOL, "", text, fixed = TRUE)
  text <- gsub(FACTOR_SYMBOL, "", text, fixed = TRUE)
  text <- gsub(LEVEL_SYMBOL, "", text, fixed = TRUE)
  gsub(INTERACTION_SYMBOL, ":", text, fixed = TRUE)
}

.fromb64.formula <- function(obj, ref = NULL) {
  return(stats::formula(fromb64(deparse(obj))))
}


.fromb64.list <- function(obj, ref = NULL) lapply(obj, fromb64)


is.b64 <- function(a) {
  test1 <- is.there(B64_SYMBOL, a)
  test2 <- is.there(LEVEL_SYMBOL, a)
  test3 <- is.there(FACTOR_SYMBOL, a)
  test4 <- is.there(INTERACTION_SYMBOL, a)
  test5 <- is.there(B64_END_SYMBOL, a)
  any(test1, test2, test3, test4, test5)
}
