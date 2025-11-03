## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)


# ---- Validity method ----
setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos))
    return("Lengths of 'value' and 'pos' must match")
  if (any(object@pos < 1 | object@pos > object@length))
    return("'pos' values must be within 1 and 'length'")
  if (anyDuplicated(object@pos))
    return("'pos' cannot contain duplicates")
  TRUE
})

# ---- Coercion methods ----
setAs("numeric", "sparse_numeric", function(from) {
  pos <- which(from != 0)
  new("sparse_numeric",
      value = from[pos],
      pos = as.integer(pos),
      length = as.integer(length(from)))
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  out[from@pos] <- from@value
  out
})

# ---- Utility ----
.check_lengths <- function(x, y) {
  if (x@length != y@length)
    stop("Vectors must have the same length")
}

# ---- Generic declarations ----
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

# ---- Arithmetic methods ----
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_lengths(x, y)
            
            # deterministic union of positions, sorted ascending
            all_pos <- sort(unique(c(x@pos, y@pos)))
            
            # align values by position using match (no names carried)
            ix <- match(all_pos, x@pos)
            iy <- match(all_pos, y@pos)
            
            vx <- ifelse(is.na(ix), 0, x@value[ix])
            vy <- ifelse(is.na(iy), 0, y@value[iy])
            
            summed <- vx + vy
            keep <- summed != 0L
            
            new("sparse_numeric",
                value = unname(summed[keep]),
                pos   = as.integer(all_pos[keep]),
                length = x@length)
          })


setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_lengths(x, y)
            y_neg <- new("sparse_numeric", value = -y@value, pos = y@pos, length = y@length)
            sparse_add(x, y_neg)
          })

setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_lengths(x, y)
            overlap <- intersect(x@pos, y@pos)
            if (length(overlap) == 0)
              return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
            val_x <- x@value[match(overlap, x@pos)]
            val_y <- y@value[match(overlap, y@pos)]
            new("sparse_numeric", value = val_x * val_y, pos = overlap, length = x@length)
          })

setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_lengths(x, y)
            overlap <- intersect(x@pos, y@pos)
            if (length(overlap) == 0) return(0)
            val_x <- x@value[match(overlap, x@pos)]
            val_y <- y@value[match(overlap, y@pos)]
            sum(val_x * val_y)
          })

# ---- Operator aliases ----
setMethod("+", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

setMethod("-", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

setMethod("*", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

# ---- Show method ----
setMethod("show", "sparse_numeric", function(object) {
  cat("Sparse numeric vector of length", object@length, "\n")
  cat("Non-zero positions:", object@pos, "\n")
  cat("Values:", object@value, "\n")
})

# ---- Plot method ----
setMethod("plot", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            plot(x@pos, x@value, col = "blue", pch = 16,
                 xlab = "Position", ylab = "Value",
                 main = "Non-zero Elements of Sparse Vectors", ...)
            points(y@pos, y@value, col = "red", pch = 17)
            legend("topright", legend = c("x", "y"),
                   col = c("blue", "red"), pch = c(16, 17))
          })

# ---- Extra method: Euclidean norm ----
setGeneric("sparse_norm", function(x) standardGeneric("sparse_norm"))
setMethod("sparse_norm", "sparse_numeric",
          function(x) sqrt(sum(x@value^2)))