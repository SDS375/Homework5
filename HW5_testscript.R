## HW5_testscript.R

library(testthat)

if(!file.exists("HW5_Class.R")) {
    stop("cannot find file 'HW5_Class.R'")
}

source("HW5_Class.R")

test_that("check validity method exists", {
    expect_false({
        validity_method <- getValidity(getClassDef("sparse_numeric"))
        is.null(validity_method)
    })
})

test_that("check validity method", {
    expect_true({
        x <- new("sparse_numeric",
                 value = c(1, 2, 3, 1),
                 pos = c(1L, 2L, 3L, 5L),
                 length = 5L)
        validObject(x)
    })
})

test_that("check validity method 2", {
    expect_error({
        x <- new("sparse_numeric",
                 value = c(1, 2, 3, 1),
                 pos = c(1L, 2L, 3L, 5L),
                 length = 5L)
        x@length <- 2L
        validObject(x)
    })
})

test_that("check coercion return class", {
    expect_s4_class({
        x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    }, "sparse_numeric")
})

test_that("check for show method", {
    expect_no_error({
        getMethod("show", "sparse_numeric")
    })
})

test_that("check for plot method", {
    expect_no_error({
        getMethod("plot", c("sparse_numeric", "sparse_numeric"))
    })
})

test_that("check for + method", {
    expect_no_error({
        getMethod("+", c("sparse_numeric", "sparse_numeric"))
    })
})

test_that("check for - method", {
    expect_no_error({
        getMethod("-", c("sparse_numeric", "sparse_numeric"))
    })
})

test_that("check for * method", {
    expect_no_error({
        getMethod("*", c("sparse_numeric", "sparse_numeric"))
    })
})

test_that("sparse add generic", expect_true(isGeneric("sparse_add")))
test_that("sparse mult generic", expect_true(isGeneric("sparse_mult")))
test_that("sparse sub generic", expect_true(isGeneric("sparse_sub")))
test_that("sparse crossprod generic", expect_true(isGeneric("sparse_crossprod")))

test_that("sparse add formals", {
    expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
    expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
    expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
    expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
    expect_s4_class({
        x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
        y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
        sparse_add(x, y)
    }, "sparse_numeric")
})

test_that("sparse_add", {
    result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
    expect_equal({
        x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
        y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
        sparse_add(x, y)
    }, result)
})

test_that("sparse add dense", {
    result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
    expect_equal({
        x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
        y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
        sparse_add(x, y)
    }, result)
})

test_that("all zero wrong length", {
    expect_error({
        x <- as(rep(0, 10), "sparse_numeric")
        y <- as(rep(0, 9), "sparse_numeric")
        sparse_add(x, y)
    })
})


test_that("numeric <-> sparse_numeric round trip", {
  v <- c(0, 3.5, 0, -2, 0, 0, 4)
  x <- as(v, "sparse_numeric")
  v_back <- as(x, "numeric")
  expect_equal(v_back, v)
})


test_that("sparse_mult basic overlap", {
  x <- as(c(0, 2, 0, 3, 0), "sparse_numeric")
  y <- as(c(1, 0, 0, 4, 0), "sparse_numeric")
  result <- sparse_mult(x, y)
  expect_s4_class(result, "sparse_numeric")
  expect_equal(as(result, "numeric"), c(0, 0, 0, 12, 0))
})

test_that("sparse_mult with disjoint supports gives all zeros", {
  x <- as(c(1, 0, 2, 0), "sparse_numeric")
  y <- as(c(0, 3, 0, 4), "sparse_numeric")
  result <- sparse_mult(x, y)
  expect_s4_class(result, "sparse_numeric")
  expect_equal(as(result, "numeric"), numeric(4))
})

test_that("operators + - * match sparse_* functions", {
  x <- as(c(0, 1, 0, 2), "sparse_numeric")
  y <- as(c(3, 0, 4, 0), "sparse_numeric")
  
  expect_equal(as(x + y, "numeric"), as(sparse_add(x, y), "numeric"))
  expect_equal(as(x - y, "numeric"), as(sparse_sub(x, y), "numeric"))
  expect_equal(as(x * y, "numeric"), as(sparse_mult(x, y), "numeric"))
})
