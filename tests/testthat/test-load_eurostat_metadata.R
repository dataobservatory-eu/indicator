library(testthat)

load_eurostat_metadata()

test_that("multiplication works", {
   expect_true( exists("indic_dict", envir = globalenv()))
   expect_true( exists("eurostat_toc", envir = globalenv()))
   expect_true( exists("var_labels", envir = globalenv()))
   expect_true( exists("value_labels", envir = globalenv()))
})
