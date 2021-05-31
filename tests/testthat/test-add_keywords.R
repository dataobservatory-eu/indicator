
data("small_population_description")

keywords_added <- add_keywords( small_population_description,
                                keywords = list("music", "demand", "demography", "test"))

test_that("correct form is returned", {
  expect_equal(names(keywords_added), names(small_population_description))
})
