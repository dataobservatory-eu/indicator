
x <- data.frame ( 
  geo = rep(c("NL", "BE", "LU"), 4), 
  time = rep(c(2016:2019),3), 
  value = runif(12, 1,100), 
  estimate = rep("actual", 12)
) 
shortcode = "observatory_test_1"
description = "A test indicator with random numbers" 
date_indicator = as.Date ( "2020-08-24")
date_earliest  = min (x$time, na.rm=TRUE)
date_latest  =  max(x$time, na.rm=TRUE)
keyword1 = "music"
keyword2 = "random"  
keyword3 = "Benelux" 
keyword4 = "observatory"


test_indicator <- indicator (
  x <- data.frame ( 
    geo = rep(c("NL", "BE", "LU"), 4), 
    time = rep(c(2016:2019),3), 
    value = runif(12, 1,100), 
    estimate = rep("actual", 12)
  ), 
  shortcode = "observatory_test_1", 
  description = "A test indicator with random numbers", 
  date_indicator = as.Date ( "2020-08-24"),
  date_earliest  = min (x$time, na.rm=TRUE),
  date_latest  =  max(x$time, na.rm=TRUE),
  keyword1 = "music",  keyword2 = "economy",  keyword3 = "demand", keyword4 = "pcr"
)

print(test_indicator)
attributes ( test_indicator)

test_that("indicator created", {
  expect_true( "indicator" %in% class ( test_indicator) )
  expect_true( attr(test_indicator, "keyword2") == "economy" )
})


test_that("exceptions are handled", {
  expect_error(
    #worng top-level keyword
    indicator (
    x <- data.frame ( 
      geo = rep(c("NL", "BE", "LU"), 4), 
      time = rep(c(2016:2019),3), 
      value = runif(12, 1,100)
    ), 
    shortcode = "observatory_test_1", 
    description = "A test indicator with random numbers", 
    date_created = as.Date ( "2020-08-24"),
    date_earliest  = min (x$time, na.rm=TRUE),
    date_latest  =  max(x$time, na.rm=TRUE),
    keyword1 = "bad_one",  keyword2 = "random",  keyword3 = "Benelux", keyword4 = "observatory"
  ))
  expect_error(
    # wrong input type
    indicator (
    x <-1:3, 
    shortcode = "observatory_test_1", 
    description = "A test indicator with random numbers", 
    date_created = as.Date ( "2020-08-24"),
    date_earliest  = min (x$time, na.rm=TRUE),
    date_latest  =  max(x$time, na.rm=TRUE),
    keyword1 = "bad_one",  keyword2 = "random",  keyword3 = "Benelux", keyword4 = "observatory"
  ))
  expect_error(
    #description is missing
    indicator (
      x <- data.frame ( 
        geo = rep(c("NL", "BE", "LU"), 4), 
        time = rep(c(2016:2019),3), 
        value = runif(12, 1,100)
      ), 
      shortcode = "observatory_test_1", 
      description = NULL, 
      date_created = as.Date ( "2020-08-24"),
      date_earliest  = min (x$time, na.rm=TRUE),
      date_latest  =  max(x$time, na.rm=TRUE),
      keyword1 = "music",  keyword2 = "random",  keyword3 = "Benelux", keyword4 = "observatory"
    ))
  expect_error(
    #worng wrong value type
    indicator (
      x <- data.frame ( 
        geo = rep(c("NL", "BE", "LU"), 4), 
        time = rep(c(2016:2019),3), 
        value = rep("hello", 12)
      ), 
      shortcode = "observatory_test_1", 
      description = NULL, 
      date_created = as.Date ( "2020-08-24"),
      date_earliest  = min (x$time, na.rm=TRUE),
      date_latest  =  max(x$time, na.rm=TRUE),
      keyword1 = "bad_one",  keyword2 = "random",  keyword3 = "Benelux", keyword4 = "observatory"
    ))
  expect_error(
    #worng wrong time type
    indicator (
      x <- data.frame ( 
        geo = rep(c("NL", "BE", "LU"), 4), 
        time = rep("hello", 12), 
        value = runif(12, 1,100)
      ), 
      shortcode = "observatory_test_1", 
      description = NULL, 
      date_created = as.Date ( "2020-08-24"),
      date_earliest  = min (x$time, na.rm=TRUE),
      date_latest  =  max(x$time, na.rm=TRUE),
      keyword1 = "bad_one",  keyword2 = "random",  keyword3 = "Benelux", keyword4 = "observatory"
    ))
})
