tmp_dir <- tempdir()

create_eurostat_database (
  ids = c("tin00092"),
  db_path = file.path(tmp_dir, "example1.db")
 )

"example1.db" %in% dir ( tmp_dir )

test_that("Database creation works", {
  expect_true ( file.exists(file.path(tmp_dir, "example1.db")) )
})
