test_that("get_fia_tables() works", {
  path <- withr::local_tempdir()
  get_fia_tables(states = "RI", download_dir = path, keep_zip = TRUE)
  files <- fs::dir_ls(path)
  expect_true(any(stringr::str_detect(files, "RI_CSV.zip")))
  expect_true(any(stringr::str_detect(files, "RI_PLOTGEOM.csv")))

  db <- read_fia(states = "RI", dir = path)

  expect_type(db, "list")
  expect_s3_class(db[[1]], "data.frame")
})
