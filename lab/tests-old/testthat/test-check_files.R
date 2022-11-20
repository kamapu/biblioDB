context("Check on existence of associated files")

test_that("check_files is working", {
  result <- evaluate_promise(
    check_files(
      x = openaccess,
      path = path.package("biblioDB"), pattern = "pdf"
    ),
    print = TRUE
  )
  expect_equal(grepl("## Everything OK!", result$output), TRUE)
})
