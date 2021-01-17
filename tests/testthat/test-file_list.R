context("handling associated files")

test_that("files can be handled in separated data frames", {
			expect_is(file_list(x = openaccess), "data.frame")
			
			Files <- file_list(x = openaccess)
			openaccess <- openaccess[ , colnames(openaccess) != "file"]
			expect_equal(file_list(x = openaccess), NULL)
			
			expect_is({file_list(openaccess) <- Files
					openaccess}, "lib_df")
		})
