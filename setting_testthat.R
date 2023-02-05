devtools::dev_sitrep()  ## for package development situation report
devtools::check() ## to check the package
devtools::document() # creating up to data document


install.packages("usethis")
usethis::use_testthat() # to setup test structure
usethis::use_test() ## will open the test file for specific function (the function must be open). Will also create new file if missing in the test folder
