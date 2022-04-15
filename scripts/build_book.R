# make sure bookdown is installed
# install.packages("bookdown")
# bookdown should install xfun but in case it does not
# install.packages("xfun")

# xfun::in_dir() changes the working directory to ensure bookdown can locate
# index.Rmd to render the book from vignettes folder
xfun::in_dir(dir = "inst/book/", expr = bookdown::render_book(
  input = "index.Rmd",
  output_format = "bookdown::gitbook"))

# delete the docs and _bookdown_files from vignettes directory
unlink(x = "inst/book/_bookdown_files", recursive = TRUE)
