test_that("plot_ctt (no save) runs silent without error", {
  #skip_if(Sys.getenv("CI") == "", message = "Run only on CI") {
    expect_silent(ctt <- plot_ctt(
      data_folder_path = file.path("testdata"),
      output_file_path = NULL))
  #}
})
