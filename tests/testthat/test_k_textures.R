test_that("image_to_rgba works", {

    img_path <- system.file("extdata", "rapideye.tif", package = "ktextures")

    # Normal usage.
    res <- .image_to_rgba(in_file = img_path, out_dir = tempdir())
    expect_equal(length(res), 4)
    expect_equal(names(res), c("red", "green", "blue", "nir"))
    expect_true(all(file.exists(res)))

    # Throw an error when given file with less then 4 bands.
    expect_error(.image_to_rbga(in_file = red))
})


