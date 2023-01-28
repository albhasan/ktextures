test_that("k_textures works", {

    truncate_layers <- function(layer_id, r) {
        if (layer_id == 4) {
            min(r[[4]] / 3.947, 2540) / 10
        } else {
            min(r[[layer_id]], 2540) / 10
        }
    }

    r <- terra::rast(system.file("extdata", "rapideye_2.tif",
                                 package = "ktextures"))
    r <- r[[-5]] # Remove transparency layer.
    r <- terra::rast(lapply(1:terra::nlyr(r), truncate_layers, r))
    k <- 4
    out_file <- tempfile(pattern = "k-texture_", fileext = ".tif")
    col_size <- row_size <- 128
    col_overlap <- row_overlap <- 4
    n_col <- terra::ncol(r)
    n_row <- terra::nrow(r)
    tmp_dir <- tempdir()

})
