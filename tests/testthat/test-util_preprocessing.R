test_that("compute_split works", {

    n_col <- 19
    n_row <- 31
    col_size <- 7
    row_size <- 5
    col_overlap <- 1
    row_overlap <- 3

    split <- compute_split(n_col = n_col, n_row = n_row, col_size = col_size,
                           row_size = row_size, col_overlap = col_overlap,
                           row_overlap = row_overlap)

    expect_equal(split[["ocol_from"]][1], 1)
    expect_equal(split[["orow_from"]][1], 1)
    expect_equal(split[["ocol_to"]][nrow(split)], n_col)
    expect_equal(split[["orow_to"]][nrow(split)], n_row)

    id_first_x <- split$x_id %in% head(unique(split$x_id), -1)
    expect_true(all(split[id_first_x, "col_to"] -
                    split[id_first_x, "col_from"] == col_size - 1))

    id_first_y <- split$y_id %in% head(unique(split$y_id), - 1)
    expect_true(all(split[id_first_y, "row_to"] -
                    split[id_first_y, "row_from"] == row_size - 1))

})


test_that("compute_split works with no overlap", {

    split <- compute_split(n_col = 19, n_row = 31, col_size = 11,
                           row_size = 7, col_overlap = 0,
                           row_overlap = 0)

    expect_equal(split[["ocol_from"]][1], 1)
    expect_equal(split[["orow_from"]][1], 1)
    expect_equal(split[["ocol_to"]][nrow(split)], 19)
    expect_equal(split[["orow_to"]][nrow(split)], 31)

    expect_equal(split[["col_from"]], split[["ocol_from"]])
    expect_equal(split[["row_from"]], split[["orow_from"]])
    expect_equal(split[["col_to"]], split[["ocol_to"]])
    expect_equal(split[["row_to"]], split[["orow_to"]])

})


test_that("add_borders works", {

    r <- terra::rast(system.file("extdata", "rapideye_2.tif",
                                 package = "ktextures"))
    col_overlap <- 3
    row_overlap <- 5
    rs <- create_borders(r, col_overlap = col_overlap,
                         row_overlap = row_overlap)
    larger_r <- terra::mosaic(r, rs[["left"]], rs[["right"]], rs[["bottom"]],
                              rs[["top"]], rs[["bottom_left"]],
                              rs[["bottom_right"]],
                              rs[["top_left"]], rs[["top_right"]])

    # Test raster's properties.
    expect_equal(crs(larger_r), crs(r))
    expect_equal(nrow(larger_r), nrow(r) + (2 * row_overlap))
    expect_equal(ncol(larger_r), ncol(r) + (2 * col_overlap))
    expect_equal(prod(dim(larger_r)),
                 prod(dim(r)) +
                 ((2 * col_overlap * nrow(r)) +
                 (2 * row_overlap * ncol(r)) +
                 (4 * col_overlap * row_overlap)) * nlyr(r))

    expect_true(dplyr::near(xmin(larger_r), xmin(r) - (xres(r) * col_overlap)))
    expect_true(dplyr::near(xmax(larger_r), xmax(r) + (xres(r) * col_overlap)))
    expect_true(dplyr::near(ymin(larger_r), ymin(r) - (yres(r) * row_overlap)))
    expect_true(dplyr::near(ymax(larger_r), ymax(r) + (yres(r) * row_overlap)))

    # Test raster's values.
    expect_true(all(r[1, 1, ] == larger_r[row_overlap, col_overlap, ]))
    expect_true(all(r[1, 1, ] == larger_r[row_overlap, col_overlap + 1, ]))
    expect_true(all(r[1, 1, ] == larger_r[row_overlap + 1, col_overlap, ]))
    expect_true(all(r[1, 1, ] == larger_r[row_overlap + 1, col_overlap + 1, ]))
    expect_true(all(r[nrow(r), ncol(r), ] ==
                    larger_r[nrow(larger_r) - row_overlap,
                             ncol(larger_r) - col_overlap, ]))
    expect_true(all(r[nrow(r), ncol(r), ] ==
                    larger_r[nrow(larger_r) - row_overlap,
                             ncol(larger_r) - col_overlap + 1, ]))
    expect_true(all(r[nrow(r), ncol(r), ] ==
                    larger_r[nrow(larger_r) - row_overlap + 1,
                             ncol(larger_r) - col_overlap, ]))
    expect_true(all(r[nrow(r), ncol(r), ] ==
                    larger_r[nrow(larger_r) - row_overlap + 1,
                             ncol(larger_r) - col_overlap + 1, ]))
    expect_true(all(r[1, ncol(r), ] ==
                    larger_r[row_overlap, ncol(larger_r) - col_overlap, ]))
    expect_true(all(r[1, ncol(r), ] ==
                    larger_r[row_overlap + 1,
                             ncol(larger_r) - col_overlap + 1, ]))
    expect_true(all(r[nrow(r), 1, ] ==
                    larger_r[nrow(larger_r) - row_overlap, col_overlap, ]))
    expect_true(all(r[nrow(r), 1, ] ==
                    larger_r[nrow(larger_r) - row_overlap + 1,
                             col_overlap + 1, ]))

})
