#' @title Compute the splits of an image.
#'
#' @name compute_split
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Compute the pixel postions (columns and row) to split an image.
#'
#' @param n_col,n_row Total number of columns and rows in the image.
#' @param col_size,row_size Number of columns and rows in each split.
#' @param col_overlap, row_overlap Columns and rows to add to each side of a
#' split.
#'
#' @return A data.frame.
#'
compute_split <- function(n_col, n_row, col_size, row_size,
                          col_overlap = 0, row_overlap = 0) {

    col_seq <- seq(1 + col_overlap, n_col - col_overlap,
                   by = col_size)
    col_breaks <- data.frame(x_id = seq_along(col_seq),
                             col_from = col_seq,
                             col_to = c(col_seq[-1] - 1,
                                        n_col - col_overlap))
    col_breaks["ocol_from"] <- col_breaks[["col_from"]] - col_overlap
    col_breaks["ocol_to"]   <- col_breaks[["col_to"]]   + col_overlap
    col_breaks <- col_breaks[col_breaks$col_from != col_breaks$col_to, ]

    row_seq <- seq(1 + row_overlap, n_row - row_overlap,
                   by = row_size)
    row_breaks <- data.frame(y_id = seq_along(row_seq),
                             row_from = row_seq,
                             row_to = c(row_seq[-1] - 1,
                                        n_row - row_overlap))
    row_breaks["orow_from"] <- row_breaks[["row_from"]] - row_overlap
    row_breaks["orow_to"]   <- row_breaks[["row_to"]]   + row_overlap
    row_breaks <- row_breaks[row_breaks$row_from != row_breaks$row_to, ]

    breaks <- expand.grid(x_id = col_breaks[["x_id"]],
                          y_id = row_breaks[["y_id"]])
    breaks <- merge(breaks, row_breaks, by = "y_id")
    breaks <- merge(breaks, col_breaks, by = "x_id")

    return(breaks)

}



#' @title Get mirrors of a raster's borders.
#'
#' @name create_borders
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Create rasters by mirroring raster's borders. These rasters
#' could be later mosaicked with the given raster.
#'
#' @param r A terra's object.
#' @param col_overlap, row_overlap Number of columns and rows to add (repeat)
#' around the given raster.
#'
#' @return A list of terra's rasters.
#'
create_borders <- function(r, col_overlap, row_overlap) {

    left_border <- terra::rast(
        crs = terra::crs(r),
        resolution = terra::res(r),
        nrows = terra::nrow(r),
        ncols = col_overlap,
        nlyrs = terra::nlyr(r),
        xmin = terra::xmin(r) - (abs(terra::xres(r)) * col_overlap),
        xmax = terra::xmin(r),
        ymin = terra::ymin(r),
        ymax = terra::ymax(r),
        vals = terra::as.array(r)[, 1:col_overlap, ]
    )
    left_border <- terra::flip(left_border, direction = "horizontal")

    right_border <- terra::rast(
        crs = terra::crs(r),
        resolution = terra::res(r),
        nrows = terra::nrow(r),
        ncols = col_overlap,
        nlyrs = terra::nlyr(r),
        xmin = terra::xmax(r),
        xmax = terra::xmax(r) + (abs(terra::xres(r)) * col_overlap),
        ymin = terra::ymin(r),
        ymax = terra::ymax(r),
        vals = terra::as.array(r)[,
            (terra::ncol(r) - col_overlap + 1):terra::ncol(r),
        ]
    )
    right_border <- terra::flip(right_border, direction = "horizontal")

    bottom_border <- terra::rast(
        crs = terra::crs(r),
        resolution = terra::res(r),
        nrows = row_overlap,
        ncols = terra::ncol(r),
        nlyrs = terra::nlyr(r),
        xmin = terra::xmin(r),
        xmax = terra::xmax(r),
        ymin = terra::ymin(r) - (abs(terra::yres(r)) * row_overlap),
        ymax = terra::ymin(r),
        vals = terra::as.array(r)[
            (terra::nrow(r) - row_overlap + 1):terra::nrow(r), ,
        ]
    )
    bottom_border <- terra::flip(bottom_border, direction = "vertical")

    top_border <- terra::rast(
        crs = terra::crs(r),
        resolution = terra::res(r),
        nrows = row_overlap,
        ncols = terra::ncol(r),
        nlyrs = terra::nlyr(r),
        xmin = terra::xmin(r),
        xmax = terra::xmax(r),
        ymin = terra::ymax(r),
        ymax = terra::ymax(r) + (abs(terra::yres(r)) * row_overlap),
        vals = terra::as.array(r)[1:row_overlap, , ]
    )
    top_border <- terra::flip(top_border, direction = "vertical")

    bottom_left <- terra::rast(
        crs = terra::crs(r),
        resolution = terra::res(r),
        nrows = row_overlap,
        ncols = col_overlap,
        nlyrs = terra::nlyr(r),
        xmin = terra::xmin(r) - (abs(terra::xres(r)) * col_overlap),
        xmax = terra::xmin(r),
        ymin = terra::ymin(bottom_border),
        ymax = terra::ymax(bottom_border),
        vals = terra::as.array(bottom_border)[, 1:col_overlap, ]
    )
    bottom_left <- terra::flip(bottom_left, direction = "horizontal")

    bottom_right <- terra::rast(
        crs = terra::crs(r),
        resolution = terra::res(r),
        nrows = row_overlap,
        ncols = col_overlap,
        nlyrs = terra::nlyr(r),
        xmin = terra::xmax(r),
        xmax = terra::xmax(r) + (abs(terra::xres(r)) * col_overlap),
        ymin = terra::ymin(bottom_border),
        ymax = terra::ymax(bottom_border),
        vals = terra::as.array(bottom_border)[,
            (terra::ncol(bottom_border) - col_overlap + 1):
            terra::ncol(bottom_border),
        ]
    )
    bottom_right <- terra::flip(bottom_right, direction = "horizontal")

    top_left <- terra::rast(
        crs = terra::crs(r),
        resolution = terra::res(r),
        nrows = row_overlap,
        ncols = col_overlap,
        nlyrs = terra::nlyr(r),
        xmin = terra::xmin(r) - (abs(terra::xres(r)) * col_overlap),
        xmax = terra::xmin(r),
        ymin = terra::ymin(top_border),
        ymax = terra::ymax(top_border),
        vals = terra::as.array(top_border)[, 1:col_overlap, ]
    )
    top_left <- terra::flip(top_left, direction = "horizontal")

    top_right <- terra::rast(
        crs = terra::crs(r),
        resolution = terra::res(r),
        nrows = row_overlap,
        ncols = col_overlap,
        nlyrs = terra::nlyr(r),
        xmin = terra::xmax(r),
        xmax = terra::xmax(r) + (abs(terra::xres(r)) * col_overlap),
        ymin = terra::ymin(top_border),
        ymax = terra::ymax(top_border),
        vals = terra::as.array(top_border)[,
            (terra::ncol(top_border) - col_overlap + 1):
            terra::ncol(top_border),
        ]
    )
    top_right <- terra::flip(top_right, direction = "horizontal")

    return(list(left = left_border, right = right_border,
                bottom = bottom_border, top = top_border,
                bottom_left = bottom_left, bottom_right = bottom_right,
                top_left = top_left, top_right = top_right))

}
