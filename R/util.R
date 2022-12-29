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



#' @title Add pixels around the given raster.
#'
#' @name add_borders
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Add pixels around the given raster by repeating the left,
#' right, bottom, and top pixels.
#'
#' @param r A terra's object.
#' @param col_overlap, row_overlap Columns and rows to add (repeat) around the
#' given raster.
#'
#' @return A terra's raster.
#'
add_borders <- function(r, col_overlap, row_overlap) {

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
        vals = terra::as.array(r)[, 1:col_overlap, ]
    )

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
        vals = terra::as.array(r)[, 1:col_overlap, ]
    )

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
        vals = terra::as.array(r)[, 1:col_overlap, ]
    )

    return(terra::mosaic(r, left_border, right_border,
                         bottom_border, top_border))

}
