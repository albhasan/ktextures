
#' Segment an image using the k-textures algorithm
#'
#' Segment a satellite image using the k-textures algorithm. This is an
#' implementation of the algorithm descibe in 'K-textures, a self-supervised
#' hard clustering deep learning algorithm for satellite image segmentation"
#' by Fabien et al which was inspired the by the k-means algorithm. K-textures
#' takes an image and a number of classes, then it trains a Convolutional
#' Neural Network which is used to separate the pixels in the given image
#' according to their texture.
#'
#' @param file_path Paths to bands of a raster images corresponding to spectral
#' bands (for example, red, green, blue, and near infrared).
#' @param k Number of segments.
#' @param out_file Path to the file resulting from the segmentation.
#'
#' @return out_file.
#'
#' @export
#'
k_textures <- function(file_path, k, out_file) {

    stopifnot("File(s) not found!" = all(file.exists(file_path)))

    # Read and scale images.
    bands <- lapply(file_path, terra::rast)

    if (length(bands) > 1)
        stopifnot("Multiple multi-band images not supported!" =
                  all(sapply(bands, terra::nlyr) == 1))

    if (length(bands) > 1) {
        # TODO: Build a gdal vrt using the given files.
        stop("This option is not supported!")
    }

    img_r <- terra::rast(file_path[1])
    img_scaled_r <- terra::scale(img_r)

    breaks <- .split_r(img_r, col_size = 128, row_size = 128)

}


#' Compute the start and end pixels for spliting a raster.
#'
#' @param r        A terra's raster object.
#' @param col_size The numer of columns in each split along x.
#' @param row_size The numer of rows in each split along y.
#' @return         A data.frame.
#'
.split_r <- function(r, col_size, row_size) {

    col_seq <- seq(1, terra::ncol(r), by = col_size)
    col_breaks <- data.frame(x_id = 1:length(col_seq),
                             col_from = col_seq,
                             col_to = c(col_seq[-1] - 1, terra::ncol(r)))
    col_breaks <- col_breaks[col_breaks$col_from != col_breaks$col_to,]

    row_seq <- seq(1, terra::nrow(r), by = row_size)
    row_breaks <- data.frame(y_id = 1:length(row_seq),
                           row_from = row_seq,
                           row_to = c(row_seq[-1] - 1, terra::nrow(r)))
    row_breaks <- row_breaks[row_breaks$row_from != row_breaks$row_to,]

    breaks <- expand.grid(x_id = col_breaks[["x_id"]],
                          y_id = row_breaks[["y_id"]])
    breaks <- merge(breaks, row_breaks, by = "y_id")
    breaks <- merge(breaks, col_breaks, by = "x_id")
    breaks["x_from"] <- terra::xFromCol(r, breaks[["col_from"]])
    breaks["x_to"]   <- terra::xFromCol(r, breaks[["col_to"]])
    breaks["y_from"] <- terra::yFromRow(r, breaks[["row_from"]])
    breaks["y_to"]   <- terra::yFromRow(r, breaks[["row_to"]])

    return(breaks)
}


