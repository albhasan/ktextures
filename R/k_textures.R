
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
#' @param r        A terra package object. A raster image.
#' @param k        Number of segments.
#' @param out_file Path to the file resulting from the segmentation.
#'
#' @return         A terra's raster.
#'
#' @export
#'
k_textures <- function(r, k, out_file, col_size = 128, row_size = 128,
                       col_overlap = 4, row_overlap = 4) {

    # Repeat pixels aroung the raster.
    r_borders <- add_borders(r, col_overlap = col_overlap,
                             row_overlap = row_overlap)

    # TODO: Compare r to r_borders using QGIS.
    breaks <- compute_split(n_col = terra::ncol(r_borders),
                            n_row = terra::nrow(r_borders),
                            col_size = col_size,
                            row_size = row_size,
                            col_overlap = col_overlap,
                            row_overlap = row_overlap)

}
