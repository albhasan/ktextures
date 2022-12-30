
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

    base_dir <- tempfile(pattern = "k-textures-")
    block_dir  <- file.path(base_dir, "data_block")
    oblock_dir <- file.path(base_dir, "data_block_overlap")
    if (!dir.exists(base_dir)) {
        dir.create(block_dir, recursive = TRUE)
        dir.create(oblock_dir)
    }

    # Repeat pixels around the raster.
    r_borders <- add_borders(r, col_overlap = col_overlap,
                            row_overlap = row_overlap)

    # Get a data.frame with the start/end position of each image block.
    breaks <- compute_split(n_col = terra::ncol(r_borders),
                            n_row = terra::nrow(r_borders),
                            col_size = col_size,
                            row_size = row_size,
                            col_overlap = col_overlap,
                            row_overlap = row_overlap)

    # Save the images to disk.
    # TODO: Why do they save the image's blocks as both tif and png?
    by(breaks, seq_len(nrow(breaks)), FUN = function(b) {
        block_r <- terra::rast(terra::as.array(r_borders)[
            b[["row_from"]]:b[["row_to"]],
            b[["col_from"]]:b[["col_to"]],
        ])
        terra::writeRaster(block_r,
            filename = file.path(block_dir, paste0("block_r_",
                                                   b[["row_from"]], "_",
                                                   b[["col_from"]], ".tif")))

        oblock_r <- terra::rast(terra::as.array(r_borders)[
            b[["orow_from"]]:b[["orow_to"]],
            b[["ocol_from"]]:b[["ocol_to"]],
        ])
        terra::writeRaster(oblock_r,
            filename = file.path(oblock_dir, paste0("oblock_r_",
                                                    b[["row_from"]], "_",
                                                    b[["col_from"]], ".tif")))
    })

    # TODO: train the model?

}
