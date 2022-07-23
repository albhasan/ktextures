
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
#' @param red,green,blue,nir Paths to raster images corresponding to the red,
#' green, blue, and near infrared bands of the spectrum. When red is a file
#' with more than 3 bands, this function extracts the first 4 bands.
#' @param k Number of segments.
#' @param out_file Path to the file resulting from the segmentation.
#'
#' @return out_file.
#'
#' @export
#'
k_textures <- function(red, green = NULL, blue = NULL, nir = NULL,
                       k, out_file) {

    stopifnot("Red file not found" = file.exists(red))

    # Get bands' paths.
    if (is.null(green) && is.null(blue) && is.null(nir)) {
        bands <- .image_to_rgba(red, out_dir = tempdir())
        red <-   bands["red"]
        green <- bands["green"]
        blue <-  bands["blue"]
        nir <-   bands["nir"]
    }

    stopifnot("Green file not found" = file.exists(green))
    stopifnot("Blue file not found" = file.exists(blue))
    stopifnot("NIR file not found" = file.exists(nir))

    red   <- terra::rast(red)
    green <- terra::rast(green)
    blue  <- terra::rast(blue)
    nir   <- terra::rast(nir)

    # Scale bands' values and export them to INT1U.
    # NOTE: This values are only valid for Planet's rapid eye images.
    bands_max   <- c("red" = 2540, "green" = 2540, "blue" = 2540, "nir" = 2540)
    bands_scale <- c("red" = 10,   "green" = 10,   "blue" = 10,   "nir" = 10)
    bands_local_scale <- c("red" = 1, "green" = 1, "blue" = 1, "nir" = 3.937)

    red   <- min(red, (bands_max["red"]/bands_local_scale["red"]))/bands_scale["red"]
    green <- min(green, (bands_max["green"]/bands_local_scale["green"]))/bands_scale["green"]
    blue  <- min(blue, (bands_max["blue"]/bands_local_scale["blue"]))/bands_scale["blue"]
    nir   <- min(nir, (bands_max["nir"]/bands_local_scale["nir"]))/bands_scale["nir"]

    # Re-construct the images with 4-bands.
    gdalUtilities::gdalbuildvrt(separate = TRUE)



}

#' Export the bands of an image
#'
#' Exprot the first 4 bands of an image-file to independent files. This
#' funciton assumes they correspond to red, green, blue, and near infra-red.
#'
#' @param in_file Path to a file.
#' @param out_dir Path to a directory for storing the results.
#'
#' @return A vector with the paths to the resulting files.
#'
.image_to_rgba <- function(in_file, out_dir) {
    f_name <- tools::file_path_sans_ext(basename(in_file))
    f_red <-  file.path(out_dir, paste0(f_name, "_red.tif"))
    f_green<- file.path(out_dir, paste0(f_name, "_green.tif"))
    f_blue<-  file.path(out_dir, paste0(f_name, "_blue.tif"))
    f_nir<-   file.path(out_dir, paste0(f_name, "_nir.tif"))
    red <- gdalUtilities::gdal_translate(
        src_dataset = in_file,
        dst_dataset = f_red,
        b = 1,
        colorinterp = "red"
    )
    green <- gdalUtilities::gdal_translate(
        src_dataset = in_file,
        dst_dataset = f_green,
        b = 2,
        colorinterp = "green"
    )
    blue <- gdalUtilities::gdal_translate(
        src_dataset = in_file,
        dst_dataset = f_blue,
        b = 3,
        colorinterp = "blue"
    )
    nir <- gdalUtilities::gdal_translate(
        src_dataset = in_file,
        dst_dataset = f_nir,
        b = 4,
        colorinterp = "alpha"
    )
    return(c("red" = f_red, "green" = f_green, "blue" = f_blue, "nir" = f_nir))
}


