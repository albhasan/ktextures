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
#' @param tmp_dir  Path to a directory for storing temporal files.
#'
#' @return         A terra's raster.
#'
#' @export
#'
k_textures <- function(r, k, out_file, tmp_dir = tempdir()) {


    #---- Setup ----

    # NOTE: The pre-trained models assumed the following:
    col_size <- 128
    row_size <- 128
    col_overlap <- 4
    row_overlap <- 4

    base_dir <- tempfile(pattern = "k-textures-", tmpdir = tmp_dir)
    block_dir  <- file.path(base_dir, "data_block")
    oblock_dir <- file.path(base_dir, "data_block_overlap")
    if (!dir.exists(base_dir)) {
        dir.create(block_dir, recursive = TRUE)
        dir.create(oblock_dir)
    }


    #---- Image pre-processing ----

    # Repeat pixels around the raster.
    rs <- create_borders(r, col_overlap = col_overlap,
                         row_overlap = row_overlap)
    r_borders <- terra::mosaic(r, rs[["left"]], rs[["right"]], rs[["bottom"]],
                               rs[["top"]], rs[["bottom_left"]],
                               rs[["bottom_right"]], rs[["top_left"]],
                               rs[["top_right"]],
                               filename = file.path(base_dir,
                                                    "image_borders.tif"))
    rm(rs)

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



    #---- Train model ----

    # Extract VGG16's last layer and freeze its weights.
    layer_name <- "block5_conv3"
    model_vgg <- keras::application_vgg16(weights = "imagenet",
                                          include_top = FALSE)
    intermediate_layer_model_vgg16 <- keras::keras_model(
        inputs = model_vgg$input,
        outputs = keras::get_layer(model_vgg, layer_name)$output
    )
    keras::freeze_weights(intermediate_layer_model_vgg16)
    rm(model_vgg)

    # Extract the pre-trained VAE layer (trained on Planet NICFI images,
    # Mato Grosso, Brazil).
    vae_planet_128 <- keras::load_model_hdf5(
        system.file("extdata",
                    "vae_planet_MODEL_3898_0.0001061_0.9703441_MODEL.h5",
                    package = "ktextures"),
        custom_objects = NULL,
        compile = TRUE
    )
    intermediate_layer_model_vgg <- keras::keras_model(
        inputs = vae_planet_128$input,
        outputs = keras::get_layer(vae_planet_128, layer_name)$output
    )
    keras::freeze_weights(intermediate_layer_model_vgg)
    rm(vae_planet_128)

    model_layer_01 <- get_model()

# load pretrained weights for the binary mask generator
name_weights_file=paste0("./weights_hard_sigmoid/layer_hard_sigmoid_",k,"_classes_accuracy_0001.h5")
load_model_weights_hdf5(model_layer_01, name_weights_file) 

}
