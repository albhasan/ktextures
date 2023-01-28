#' @title Compute the loss function.
#'
#' @name feature_loss2
#'
#' @author Fabien Wagner, \email{wagner.h.fabien@gmail.com}
#'
#' @description Function for computing  the loss using layers VGG and custom
#' Variational Auto Encoder (VAE).
#'
#' @param y_true TODO
#' @param y_pred TODO
#' @param layer_vgg   Intermediate layer of the VGG model.
#' @param layer_vgg16 Intermediate layer of the VGG16 model.
#'
#' @return TODO
#'
feature_loss2 <- function(y_true, y_pred, layer_vgg,
                          layer_vgg16) {

    y_true_f <- keras::k_flatten(layer_vgg(y_true))
    y_pred_f <- keras::k_flatten(layer_vgg(y_pred))
    y_true_f2 <-
        keras::k_flatten(layer_vgg16(y_true[, , , 1:3]))
    y_pred_f2 <-
        keras::k_flatten(layer_vgg16(y_pred[, , , 1:3]))
    result <- keras::k_mean((y_true_f  - y_pred_f)^2) +
              keras::k_mean((y_true_f2 - y_pred_f2)^2)
    return(result)

}


#' @title Binary mask generator.
#'
#' @name hard_sigmoid_keras
#'
#' @author Fabien Wagner, \email{wagner.h.fabien@gmail.com}
#'
#' @description Hard signoid function for generating mask.
#'
#' @param x
#'
#' @return TODO
#'
hard_sigmoid_keras <- function(x) {
    keras::k_maximum(0, keras::k_minimum(1, (x - 0.5) * 5000 + 2500))
}


#' @title Get binary mask model.
#'
#' @name get_model
#'
#' @author Fabien Wagner, \email{wagner.h.fabien@gmail.com}
#'
#' @description Get the CNN model for generating binary masks with pre-trained
#' and frozen weight.
#'
#' @param input_shape TODO
#' @param num_classes TODO
#'
#' @return TODO
#
get_model <- function(input_shape = c(128, 128, 1)) {
                      #, num_classes = k) { TODO: Remove this argument.

    # TODO: Remove magrittr's pipe.
    inputs <- keras::layer_input(shape = input_shape)

    up_001 <- inputs %>%
        keras::layer_conv_2d(filters = 64,
                             kernel_size = c(1, 1),
                             padding = "same") %>%
        keras::layer_batch_normalization() %>%
        keras::layer_activation("elu") %>%
        keras::layer_dropout(0.05) %>%
        keras::layer_conv_2d(filters = 64,
                             kernel_size = c(1, 1),
                             padding = "same") %>%
        keras::layer_batch_normalization() %>%
        keras::layer_activation("elu") %>%
        keras::layer_dropout(0.05) %>%
        keras::layer_conv_2d(filters = 1,
                             kernel_size = c(1, 1),
                             activation = hard_sigmoid_keras,
                             dtype = "float32")

    classify2 <- keras::layer_concatenate(list(inputs, up_001),
                                          name = "input_val")

    classify <-  classify2 %>%
        keras::layer_lambda(function(x) {
                              x[, , , 2, drop = FALSE]
                            },
                            name = "output_val",
                            dtype = "float32")

    model <- keras::keras_model(
        inputs = inputs,
        outputs = classify
    )

    return(model)

}
