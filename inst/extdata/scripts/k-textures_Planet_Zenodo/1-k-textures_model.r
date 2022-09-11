# Script: k-textures model ------------------
# Authors: Fabien H. Wagner (wagner.h.fabien@gmail.com) and Ricardo Dal'Agnol da Silva (ricds@hotmail.com)
# Date Created: 2022-05-16

# This is the code of the k-textures algorithm which provides self supervised segmentation
# of a Planet NICFI 4-band image (RGB-NIR) for a k number of classes.
# It is an example of its application with the Planet image used in the paper.

# The model detects k hard clustering classes represented in the model as k discrete binary
# masks and their associated k independently generated textures, that combined are a simulation
# of the original image. 

# The similarity loss is the mean squared error between the features of the original and
# the simulated image, both extracted from the penultimate convolutional block of Keras 
# 'imagenet' pretrained VGG-16 model and a custom feature extractor made with Planet data.

# When you run the code, the entire image of Planet is given to the model in tiles of 136x136 pixels (input) and 
# 128x128 pixels (output) to find the k classes. The model will train and generate weight files in the 'weights' directory.
# This process takes a while (~20 mins or more), and you can track the progress in the 'weights' directory. 

# After the model goes through all epochs (iterations), the prediction from the model return tiles of 128x128 pixels that are merged to obtain
# the final raster of clusters and the raster of the simulated image textures, saved in the directory "./MASK/"

# One example image is already pre-processed and ready to test the model.
# To prepare your own data to use in the model, use the code 2-Preparation_Planet_image.r first

# load libraries
library(keras)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)
library(tensorflow)
library(doParallel)
library(raster) 

# set working directory
# change to the path of the unzipped directory
setwd("K:\\k-textures_Planet_Zenodo\\")

# gdal function that are used to mosaic images
# you need to have OSGEO4W installed in your computer with GDAL - https://www.osgeo.org/projects/osgeo4w/
# https://qgis.org/en/site/forusers/download.html -> OSGeo4W Network Installer
gdal_translate = paste("C:/OSGeo4W/OSGeo4W.bat","C:/OSGeo4W/bin/gdal_translate.exe")
gdalbuildvrt = paste("C:/OSGeo4W/OSGeo4W.bat","C:/OSGeo4W/bin/gdalbuildvrt.exe")

# k is the number of classes of the k-textures model 
# can take values between 2 and 128
# here k=6 classes for our example
k = 6L 

# to predict the simulated image and the raster of the k-classes set prediction to TRUE
# otherwise the model will only train
PREDICTION = TRUE



# dont need to change the code below this point ---------------------------
# CNN used in the binary mask generator -----------------------------------

# directory creation to store temporary images, weights, time series of
# accuracy and final results (image of the clusters and simulated images)
dir.create("./tmp/")
dir.create("./weights/")
dir.create("./weights_save/")
dir.create("./epoch_history/")
dir.create("./MASK/")

# clean the weights directory
unlink(list.files("./weights/",full.names = TRUE),recursive=FALSE )

# extraction of the vgg16 layer is used in the loss computation
# it extracts the last convolutional layers of vgg16 and freeze the weights 
model_vgg <- application_vgg16(weights = 'imagenet', include_top = FALSE)
layer_name="block5_conv3" 
intermediate_layer_model_vgg16 <- keras_model(inputs = model_vgg$input, outputs = get_layer(model_vgg, layer_name)$output)
freeze_weights(intermediate_layer_model_vgg16)
# remove the complete model from R
rm(model_vgg)

# extraction of the our custom VAE layer used in the loss computation 
# it extracts the last convolutional layers of custom VAE trained on 130 Planet NICFI images over
# the Brazilian state of Mato Grosso, and freeze the weights
vae_planet_128 <- load_model_hdf5("./vae_model_saved/vae_planet_MODEL_3898_0.0001061_0.9703441_MODEL.h5", custom_objects = NULL, compile = TRUE)
layer_name="block5_conv3" 
intermediate_layer_model_vgg <- keras_model(inputs = vae_planet_128$input, outputs = get_layer(vae_planet_128, layer_name)$output)
freeze_weights(intermediate_layer_model_vgg)
# remove the complete model from R
rm(vae_planet_128)

# function to compute loss with layers from vgg and our custom VAE
Feature_loss2<- function(y_true, y_pred) {
  y_true_f <- k_flatten(intermediate_layer_model_vgg(y_true))
  y_pred_f <- k_flatten(intermediate_layer_model_vgg(y_pred))
  y_true_f2 <- k_flatten(intermediate_layer_model_vgg16(y_true[,,,1:3]))
  y_pred_f2 <- k_flatten(intermediate_layer_model_vgg16(y_pred[,,,1:3]))
  result <- k_mean((y_true_f-y_pred_f)^2) +  k_mean((y_true_f2-y_pred_f2)^2)
  return(result)
}

# definition of the hard sigmoid function used in the binary mask generator
# parameters cannot be changed at this point.

hard_sigmoid_keras <- function(x) {
  k_maximum(0,
            k_minimum(1,(x-0.5 )*5000 + 2500)
  )}


# definition of the CNN model used to generate the binary masks
# with pretrained and freezed weights 
get_model <- function(input_shape = c(128, 128, 1),num_classes=k) {
  
  inputs <- layer_input(shape = input_shape)
  
  
  up_001 <- inputs %>% 
    
    layer_conv_2d(filters = 64, kernel_size = c(1, 1), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("elu")%>%  layer_dropout(0.05) %>%
    
    layer_conv_2d(filters = 64, kernel_size = c(1, 1), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("elu") %>% layer_dropout(0.05) %>%
    
    layer_conv_2d(filters = 1, 
                  kernel_size = c(1, 1),
                  activation = hard_sigmoid_keras, dtype="float32")
  
  classify2 <- layer_concatenate(list(inputs,up_001), name= "input_val")
  
  classify <-  classify2%>% layer_lambda(function(x) {x[,,,2, drop = FALSE]}, name = "output_val", dtype="float32")
  
  model <- keras_model(
    inputs = inputs,
    outputs = classify
  )
  
  return(model)
}

model_layer_01 <- get_model()

# load pretrained weights for the binary mask generator
name_weights_file=paste0("./weights_hard_sigmoid/layer_hard_sigmoid_",k,"_classes_accuracy_0001.h5")
load_model_weights_hdf5(model_layer_01, name_weights_file) 

# freeze the weights of the model, they are now non trainable, and drop out does not apply
freeze_weights(model_layer_01)

# model summary
summary(model_layer_01)



# k-textures model --------------------------------------------------------

# training parameters
batch_size = 1024L # all the images in one batch
epochs=  1024L*10# 5120 epochs run in 12-14 min on a RTX2080 GPU, you can increase this number, less than 15000 epochs are mostly sufficient to have a good convergence
lr_rate = 0.001 #
verbosity= 0 # if set to 1 or 2 training of the model is printed in the console, but not advised if you have more than 1000-2000 epochs 


# Data preparation
# in train, all images; in test, a selection of 30 images to visualize the results  

# training dataset
train <- tibble(
  img = list.files("./data_136/", pattern = ".png", full.names = TRUE),
  mask = list.files("./data_128/", pattern = ".png", full.names = TRUE)
)

# test dataset
selection = as.integer(seq(1,1024,length.out = 30))
test <- tibble(
  img = list.files("./data_136/", pattern = ".png", full.names = TRUE)[selection],
  mask = list.files("./data_128/", pattern = ".png", full.names = TRUE)[selection]
)

# pre-shuffle, only the train data
if (TRUE) {
  n_rand=sample(1:dim(train)[1],dim(train)[1])
  train=train[n_rand,]
}

# combine the train and test dataset
combined <- bind_rows(train, test)
ind <- list(analysis = seq(nrow(train)), assessment = nrow(train) + seq(nrow(test)))

# complete dataset
data <- make_splits(ind, combined)
train_samples = length(data$in_id)



# model definition -------------------------------------------------

# model
get_model <- function(input_shape = c(136, 136, 4),ncols=128, nclass=k) {
  
  inputs <- layer_input(shape = input_shape)
  
  # inputs cropped to 128x128
  inputs_128 <- inputs %>% layer_cropping_2d(4)
  
  # sigmoid layer for the binary masks generation -----------------------------------------------------------------------------------
  
  inputs000 <- inputs %>%   
    
    layer_conv_2d(filters = 64, kernel_size = c(1, 1), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("elu") %>% 
    
    layer_conv_2d(filters = 64, kernel_size = c(1, 1), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("elu") %>% 
    
    
    layer_conv_2d(filters = 1, kernel_size = c(1, 1), padding = "same")  %>%
    layer_batch_normalization() %>% 
    layer_activation("sigmoid", name = "red") %>% layer_cropping_2d(4) 
  
  # a small noise is added to the value of the sigmoid
  img <- inputs000%>% layer_gaussian_noise(stddev=0.0005)
  
  
  
  
  # binary mask generator ---------------------------------------------------
  
  # binary mask generator for an odd number of classes
  if(nclass%%2!=0){
    
    # index preparation for right et left side  
    vect_r=1:{{nclass-1}/2}
    vect_r_minus_1=vect_r-1
    
    vect_l=1:{{(nclass-1)/2}}
    vect_l_inv={{(nclass-1)/2}}:1
    vect_l_minus_1=vect_l-1
    
    # init de tensor constant
    hardsig_right_000 <- k_constant(0,shape=c(1L,128L,128L,1L)) 
    hardsig_left_000 <- k_constant(0,shape=c(1L,128L,128L,1L)) 
    
    
    for(i in 1:{{nclass-1}/2}){
      eval(parse(text= 
                   paste0("inputs_right_00",vect_r[i]," <- layer_add(list(img,k_constant(",vect_r_minus_1[i],"/nclass,shape=c(1L,128L,128L,1L))))
  hardsig_right_00",vect_r[i],"=model_layer_01(inputs_right_00",vect_r[i],")
  coeff_right_00",vect_r[i],"  <- layer_subtract(list(hardsig_right_00",vect_r[i],",hardsig_right_00",vect_r_minus_1[i],"))")
      ))
    }
    
    for(i in 1:{{nclass-1}/2}){
      eval(parse(text=
                   paste0("inputs_left_00",vect_r[i]," <- layer_add(list(layer_multiply(list(layer_subtract(list(img,k_constant(",vect_l_minus_1[i],"/nclass,shape=c(1L,128L,128L,1L)) )), k_constant(-1,shape=c(1L,128L,128L,1L)) )),k_constant(1,shape=c(1L,128L,128L,1L)) ))
  hardsig_left_00",vect_r[i],"=model_layer_01(inputs_left_00",vect_r[i],")
  coeff_left_00",vect_r[i],"  <- layer_subtract(list(hardsig_left_00",vect_r[i],",hardsig_left_00",vect_l_minus_1[i],"))")
      ))
    }
    
    # plot(as.array(img),as.array(coeff_right_001))
    
    eval(parse(text=
                 paste0("sum_layer_r_and_l=layer_add(inputs=list(",
                        paste0(paste0("coeff_right_00",vect_r,collapse=","),",",
                               paste0("coeff_left_00",vect_r,collapse=",")),"))"
                 )
    ))
    
    coeff_middle <- layer_subtract(list(k_constant(1,shape=c(1L,128L,128L,1L)), sum_layer_r_and_l))
    #plot(as.array(img),as.array(coeff_middle))
    
    float32="float32"
    eval(parse(text=
                 paste0("inputs000=layer_concatenate(inputs=list(",
                        paste0(paste0("coeff_right_00",vect_r,collapse=","),",coeff_middle,",
                               paste0("coeff_left_00",vect_l_inv,collapse=",")),
                        "))  %>%  k_cast(dtype=",float32,")")
    ))
    
    # plot(as.array(img),as.array(inputs000)[,,,5])
  }
  
  # binary mask generator for a par number of classes (and different of 2)
  if(nclass%%2==0 & nclass!=2){
    
    # index preparation for right et left side   
    vect_r=1:{{nclass}/2}
    vect_r_minus_1=vect_r-1
    
    vect_l=1:{{(nclass)/2 - 1}}
    vect_l_inv={{(nclass)/2 - 1}}:1
    vect_l_minus_1=vect_l-1
    
    # init constant tensor 
    hardsig_right_000 <- k_constant(0,shape=c(1L,128L,128L,1L)) 
    hardsig_left_000 <- k_constant(0,shape=c(1L,128L,128L,1L)) 
    
    
    for(i in 1:{{nclass}/2}){
      eval(parse(text= 
                   paste0("inputs_right_00",vect_r[i]," <- layer_add(list(img,k_constant(",vect_r_minus_1[i],"/nclass,shape=c(1L,128L,128L,1L))))
  hardsig_right_00",vect_r[i],"=model_layer_01(inputs_right_00",vect_r[i],")
  coeff_right_00",vect_r[i],"  <- layer_subtract(list(hardsig_right_00",vect_r[i],",hardsig_right_00",vect_r_minus_1[i],"))")
      ))
    }
    
    for(i in 1:{{nclass}/2 - 1}){
      eval(parse(text=
                   paste0("inputs_left_00",vect_r[i]," <- layer_add(list(layer_multiply(list(layer_subtract(list(img,k_constant(",vect_l_minus_1[i],"/nclass,shape=c(1L,128L,128L,1L)) )), k_constant(-1,shape=c(1L,128L,128L,1L)) )),k_constant(1,shape=c(1L,128L,128L,1L)) ))
  hardsig_left_00",vect_r[i],"=model_layer_01(inputs_left_00",vect_r[i],")
  coeff_left_00",vect_r[i],"  <- layer_subtract(list(hardsig_left_00",vect_r[i],",hardsig_left_00",vect_l_minus_1[i],"))")
      ))
    }
    
    
    
    eval(parse(text=
                 paste0("sum_layer_r_and_l=layer_add(inputs=list(",
                        paste0(paste0("coeff_right_00",vect_r,collapse=","),",",
                               paste0("coeff_left_00",vect_l,collapse=",")),"))"
                 )
    ))
    
    coeff_middle <- layer_subtract(list(k_constant(1,shape=c(1L,128L,128L,1L)), sum_layer_r_and_l))
    
    float32="float32"
    eval(parse(text=
                 paste0("inputs000=layer_concatenate(inputs=list(",
                        paste0(paste0("coeff_right_00",vect_r,collapse=","),",coeff_middle,",
                               paste0("coeff_left_00",vect_l_inv,collapse=",")),
                        "))  %>%  k_cast(dtype=",float32,")")
    ))
    
  }
  
  
  
  # binary mask generator for a par number of classes and k=2
  if(nclass%%2==0 & nclass==2){
    
    # index preparation for right et left side 
    vect_r=1:{{nclass}/2}
    vect_r_minus_1=vect_r-1
    
    # init constant tensor 
    hardsig_right_000 <- k_constant(0,shape=c(1L,128L,128L,1L)) 
    
    # coefficient computation
    for(i in 1:{{nclass}/2}){
      eval(parse(text= 
                   paste0("inputs_right_00",vect_r[i]," <- layer_add(list(img,k_constant(",vect_r_minus_1[i],"/nclass,shape=c(1L,128L,128L,1L))))
  hardsig_right_00",vect_r[i],"=model_layer_01(inputs_right_00",vect_r[i],")
  coeff_right_00",vect_r[i],"  <- layer_subtract(list(hardsig_right_00",vect_r[i],",hardsig_right_00",vect_r_minus_1[i],"))")
      ))
    }
    
    coeff_middle <- layer_subtract(list(k_constant(1,shape=c(1L,128L,128L,1L)), coeff_right_001))
    # concatenate
    float32="float32"
    eval(parse(text=
                 paste0("inputs000=layer_concatenate(inputs=list(",
                        paste0("coeff_right_001,","coeff_middle"),
                        "))  %>%  k_cast(dtype=",float32,")")
    ))
    
  }
  
  
  
  
  
  # texture generation ------------------------------------------------------
  
  #
  for (i in 1:nclass) {
    Idx= paste0("_",sprintf("%02.0f",i))
    Idn=as.character(i-1)
    eval(parse(text=paste0(
      "cl",Idx,"<-inputs000 %>% layer_lambda(function(x) {x[,,,",i,", drop = FALSE]})" 
    )))
    
  }
  
  #  plot(as.array(img),as.array(coeff_left_001))
  
  
  # semantic segmentation layer for the results 
  inputs00 <- inputs000 %>% k_argmax( axis = -1L)%>%
    k_cast(dtype="float32")    %>%
    layer_reshape( target_shape=c(128,128,1),dtype="float32", name="clust")
  
  
  # generation of a random class that is use to be further able to access all layer in prediction 
  random_layer<-  inputs00 %>%k_less_equal(nclass) %>% k_cast('float32') %>% 
    layer_lambda(function(x) {x[,,,1, drop = FALSE] * k_random_normal(shape=c(128,128,1),dtype = 'float32')})
  
  
  # texture generation for each binary mask -----------------------------------------------------------------------------------
  
  # texture is simulated on a larger image of 144 * 144 then cropped to 128*128
  # in order to remove border effect 
  
  crop8 <- inputs_128 %>% layer_cropping_2d(28)
  crop8 <- layer_concatenate(list(crop8,crop8),axis=2L)
  crop8 <- layer_concatenate(list(crop8,crop8),axis=1L)
  
  
  # nclass is the number of simulated textures 
  for (i in 1:nclass) {
    Idx = paste0("_",sprintf("%02.0f",i))
    same = "same"
    tanh = "tanh"
    sigmoid = "sigmoid" 
    
    
    # texture generator 
    eval(parse(text=paste0(
      "fc1",Idx," <- crop8 %>% layer_lambda(function(x) {x[,,,1, drop = FALSE]})%>% k_less(2) %>% k_cast('float32')
fc1",Idx," <- layer_multiply(list(fc1",Idx," , k_random_normal(  shape=c(1,144,144,1),dtype = 'float32')))
conv1",Idx," <- fc1",Idx," %>%  layer_conv_2d(filters = 16, kernel_size = c(3, 3),padding = ",same,")%>%
  layer_batch_normalization()%>% layer_activation_leaky_relu()
conv2",Idx," <- conv1",Idx," %>%layer_conv_2d(filters = 16,kernel_size = c(3, 3),padding = ",same,")%>%
  layer_batch_normalization()%>% layer_activation_leaky_relu()
conv3",Idx," <- conv2",Idx," %>%  layer_conv_2d(filters = 16, kernel_size = c(3, 3),padding = ",same,")%>%
  layer_batch_normalization()%>% layer_activation_leaky_relu()
conv4",Idx," <- conv3",Idx," %>%layer_conv_2d(filters = 16,kernel_size = c(3, 3),padding = ",same,")%>%
  layer_batch_normalization()%>% layer_activation_leaky_relu()
down",Idx," <- conv4",Idx," %>%
  layer_conv_2d(filters = 4, kernel_size = c(3, 3), padding = ",same,",activation = ",sigmoid,")%>%
  layer_cropping_2d(8)"
    )))
    
    
    
    # each generated texture is multiplied by its binary mask
    eval(parse(text=paste0( 
      "d",Idx," <- layer_multiply(inputs=list(cl",Idx,",down",Idx,"))"
    )))
    
  }
  
  # all the layers of the product of textures and binary masks are added to obtain the simulated image
  
  float32 = "float32"
  float16 = "float16"
  eval(parse(text=paste0( 
    'img_pred <- layer_add(inputs = list(',paste("d_",sprintf("%02.0f",1:nclass),sep="",collapse=","),"), dtype =",float32,", name = ", "'img_pred'" ,")" 
  )))
  
  
  
  # this is only a cheat to render the cluster layer accessible for prediction
  random_1 <-layer_add(inputs = list(layer_multiply(inputs=list(random_layer,k_constant(0,shape=c(1L,128L,128L,1L)))),k_constant(1,shape=c(1L,128L,128L,1L))), dtype ="float32", name="lay01")
  
  # predicted image
  img_pred_aug <- layer_multiply(inputs=list(img_pred,random_1), dtype ="float32", name="img_pred_aug")
  
  model <- keras_model(
    inputs = inputs,
    outputs = img_pred_aug
  )
  
  # compile the model
  model %>% compile(
    optimizer = optimizer_adam(lr = lr_rate,clipnorm = 1.0),
    loss = Feature_loss2,
    metrics = "mean_absolute_error"
  )
  
  return(model)
}

model <- get_model()

## to load previous weights
# load_model_weights_hdf5(model, "./weights/xxxx.h5")


# the preprocessing pipeline is adapted from https://blogs.rstudio.com/ai/posts/2019-08-23-unet/
create_dataset <- function(data, train, batch_size = 1L) {
  
  dataset <- data %>%
    tensor_slices_dataset() %>%
    dataset_map(~.x %>% list_modify(
      img =  tf$image$decode_png(tf$io$read_file(.x$img),channels=4),
      mask = tf$image$decode_png(tf$io$read_file(.x$mask),channels=4)
    )) %>%
    dataset_map(~.x %>% list_modify(
      img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
      mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
    )) 
  
  # train in batches; batch size might need to be adapted depending on
  # available memory
  dataset <- dataset %>%
    dataset_batch(batch_size)
  
  dataset %>%
    # output needs to be unnamed
    dataset_map(unname)
}  


# Training and test sets creation 
training_dataset <- create_dataset(training(data), train = TRUE)
validation_dataset <- create_dataset(testing(data), train = FALSE)
training_dataset <- dataset_repeat(training_dataset, count = epochs)

# files to save the weights and to save the history of loss and mean absolute error
nam_weight = paste0("weights/ktextures_VAE_VGG_1x1_",sprintf("%03.0f",k),"_","RGBNIR","_planet_{epoch:03d}_{loss:.7f}_{mean_absolute_error:.7f}.h5")
nam_history = paste0("epoch_history/ktextures_VAE_VGG_1x1_",sprintf("%03.0f",k),"_","RGBNIR","_planet.csv")


# callbacks, we monitor the loss and save only best models
callbacks_list <- list(
  callback_csv_logger(nam_history, separator = ";", append = FALSE),
  callback_model_checkpoint(filepath = nam_weight,
                            monitor = "loss",save_best_only = TRUE,
                            save_weights_only = TRUE, mode = "min" ) 
)



### fitting the model
fit(model,training_dataset, workers = 1,steps_per_epoch = as.integer(train_samples / batch_size), epochs = epochs,callbacks = callbacks_list,verbose=verbosity)

## if you want to see the evolution of the loss and mean absolute error
## in the R console set verbosity to 1 or 2 (not recommended for this model as Rstudio can get freeze with epoch > 2000)
## in the directory "./weights/" you should see that new weights .h5 are saved during the run every time the error decreases


# prediction    
if (PREDICTION) {
  
  # creation of the temporary directories 
  dir.create("./tmp/predtiff")
  dir.create("./tmp/FINAL")
  unlink(list.files("./tmp/predtiff",full.names = TRUE)) 
  unlink(list.files("./tmp/FINAL",full.names = TRUE)) 
  
  # name and full path of the image
  pantif_border=list.files("./PAN",pattern = ".tif",full.names = TRUE)[1] 
  
  
  # we will use the 128*128 images to store the results, they are already at the good size
  tiles_dir="./data_128/"
  
  # Data preparation
  train_pred <- tibble(
    img = list.files("./data_136/", pattern = ".png", full.names = TRUE),
    mask = list.files("./data_136/", pattern = ".png", full.names = TRUE)
  )
  selection = as.integer(seq(1,1024,length.out = 30))
  test_pred <- tibble(
    img = list.files("./data_136/", pattern = ".png", full.names = TRUE)[selection],
    mask = list.files("./data_136/", pattern = ".png", full.names = TRUE)[selection]
  )
  
  
  combined_pred <- bind_rows(train_pred, test_pred)
  ind_pred <- list(analysis = seq(nrow(train_pred)), assessment = nrow(train_pred) + seq(nrow(test_pred)))
  #splits <- make_splits(ind, combined)
  #splits
  #<Analysis/Assess/Total>
  #  <8/4/12>
  
  #data <- initial_split(data, prop = 0.8)
  data_pred <- make_splits(ind_pred, combined_pred)
  train_samples_pred = length(data_pred$in_id)  
  
  # dataset to evalute results
  evaluation_dataset <- create_dataset(training(data_pred), train = FALSE)
  #evaluate(model,evaluation_dataset)
  
  # list all the weight and order them by the accuracy
  list_weight=list.files("./weights/",pattern=paste0("_",sprintf("%03.0f",k),"_"),full.names = TRUE)
  strsplit(list_weight,"_")
  sorting_order=order(as.numeric(sapply(strsplit(list_weight,"_"),"[[",9)))
  list_weight=list_weight[sorting_order]
  
  # copy the best weight to the "./weights_save/" directory
  file.copy(list_weight[1],to="./weights_save/")
  
  # load the weights with the best accuracy
  load_model_weights_hdf5(model, list_weight[1])
  
  # load the intermediate model that predict the cluster and the simulated image
  intermediate_layer_model <- keras_model(inputs = model$input, outputs = get_layer(model, "clust")$output)
  predictions_cluster <- predict(intermediate_layer_model, evaluation_dataset)
  dim(predictions_cluster)
  intermediate_layer_model5 <- keras_model(inputs = model$input, outputs = get_layer(model, "img_pred")$output)
  predictions_image <- predict(intermediate_layer_model5, evaluation_dataset)
  dim(predictions_image)
  
  
  
  # prediction of the 4-bands simulated image (4096x4096x4) -----------------
  
  # list of the 128x128 images that will receive the results 
  images_iter2 <- list.files(tiles_dir, pattern = ".tif", full.names = TRUE)#[samples_index] 
  images_iter <- basename(images_iter2)
  images_iter3=substr(images_iter,nchar(images_iter)[1]-10,nchar(images_iter)[1])
  # r_tmp=raster::brick(images_iter2[1])
  
  # to run in parallel, 
  cl=makePSOCKcluster(parallel::detectCores()) 
  registerDoParallel(cl)
  
  # save the model results in each 128x128 images
  system.time({
    
    list_nam <- foreach(i = iapply(X=predictions_image,MARGIN=1),j=icount(),.packages = c("raster")) %dopar% {
      
      new_img1=t(i[,,1])
      new_img2=t(i[,,2])
      new_img3=t(i[,,3])
      new_img4=t(i[,,4])
      
      img_array=array(data = NA, dim = c(128,128,4),dimnames = NULL)
      img_array[,,1]=as.matrix(new_img1)*255
      img_array[,,2]=as.matrix(new_img2)*255
      img_array[,,3]=as.matrix(new_img3)*255
      img_array[,,4]=as.matrix(new_img4)*255
      
      r=brick(images_iter2[j])
      
      
      r[[1]]=img_array[,,1]
      r[[2]]=img_array[,,2]
      r[[3]]=img_array[,,3]
      r[[4]]=img_array[,,4]
      
      writeRaster(r, paste("./tmp/predtiff/",images_iter3[j],sep=""),datatype='INT1U',overwrite=TRUE)
      
    }
    
  }) 
  stopCluster(cl)
  
  
  # merge all the 128x128 images that contain the result to have to image full size
  
  # name of the resulting simulated image
  name_final=basename(pantif_border)
  name_final=sub(".tif",paste0("_k_",sprintf("%03.0f",k),"_img_rgbn.tif"), name_final)
  name_final=sub(".tif","", name_final) 
  
  # making the list of the 128x128 images to merge  
  list_datamask=unlist(list.files("./tmp/predtiff",full.names = TRUE))
  sink(file="./tmp/list.txt")
  cat(list_datamask,sep="\n")
  sink()
  
  # full name of the .vrt and tif of the result
  name_final_vrt=paste("./MASK/",name_final,".vrt",sep="")
  name_final_TIF=paste("./MASK/",name_final,".TIF",sep="")
  
  # merge operation, the result is the simulated image with size 4096 x 4096   
  system(paste(gdalbuildvrt, name_final_vrt ,"-input_file_list","./tmp/list.txt"))
  system(paste(gdal_translate, "-co COMPRESS=DEFLATE -co NUM_THREADS=12L -a_nodata 255", name_final_vrt, name_final_TIF))
  
  # remove .vrt  
  unlink(list.files(paste(getwd(),"/MASK/",sep=""),pattern='.vrt',full.names = TRUE))
  
  
  # prediction of the clusters (4096x4096x1)  -------------------------------
  
  # clean files
  unlink(list.files("./tmp/predtiff",full.names = TRUE))
  unlink(list.files("./tmp/FINAL",full.names = TRUE))
  
  # list of the 128x128 images that will receive the results 
  images_iter2 <- list.files(tiles_dir, pattern = ".tif", full.names = TRUE)#[samples_index] 
  images_iter <- basename(images_iter2)
  images_iter3=substr(images_iter,nchar(images_iter)[1]-10,nchar(images_iter)[1])
  
  # r_tmp=raster::brick(images_iter2[1])
  
  # library(doParallel)
  
  # 
  cl=makePSOCKcluster(parallel::detectCores()) # 12 cpu cores, but set the value to your number of cpu cores
  registerDoParallel(cl)
  
  # save the model results in each 128x128 images  
  
  system.time({
    
    list_nam <- foreach(i = iapply(X=predictions_cluster,MARGIN=1),j=icount(),.packages = c("raster")) %dopar% {
      
      new_img1=t(i[,])
      new_img2=t(i[,])
      new_img3=t(i[,])
      new_img4=t(i[,])
      
      img_array=array(data = NA, dim = c(128,128,4),dimnames = NULL)
      img_array[,,1]=as.matrix(new_img1)*floor(255/k)
      img_array[,,2]=as.matrix(new_img2)*floor(255/k)
      img_array[,,3]=as.matrix(new_img3)*floor(255/k)
      img_array[,,4]=as.matrix(new_img4)*floor(255/k)
      
      r=brick(images_iter2[j])
      r[[1]]=img_array[,,1]
      r[[2]]=img_array[,,2]
      r[[3]]=img_array[,,3]
      r[[4]]=img_array[,,4]
      
      writeRaster(r[[1]], paste("./tmp/predtiff/",images_iter3[j],sep=""),datatype='INT1U',overwrite=TRUE)
    }
    
  }) 
  stopCluster(cl)
  
  # set names for the files
  name_final=basename(pantif_border)
  name_final=sub(".tif",paste0("_k_",sprintf("%03.0f",k),"_cluster.tif"), name_final)
  name_final=sub(".tif","", name_final)
  
  # list images to combine
  list_datamask=unlist(list.files("./tmp/predtiff",full.names = TRUE))
  
  # create txt file with the image names
  sink(file="./tmp/list.txt")
  cat(list_datamask,sep="\n")
  sink()
  
  # run the mosaic
  name_final_vrt=paste("./MASK/",name_final,".vrt",sep="")
  name_final_TIF=paste("./MASK/",name_final,".TIF",sep="")
  system(paste(gdalbuildvrt, name_final_vrt ,"-input_file_list","./tmp/list.txt" ))
  system(paste(gdal_translate, "-co COMPRESS=DEFLATE -co NUM_THREADS=12L -a_nodata 255", name_final_vrt, name_final_TIF))
  
  # remove files  
  unlink(list.files(paste(getwd(),"/MASK/",sep=""),pattern='.vrt',full.names = TRUE))
  unlink(list.files("./tmp/",full.names = TRUE),recursive=TRUE)
  
  
}

# finish
beepr::beep("complete")

