# Script: Preparation of a Planet NICFI quad to a RGB-NIR image and creating tiles for the k-textures model ------------------
# Authors: Fabien H. Wagner (wagner.h.fabien@gmail.com) and Ricardo Dal'Agnol da Silva (ricds@hotmail.com)
# Date Created: 2022-05-16

# here the objective is to save Planet NICFI full image without the 5th band, which does not have data
# and to scale the all the bands to 8 bytes (between 0 and 255)
# this process reduces the file size to ~30% of the original size

# we also create tiles of 136x136 and 128x128 which are required for the model as input and output, respectively

# the input Planet NICFI image should be in the "quad" directory
# the outputs will be placed in the 'data_136' and 'data_128' directories

# load libraries
library(raster)
library(sf)

# set working directory
setwd("K:/k-textures_Planet_Zenodo/")

# gdal function that are used to mosaic images
# you need to have OSGEO4W installed in your computer with GDAL - https://www.osgeo.org/projects/osgeo4w/
# https://qgis.org/en/site/forusers/download.html -> OSGeo4W Network Installer
# you also need to check if the gdal_retile.py script is in the folder "C:/OSGeo4W/apps/Python39/Scripts/gdal_retile.py"
# we use that function in this script
gdal_translate = paste("C:/OSGeo4W/OSGeo4W.bat","C:/OSGeo4W/bin/gdal_translate.exe")
gdalbuildvrt = paste("C:/OSGeo4W/OSGeo4W.bat","C:/OSGeo4W/bin/gdalbuildvrt.exe")

# clean temporary files
unlink(list.files("./tmp",full.names = TRUE),recursive=FALSE )

# if you want to remove all the prepocessed data order to prepare all
# from an original Planet quad set to TRUE
if (TRUE){
  unlink(list.files("./data_128/",full.names = TRUE),recursive=FALSE )
  unlink(list.files("./data_136/",full.names = TRUE),recursive=FALSE )
  unlink(list.files("./PAN/",full.names = TRUE),recursive=FALSE )
  unlink(list.files("./PANPRED/",full.names = TRUE),recursive=FALSE )
}





# image scale to 8 bytes -----------------------------------------------

# list images
img_list=list.files(path = "./quad/",full.names = TRUE)
i_img=1

# set the directory for the temporary files
tiles_dir = paste0(getwd(),"/tmp/")

# save individual bands of the image
img_name_01 = paste0(tiles_dir, sub(".tif","_01.tif",basename(img_list[i_img])))
img_name_02 = paste0(tiles_dir, sub(".tif","_02.tif",basename(img_list[i_img])))
img_name_03 = paste0(tiles_dir, sub(".tif","_03.tif",basename(img_list[i_img])))
img_name_04 = paste0(tiles_dir, sub(".tif","_04.tif",basename(img_list[i_img])))

# get each individual band from the image
system.time({system(paste(gdal_translate, "-b 1 -colorinterp red", img_list[i_img], img_name_01))})
system.time({system(paste(gdal_translate, "-b 2 -colorinterp green", img_list[i_img], img_name_02))})
system.time({system(paste(gdal_translate, "-b 3 -colorinterp blue", img_list[i_img], img_name_03))})
system.time({system(paste(gdal_translate, "-b 4 -colorinterp alpha", img_list[i_img], img_name_04))})

# scaling the bands and saving in int1u 
multi_in=list.files(path = "./tmp",full.names = TRUE)
multi_in_10000=multi_in
multi_in_10000=sub(".tif","_scaled.tif",multi_in)

# remove excess data and scale data to 8 bytes
for (m in 1:4) {
  if (m %in% c(1,2,3)){
    system(paste("C:/OSGeo4W/OSGeo4W.bat gdal_calc --calc","minimum(2540,A)/10","--format GTiff --type Byte","--co COMPRESS=DEFLATE","-A",multi_in[m],"--A_band 1","--outfile",multi_in_10000[m]))
  }
  if (m %in% c(4)){
    system(paste("C:/OSGeo4W/OSGeo4W.bat gdal_calc --calc","minimum(2540,A/3.937)/10","--format GTiff --type Byte","--co COMPRESS=DEFLATE","-A",multi_in[m],"--A_band 1","--outfile",multi_in_10000[m]))
  }
}

# reconstruct the image with 4-bands
multi_out=multi_in[1]
multi_out=sub("_01.tif",".tif",multi_out)
multi_out=sub("tmp","PAN",multi_out)
multi_vrt=multi_out
multi_vrt=sub(".tif",".vrt",multi_vrt)

# reconstruct the image with the 4-bands scaled
system.time({system(paste(gdalbuildvrt, "-separate" ,  multi_vrt, paste(multi_in_10000[c(3,2,1,4)],collapse = " "))) })
system.time({system(paste(gdal_translate, multi_vrt, multi_out,"-co COMPRESS=DEFLATE -co PREDICTOR=2 --config GDAL_CACHEMAX 100 -co NUM_THREADS=12")) })
unlink(multi_vrt)
unlink(list.files("./tmp",full.names = TRUE),recursive=FALSE )



# preparation of the 4-bands RGBNIR image with a border of 4 pixels --------

# clean tmp files
unlink(list.files("./tmp",full.names = TRUE),recursive=FALSE )

# set img names
pantif=list.files("./PAN",full.names = TRUE)  
pantif_nam=basename(pantif)

# names of the subset image that will contain the borders and temporary images
panpredtiftmp=sub("PAN","tmp",pantif)
pantop=sub("PAN","tmp",pantif)
pantop=sub("\\.tif","\\_top.tif",pantop)
panbot=sub("PAN","tmp",pantif)
panbot=sub("\\.tif","\\_bot.tif",panbot)
panlef=sub("PAN","tmp",pantif)
panlef=sub("\\.tif","\\_lef.tif",panlef)
panrig=sub("PAN","tmp",pantif)
panrig=sub("\\.tif","\\_rig.tif",panrig)

# name and path of the image with the border
panpredtif=sub("PAN","PANPRED",pantif)

# resolution of the image
res_im=res(raster(pantif[1]))[1]


j=1
#for (j in 1:length(pantif)) {  

# define extent
extn=extent(raster(pantif[j]))

# put bands together
img_name =sub("PAN","tmp",pantif[j])
system.time({system(paste(gdal_translate, "-b 1 -b 2 -b 3 -b 4 -colorinterp red,green,blue,alpha", pantif[j], img_name))})

# create top border
system.time({system(paste(gdal_translate, "-srcwin 0 1 4096 4 -colorinterp red,green,blue,alpha", pantif[j],pantop[j] ))})
rt=brick(pantop[j])
rt=flip(rt,"y")
extent(rt)=c(extn@xmin,extn@xmax,extn@ymax,extn@ymax + 4*res_im)
pantoptmp=sub("_top","_top_tmp",pantop[j])
writeRaster(rt,filename = pantoptmp,datatype="INT1U",overwrite=T)

# create bottom border
system.time({system(paste(gdal_translate, "-srcwin 0 4091 4096 4 -colorinterp red,green,blue,alpha", pantif[j],panbot[j] ))})
rb=brick(panbot[j])
rb=flip(rb,"y")
extent(rb)=c(extn@xmin,extn@xmax,extn@ymin- (4*res_im),extn@ymin)
panbottmp=sub("_bot","_bot_tmp",pantop[j])
writeRaster(rb,filename = panbottmp,datatype="INT1U",overwrite=T)

# merge top border and bottom border
list_datamask=c( img_name,pantoptmp,panbottmp)
list_datamask=paste(list_datamask,collapse =" ")
name_final_vrt=sub(".tif","_tmp.vrt",panpredtiftmp[j])
name_final_TIF=sub(".tif","_tmp.tif",panpredtiftmp[j])
system(paste(gdalbuildvrt, name_final_vrt ,list_datamask ))
system.time({system(paste(gdal_translate, "-co COMPRESS=DEFLATE -a_nodata 255 -colorinterp red,green,blue,alpha", name_final_vrt, name_final_TIF))})

# right border
extn=extent(raster(name_final_TIF))
system.time({system(paste(gdal_translate, "-srcwin 4091 0  4 4104 -colorinterp red,green,blue,alpha", name_final_TIF,panrig[j] ))})
rr=brick(panrig[j])
rr=flip(rr,"x")
extent(rr)=c(extn@xmax,extn@xmax+(4*res_im),extn@ymin,extn@ymax)
panrigtmp=sub("_rig","_rig_tmp",panrig[j])
writeRaster(rr,filename = panrigtmp,datatype="INT1U",overwrite=T)

# left border
extn=extent(raster(name_final_TIF))
system.time({system(paste(gdal_translate, "-srcwin 1 0  4 4104 -colorinterp red,green,blue,alpha", name_final_TIF,panlef[j] ))})
rl=brick(panlef[j])
rl=flip(rl,"x")
extent(rl)=c(extn@xmin-(4*res_im),extn@xmin,extn@ymin,extn@ymax)
panleftmp=sub("_lef","_lef_tmp",panlef[j])
writeRaster(rl,filename = panleftmp,datatype="INT1U",overwrite=T)

# merge of the img + bottom and top border with the left and the right border
list_datamask=c( name_final_TIF,panrigtmp,panleftmp)
list_datamask=paste(list_datamask,collapse =" ")
name_final_vrt=sub("_tmp.tif",".vrt",name_final_TIF)
name_final_TIF=sub("_tmp.tif",".tif",name_final_TIF)
name_final_TIF=sub("tmp","PANPRED",name_final_TIF)

system(paste(gdalbuildvrt, name_final_vrt ,list_datamask ))
system.time({system(paste(gdal_translate, "-co COMPRESS=DEFLATE -a_nodata 255 -colorinterp red,green,blue,alpha", name_final_vrt, name_final_TIF))})

unlink(list.files("./tmp",full.names = TRUE),recursive=FALSE )
print(j)
#}   



# preparation of the tiled images to train the model and make prediction --------

# create the directory for the tiled image, 128 x 128 pixels (output) and 136 x 136 (input)
dir.create('./data_128/')
dir.create('./data_136/')

# select the image with 4 bands and the original size of 4096 x 4096
pantif_border=list.files("./PAN",pattern = ".tif",full.names = TRUE) 

# generate the 128 x 128 tif and png images in with gdal_retile.py (Need Osgeo installed and check the path)

# where to write the tiles
tiles_dir="./data_128/"
# .tif generation
system.time({
  system(paste("C:/OSGeo4W/OSGeo4W.bat","python","C:/OSGeo4W/apps/Python39/Scripts/gdal_retile.py"," -ps 128 128","--config GDAL_CACHEMAX 100", "-targetDir",tiles_dir,  pantif_border, sep=" "))
})
# .png generation
system.time({
  system(paste("C:/OSGeo4W/OSGeo4W.bat","python","C:/OSGeo4W/apps/Python39/Scripts/gdal_retile.py"," -ps 128 128","-of png","--config GDAL_CACHEMAX 100", "--config GDAL_PAM_ENABLED NO", "-targetDir",tiles_dir,  pantif_border, sep=" "))
})


# preparation of 136*136 image tiles for the model inputs 

# the image in "./PANPRED" directory is the same that in "./PAN" but with a border of 8 pixels 
# on each side filled with the mirroring image. The size of this image is 4104 x 4104 
pantif_border=list.files("./PANPRED",pattern = ".tif",full.names = TRUE)

# generate the 136x136 tif and png images in with gdal_retile.py and 8 pixels of overlap between images
# where to write the tiles
tiles_dir="./data_136/"
# .tif generation
system.time({
  system(paste("C:/OSGeo4W/OSGeo4W.bat","python","C:/OSGeo4W/apps/Python39/Scripts/gdal_retile.py"," -ps 136 136","-overlap 8","--config GDAL_CACHEMAX 100", "-targetDir",tiles_dir,  pantif_border, sep=" "))
})
# .png generation
system.time({
  system(paste("C:/OSGeo4W/OSGeo4W.bat","python","C:/OSGeo4W/apps/Python39/Scripts/gdal_retile.py"," -ps 136 136","-overlap 8", "-of png","--config GDAL_CACHEMAX 100", "--config GDAL_PAM_ENABLED NO", "-targetDir",tiles_dir,  pantif_border, sep=" "))
})

