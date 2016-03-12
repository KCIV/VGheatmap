#' @export
#' @param Image Set address of Image file, file.choose() by default.
#' @param string Set name you wish the image to be called.
#' @param env Enviroment set to global so user will see it.
#' @description Will take in an image location (file.choose() default), String to call the object, and Enviroment location (Global defaul). This allows the user to create a custom raster image
#' @title CreateMapRaster



CreateMapRaster = function(Image = file.choose(),string,env=globalenv())
{

  imagemax = readJPEG(Image)

  newraster = rasterGrob(imagemax, interpolate=TRUE)


  assign(paste(string),newraster,envir=env)

}
