#'@title Plot For HeatMap Generation Allows full control over many variables.
#'@description Function that uses ggplot to create the basic geom_point with given demensions (default 1024 for Insurgency) with theme_nothing() from cowplot.
#'@param dataset The data set being used for graphic
#'@param X The column X is being stored
#'@param Y The column Y is being stored
#'@param xpixel The size of x background image in pixels that is being used. Defaults to 1024.
#'@param ypixel The size of y background image in pixels that is being used. Defaults to 1024.
#'@param Map gives the map name which is used to grab the correct image file from the enviroment. Use case sensative name of raster image. IE if image name is imarket type in "imarket" and not "Imarket" or "iMarket"
#'@param string is the title you wish to see at the top left of the image.
#'@param alphalev this is the value you wish to set the color alpha levels.
#'@return The return object is the plot created by ggplot.
#'@export

CreateHeatMap = function(dataset,X = dataset[,2],Y = dataset[,3],xpixel=1024,ypixel=1024,Map,string,alphalev=1){


  print("Map argument is case sensative")


  Text = grobTree(textGrob(string,x=.1,y=.95,hjust=0,gp=gpar(col="red",fontsize=13,fontface="bold")))

  plot = ggplot(dataset, aes(x=X,y=Y)) + geom_point(shape=1) + xlim(0,xpixel) + ylim(-ypixel,0) + theme_nothing()

  filename = Map

  plot = plot + theme_nothing() + annotation_custom(filename,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf) + stat_density2d(aes(fill=..level..,alpha=..level..),alpha = alphalev,geom="polygon") + scale_fill_gradient(low="blue",high="red") + geom_density2d() + coord_fixed(ratio = 1) +geom_point(data = dataset,aes(x=X,y=Y),shape = 1,alpha=.3) + annotation_custom(Text)
  return(plot)

}
