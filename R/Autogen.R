#' @title Autogen will be called from ConvertandgraphIns() and be used to automatically create 4 heatmaps after conversion.
#' @param dataset The data set to graph.
#' @param mapname The name of the raster image to plot against.
#' @param string The text you wish to display on the image.
#' @description This function will automatically return a plot from the given data. Must use converted data. Set for 1024 Insurgency image defaults.
#' @export


Autogen = function(dataset,mapname,string)
{

  Text = grid::grobTree(grid::textGrob(string,x=.1,y=.95,hjust=0,gp=gpar(col="red",fontsize=13,fontface="bold")))

  plot = ggplot2::ggplot(dataset, ggplot2::aes(x=X,y=Y)) + geom_point(shape=1) + xlim(0,1024) + ylim(-1024,0) + theme_nothing()


  plot = plot + theme_nothing() + annotation_custom(mapname,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf) + stat_density2d(aes(fill=..level..,alpha=..level..),geom="polygon") + scale_fill_gradient(low="blue",high="red") + geom_density2d() + coord_fixed(ratio = 1) +geom_point(data = dataset,aes(x=X,y=Y),shape = 1,alpha=.3) + annotation_custom(Text)




  return(plot)

}
