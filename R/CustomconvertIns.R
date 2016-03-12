#' @export
#' @param dataset The data you wish to convert
#' @param mapname Name of the map
#' @param Posx Posx offset
#' @param Posy Posy offset
#' @param scale scale value
#' @param env set the enviroment, Global by default.
#' @return Does not return a value does create multiple data sets.
#' @description Forumla to take in a raw data set and convert it from given values.
#' @title CustomconvertIns

CustomconvertIns = function(dataset,mapname,Posx,Posy,scale,env=globalenv())
{
    # saves the converted files to specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-Posx)/scale), ((dataset[,3]-Posy)/scale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-Posx)/scale), ((dataset[,9]-Posy)/scale))
    names(converteddeaths) = c("Side","X","Y")

    assign(paste(mapname,"Totalkills",sep=""),convertedkills,envir=env)
    assign(paste(mapname,"Totaldeaths",sep=""),converteddeaths,envir=env)

    # split the deaths and kills by "side"
    splitdatadeaths = split(converteddeaths,converteddeaths[,1])
    splitdatakills = split(convertedkills,convertedkills[,1])


    DS = splitdatadeaths$Security
    DI = splitdatadeaths$Insurgents

    KS = splitdatakills$Security
    KI = splitdatakills$Insurgents


    assign(paste(mapname,"DeathSec",sep=""),DS,envir=env)
    assign(paste(mapname,"DeathIns",sep=""),DI,envir=env)
    assign(paste(mapname,"KillSec",sep=""),KS,envir=env)
    assign(paste(mapname,"KillIns",sep=""),KI,envir=env)
}
