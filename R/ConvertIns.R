#' @export
#' @param dataset The Data you wish to convert.
#' @param mapname The map name you wish to graph the data against. This is used to get the conversion for the data set. (conversion is map specific)
#' @param env set the enviroment.
#' @return Data sets you wish to graph.
#' @description Will take in a data set, run a conversion based on the map name given, then return a series of datasets to then be used for graphing.
#' @title ConvertIns

ConvertIns = function(dataset,mapname, env = globalenv()){
  # have to convert in any case
  #
  # Map OverView Data
  # https://github.com/jaredballou/insurgency-data/blob/7aaf99049ca98aa0b9225696ca562b83d607a83b/maps/parsed/dry_canal.json


  DIT = "Death Insurgent"
  DST = "Death Security"
  KIT = "Kill Insurgent"
  KST = "Kill Security"
  TDT = "Total Deaths"
  TKT = "Total Kills"


  ContactPosx = -4648
  ContactPosy = 3900
  Contactscale = 9

  MarketPosx = -6386
  MarketPosy = 5390
  Marketscale = 12

  BuhrizPosx = -11503
  BuhrizPosy = 9100
  Buhrizscale = 18

  DistrictPosx = -9976
  DistrictPosy = 10261
  Districtscale = 12

  HeightsPosx = -7150
  HeightsPosy = 6500
  Heightsscale = 14

  StationPosx = -6534
  StationPosy = 6747
  Stationscale = 12

  MinistryPosx = -7000
  MinistryPosy = 4650
  Ministryscale = 13

  DryCanalPosx = -4800
  DryCanalPosy = 4000
  DryCanalscale = 10.3

  TellPosx = -6899
  TellPosy = 2360
  Tellscale = 10

  UprisingPosx = -3497
  UprisingPosy = 4054
  Uprisingscale = 7

  VerticalityPosx = -7124
  VerticalityPosy = 6169
  Verticalityscale = 12

  EmbassyPosx = -5156
  EmbassyPosy = 4771
  Embassyscale = 9.35

  KandagalPosx = -5873
  KandagalPosy = 7426
  Kandagalscale = 11

  PanjPosx = -5374
  PanjPosy = 5842
  Panjscale = 11

  PeakPosx = -8812
  PeakPosy = 8534
  Peakscale = 17

  RevoltPosx = -5793
  RevoltPosy = 4728
  Revoltscale = 10

  SiegePosx = -5400
  SiegePosy = 4110
  Siegescale = 8

  SinjarPosx = -7351
  SinjarPosy = 8007
  Sinjarscale = 13.5

  # completed section for
  # Revolt
  # Peak
  # Panj
  # Market
  # Buhriz
  # District
  # Heights
  # Station
  # Ministry
  # Contact
  # DryCanal
  # Tell
  # Uprising
  # Verticality
  # Kandagal
  # Seige
  # Sinjar

  if(mapname=="sinjar" || mapname =="Sinjar")
  {
    # saves the converted files to "Sinjar" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-SinjarPosx)/Sinjarcale), ((dataset[,3]-SinjarPosy)/Sinjarscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-SinjarPosx)/Sinjarscale), ((dataset[,9]-SinjarPosy)/Sinjarscale))
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



  if(mapname=="siege" || mapname =="Siege")
  {
    # saves the converted files to "Siege" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-SiegePosx)/Siegescale), ((dataset[,3]-SiegePosy)/Siegescale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-SiegePosx)/Siegescale), ((dataset[,9]-SiegePosy)/Siegescale))
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



  if(mapname=="revolt" || mapname =="Revolt")
  {
    # saves the converted files to "Revolt" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-RevoltPosx)/Revoltscale), ((dataset[,3]-RevoltPosy)/Revoltscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-RevoltPosx)/Revoltscale), ((dataset[,9]-RevoltPosy)/Revoltscale))
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


  if(mapname=="peak" || mapname =="Peak")
  {
    # saves the converted files to "Peak" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-PeakPosx)/Peakscale), ((dataset[,3]-PeakPosy)/Peakscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-PeakPosx)/Peakscale), ((dataset[,9]-PeakPosy)/Peakscale))
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


  if(mapname=="panj" || mapname =="Panj")
  {
    # saves the converted files to "Panj" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-PanjPosx)/Panjscale), ((dataset[,3]-PanjPosy)/Panjscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-PanjPosx)/Panjscale), ((dataset[,9]-PanjPosy)/Panjscale))
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

  if(mapname=="kandagal" || mapname =="Kandagal")
  {
    # saves the converted files to "Kandagal" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-KandagalPosx)/Kandagalscale), ((dataset[,3]-KandagalPosy)/Kandagalscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-KandagalPosx)/Kandagalscale), ((dataset[,9]-KandagalPosy)/Kandagalscale))
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



  if(mapname=="embassy" || mapname =="Embassy")
  {
    # saves the converted files to "Embassy" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-EmbassyPosx)/Embassyscale), ((dataset[,3]-EmbassyPosy)/Embassyscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-EmbassyPosx)/Embassyscale), ((dataset[,9]-EmbassyPosy)/Embassyscale))
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

  if(mapname=="verticality" || mapname =="Verticality")
  {
    # saves the converted files to "Verticality" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-VerticalityPosx)/Verticalityscale), ((dataset[,3]-VerticalityPosy)/Verticalityscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-VerticalityPosx)/Verticalityscale), ((dataset[,9]-VerticalityPosy)/Verticalityscale))
    names(converteddeaths) = c("Side","X","Y")

    assign(paste("convertedkills",mapname,sep=""),convertedkills,envir=env)
    assign(paste("converteddeaths",mapname,sep=""),converteddeaths,envir=env)

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


  if(mapname=="uprising" || mapname =="Uprising")
  {
    # saves the converted files to "Uprising" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-UprisingPosx)/Uprisingscale), ((dataset[,3]-UprisingPosy)/Uprisingscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-UprisingPosx)/Uprisingscale), ((dataset[,9]-UprisingPosy)/Uprisingscale))
    names(converteddeaths) = c("Side","X","Y")

    assign(paste("convertedkills",mapname,sep=""),convertedkills,envir=env)
    assign(paste("converteddeaths",mapname,sep=""),converteddeaths,envir=env)

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


  if(mapname=="tell" || mapname =="Tell")
  {
    # saves the converted files to "Tell" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-TellPosx)/Tellscale), ((dataset[,3]-TellPosy)/Tellscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-TellPosx)/Tellscale), ((dataset[,9]-TellPosy)/Tellscale))
    names(converteddeaths) = c("Side","X","Y")

    assign(paste("convertedkills",mapname,sep=""),convertedkills,envir=env)
    assign(paste("converteddeaths",mapname,sep=""),converteddeaths,envir=env)

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


  if(mapname=="drycanal" || mapname =="DryCanal")
  {
    # saves the converted files to "DryCanal" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-DryCanalPosx)/DryCanalscale), ((dataset[,3]-DryCanalPosy)/DryCanalscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-DryCanalPosx)/DryCanalscale), ((dataset[,9]-DryCanalPosy)/DryCanalscale))
    names(converteddeaths) = c("Side","X","Y")

    assign(paste("convertedkills",mapname,sep=""),convertedkills,envir=env)
    assign(paste("converteddeaths",mapname,sep=""),converteddeaths,envir=env)

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

  if(mapname=="contact" || mapname =="Contact")
  {
    # saves the converted files to "Contact" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-ContactPosx)/Contactscale), ((dataset[,3]-ContactPosy)/Contactscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-ContactPosx)/Contactscale), ((dataset[,9]-ContactPosy)/Contactscale))
    names(converteddeaths) = c("Side","X","Y")

    assign(paste("convertedkills",mapname,sep=""),convertedkills)
    assign(paste("converteddeaths",mapname,sep=""),converteddeaths)

    # split the deaths and kills by "side"
    splitdatadeaths = split(converteddeaths,converteddeaths[,1])
    splitdatakills = split(convertedkills,convertedkills[,1])


    DS = splitdatadeaths$Security
    DI = splitdatadeaths$Insurgents

    KS = splitdatakills$Security
    KI = splitdatakills$Insurgents


    assign(paste(mapname,"DeathSec",sep=""),DS)
    assign(paste(mapname,"DeathIns",sep=""),DI)
    assign(paste(mapname,"KillSec",sep=""),KS)
    assign(paste(mapname,"KillIns",sep=""),KI)


  }

  if(mapname=="market" || mapname =="Market")
  {
    # saves the converted files to "market" specific tag.


    convertedkills = data.frame(dataset[,1], ((dataset[,2]-MarketPosx)/Marketscale), ((dataset[,3]-MarketPosy)/Marketscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-MarketPosx)/Marketscale), ((dataset[,9]-MarketPosy)/Marketscale))
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

  if(mapname=="buhriz" || mapname =="Buhriz")
  {
    # saves the converted files to "buhriz" specific tag.
    #
    convertedkills = data.frame(dataset[,1], ((dataset[,2]-BuhrizPosx)/Buhrizscale), ((dataset[,3]-BuhrizPosy)/Buhrizscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-BuhrizPosx)/Buhrizscale), ((dataset[,9]-BuhrizPosy)/Buhrizscale))
    names(converteddeaths) = c("Side","X","Y")

    assign(paste("convertedkills",mapname,sep=""),convertedkills,envir=env)
    assign(paste("converteddeaths",mapname,sep=""),converteddeaths,envir=env)

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

  if(mapname=="district" || mapname =="District")
  {
    # saves the converted files to "District" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-DistrictPosx)/Districtscale), ((dataset[,3]-DistrictPosy)/Districtscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-DistrictPosx)/Districtscale), ((dataset[,9]-DistrictPosy)/Districtscale))
    names(converteddeaths) = c("Side","X","Y")

    assign(paste("convertedkills",mapname,sep=""),convertedkills,envir=env)
    assign(paste("converteddeaths",mapname,sep=""),converteddeaths,envir=env)

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


  if(mapname=="heights" || mapname =="Heights")
  {
    # saves the converted files to "Heights" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-HeightsPosx)/Heightsscale), ((dataset[,3]-HeightsPosy)/Heightsscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-HeightsPosx)/Heightsscale), ((dataset[,9]-HeightsPosy)/Heightsscale))
    names(converteddeaths) = c("Side","X","Y")

    assign(paste("convertedkills",mapname,sep=""),convertedkills,envir=env)
    assign(paste("converteddeaths",mapname,sep=""),converteddeaths,envir=env)

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

  if(mapname=="ministry" || mapname =="Ministry")
  {
    # saves the converted files to "Ministry" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-MinistryPosx)/Ministryscale), ((dataset[,3]-MinistryPosy)/Ministryscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-MinistryPosx)/Ministryscale), ((dataset[,9]-MinistryPosy)/Ministryscale))
    names(converteddeaths) = c("Side","X","Y")

    assign(paste("convertedkills",mapname,sep=""),convertedkills,envir=env)
    assign(paste("converteddeaths",mapname,sep=""),converteddeaths,envir=env)

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


  if(mapname=="station" || mapname =="Station")
  {
    # saves the converted files to "Station" specific tag.

    convertedkills = data.frame(dataset[,1], ((dataset[,2]-StationPosx)/Stationscale), ((dataset[,3]-StationPosy)/Stationscale))
    names(convertedkills) = c("Side","X","Y")

    converteddeaths = data.frame(dataset[,7], ((dataset[,8]-StationPosx)/Stationscale), ((dataset[,9]-StationPosy)/Stationscale))
    names(converteddeaths) = c("Side","X","Y")

    assign(paste("convertedkills",mapname,sep=""),convertedkills,envir=env)
    assign(paste("converteddeaths",mapname,sep=""),converteddeaths,envir=env)

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
}
