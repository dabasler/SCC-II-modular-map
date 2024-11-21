#sccii_plotmapp
# This script provides functions create multi-layered maps for the SCCII-hoelstein site
# Code by D.Basler 2022-2024, based on rudimentary code by C.Zahnd 2020
options(stringsAsFactors = FALSE)
#Load Packages:
library(dplyr)
library(plotrix)
library(shape)
library(png)
library(readxl)


############### PLOTTING FUNCTIONS
data_import<-function(mapdatapath,metadatapath){
  metadata <- read_excel(path = file.path(mapdatapath ,"datasheets/Metadata_Trees_SCCII.xlsx"), sheet = 1, na = "NA")
  metadata <-LV03toLocal(metadata)
  metadata <-metadata[!is.na(metadata$species),]
  metadata <-metadata[-grep("x",metadata$nr),]
  names(metadata)[7]<-'dbh'
  metadata$dbh[is.na(metadata$dbh)]<-metadata$dbh_2022[is.na(metadata$dbh)]
  metadata$nr_pos[is.na(metadata$nr_pos)]<-"t"
  metadata$dm_pos[is.na(metadata$dm_pos)]<-"l"
  # Convert 'y' to TRUE and 'n' to FALSE
  cols_to_convert <- sapply(metadata, function(col) all(col %in% c('y', 'n')))
  metadata[cols_to_convert] <- lapply(metadata[cols_to_convert], function(col) col == 'y')
  #relabel the alive broken "ab" with alive "a"
  for (i in which(startsWith(names(metadata),"status"))) metadata[metadata[,i]=="ab" & !is.na(metadata[,i]) ,i]<-"a"
  for (i in which(substr(names(metadata),1,5) %in% c("point","sapfl","bandd","leafw","nsc_2","phenoc","soilm"))) metadata[is.na(metadata[,i]),i]<-FALSE
  metadata <<-metadata
  traps     <<- read_excel(path = file.path(mapdatapath ,"./datasheets/littertraps.xlsx"), sheet = 1, na = "NA")
  paths     <<- read_excel(path = file.path(mapdatapath ,"./datasheets/footpaths.xlsx"), sheet = 1, na = "NA")
  suction   <<- read_excel(path = file.path(mapdatapath ,"./datasheets/suctionplates.xlsx"), sheet = 1, na = "NA")
  drop_icon <<- readPNG(file.path(mapdatapath ,"./datasheets/drop.png"))
  roofs     <<- read_excel(path = file.path(mapdatapath ,"./datasheets/roofs_final.xlsx"), sheet = 1, na = "NA")
  raintraps <<- read_excel(path = file.path(mapdatapath ,"./datasheets/raintraps.xlsx"), sheet = 1, na = "NA")
  # Soil pits are hard-coded, see below
  sapflow_logger <<- read.table(file.path(mapdatapath ,"./datasheets/sapflow_logger_pos.csv"),sep="\t",header=TRUE)
  sapflow_trees <- read.table(file.path(mapdatapath ,"./datasheets/sapflow_logger_trees.csv"),sep="\t",header=TRUE)
  names(sapflow_trees)[2]<-"nr"
  sapflow_trees<-merge(sapflow_trees,metadata[,c("nr","defx","defy")],by="nr")
  sapflow_trees<<-sapflow_trees
  craneroute <<- read_excel(path = file.path(mapdatapath ,"./datasheets/craneroute.xlsx"), sheet = 1, na = "NA")
  message("Data imported")
}


LV03toLocal<-function(data){
  # Turn the coordinate system:
  data$newx <- data$xcor - 625458.4
  data$newy <- data$ycor - 254150.0
  data$angle <- atan(data$newy / data$newx)
  data$dist <- data$newy / sin(data$angle)
  a <- atan(76.5 / 92.4)
  data$corrangle <- data$angle - a
  data$defx <- cos(data$corrangle) * data$dist
  data$defy <- sin(data$corrangle) * data$dist
  return (data)
}

# Not used for map drawing but for exporting mapped features to LV03
LocaltoLV03<-function(defx,defy){
  dist<-sqrt(defx^2+defy^2)
  if (defy<0) dist <- -dist
  corrangle<-acos(defx/dist)
  angle<-corrangle + a
  #if (angle>pi) angle<-angle-pi
  newy <- dist*sin(angle)
  newx<- newy/tan(angle)
  xcor  <- newx + 625458.4 
  ycor <- newy + 254150.0
  return(c(xcor,ycor))
}

col2hex <- function(x, alpha = FALSE) {
  args <- as.data.frame(t(col2rgb(x, alpha = alpha)))
  args <- c(args, list(names = x, maxColorValue = 255))
  do.call(rgb, args)
}

parse_flt_string<-function(fltstr,year,metadata){ # mdn is metadatanames , do not include _year in filter arguments, this will be applied from the year selected in the Golbal Options
  mdn<-names(metadata)
  if (is.null(fltstr) || fltstr=="all" || fltstr=="") return(TRUE)  
  if (grepl("&$", fltstr) || grepl("\\|$", fltstr)) fltstr<-substr(fltstr,1,nchar(fltstr)-1) # Remove & or | from end of lines
  ystr<-as.character(year)
  # Handle missing DBH measurements
  if (grepl("dbh",fltstr) &  sprintf("dbh_%s",ystr) %in% mdn) {
    metadata$dbh[!is.na(metadata[[sprintf("dbh_%s",ystr)]])]<- metadata[!is.na(metadata[[sprintf("dbh_%s",ystr)]]),sprintf("dbh_%s",ystr)]
    mdn<-mdn[-which(mdn==sprintf("dbh_%s",ystr))]
  }
  ycols<-substr(mdn[endsWith(mdn,ystr)],1,nchar(mdn[endsWith(mdn,ystr)])-5) #basename of columns wich contain year
  for ( n in  ycols) fltstr  <- gsub(sprintf("\\b%s(?!_\\d{4})\\b",n), sprintf("%s_%s",n,ystr), fltstr, perl = TRUE) #replace shorthands with year if not already specified
  for (n in mdn) fltstr<-sub(n,sprintf("metadata$%s",n),fltstr) # Add Table Name
  fltstr <- sub('alive',sprintf("metadata$status_%s=='a'",ystr),fltstr)
  fltstr <- sub('dead',sprintf("metadata$status_%s!='a'",ystr),fltstr)
  print(fltstr)
  out<-FALSE
  try (out<-eval(parse(text = fltstr)),silent=TRUE)
  return(out)
}


  
treecolor<-function(species=NULL,as.hex=TRUE){
  tc<-data.frame(
    latin = c("Pinus sylvestris",
              "Quercus sp.",
              "Acer pseudoplatanus",
              "Fraxinus excelsior",
              "Fagus sylvatica", 
              "Abies alba",
              "Carpinus betulus", 
              "Ilex aquifolium", 
              "Juglans regia",
              "Picea abies",
              "Prunus avium",
              "Pseudotsuga menziesii",
              "Sorbus torminalis",
              "Ulmus sp.",
              "trees"),
    german = c("Wald-Kiefer",
               "Eiche (sp.)",
               "Berg-Ahorn",
               "Gemeine Esche",
               "Rotbuche",
               "Weißtanne",
               "Hainbuche",
               "Europäische Stechpalme",
               "Walnussbaum",
               "Gemeine Fichte",
               "Vogelkirsche",
               "Douglasie",
               "Elsbeere",
               "Ulme (sp.)",
               "Bäume"),
    english = c("Scots Pine",
                "Oak (sp.)",
                "Sycamore Maple",
                "Ash Tree",
                "European Beech",
                "Silver Fir",
                "Hornbeam",
                "Holly",
                "Walnut Tree",
                "Norway Spruce",
                "Wild Cherry",
                "Douglas Fir",
                "Wild Service Tree",
                "Elm (sp.)",
                "Trees"),
    conifer=c(T,F,F,F,F,T,F,F,F,T,F,T,F,F,NA),
    species=c("ps" ,"qs" ,"ap"  ,"fe"  ,"fs"  ,"aa"  ,"cb"  ,"ia"  ,"jr"  ,"pa"  ,"pm"  ,"st"  ,"us"  ,"pr", "tr"),
    col=c("darkseagreen3", "darkorange3",  "gold1",  "black",  "dodgerblue2",  "lightseagreen",  "grey60",  "red",  "saddlebrown",  "forestgreen",  "limegreen",  "orange",  "navajowhite3",  "maroon3","grey60")
  )
  tc<-tc[order(tc$conifer,tc$species),]
  if (as.hex) tc$col<-sapply (tc$col, FUN = function (x) paste(c("#",format(as.hexmode(col2rgb(x)[,1]),width=2, upper.case = TRUE)),collapse=""))
  if (is.null(species)) return(tc)
  species[species=="qp"]<-"qs"  # Fix old Quercus Labels
  return (sapply(species,function(x) tc$col[tc$species==x]))
}


plot_basemap<-function(crane=TRUE,grid=TRUE,footpaths=TRUE,road=TRUE,gates=TRUE,fence=TRUE,containers=TRUE,presentation=FALSE){
  # Main area and crane radius: 
  if (presentation){
    par(mar = c(4,4,0.5,1))
    with(metadata, plot(defx, defy, asp = 1, type = "n", ylim = c(-1, 141), xlim = c(0, 121), axes = T,xlab="x (m)",ylab="y (m)",xaxs="i",yaxs="i",las=1))
  } else {
    par(mar = c(0,0,0,0))
    with(metadata, plot(defx, defy, asp = 1, type = "n", ylim = c(-25, 150), xlim = c(-5, 125), axes = F))
  }
  
  if(crane){
    rect(-1, -1, 121, 141, NA, col = rgb(0, 0.3, 0, alpha = 0.05))
    draw.circle(68,74, radius = 62.5, border = NA, col = "white")
    rect(66,72,70,76, border = "grey40", col = "grey")
    points(68, 74, col = "yellow", pch = 16, cex = 1.5)
  }
  if (presentation)box()
  if (grid){
    # 10x10 grid:
    for(i in seq(10,110,10)){
      lines(x = c(i,i), y = c(-1,141), col = "grey", lwd = 0.5)
    }
    for(i in seq(10,130,10)){
      lines(y = c(i,i), x = c(-1,121), col = "grey", lwd = 0.5)
    }
  }
  
  if (fence){
    # Fence:
    rect(-1, -1, 121, 141, border = "grey60", lwd = 2, col = NA)
  }
  
  if (footpaths)  {
    # Footpaths:
    # Plastic:
    with(paths[paths$path == 3,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 4,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 5,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 6,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 8,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 10,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 12,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 13,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 14,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 15,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 16,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 17,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 18,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 19,], lines(x, y, col= "grey70", lwd = 1.2))
    with(paths[paths$path == 20,], lines(x, y, col= "grey70", lwd = 1.2))
    # Metal:
    with(paths[paths$path == 1,], lines(x, y, col= "grey30", lwd = 1.2))
    with(paths[paths$path == 2,], lines(x, y, col= "grey30", lwd = 1.2))
    with(paths[paths$path == 7,], lines(x, y, col= "grey30", lwd = 1.2))
    with(paths[paths$path == 9,], lines(x, y, col= "grey30", lwd = 1.2))
    with(paths[paths$path == 11,], lines(x, y, col= "grey30", lwd = 1.2))
  }
  
  if (road){
    
    road_coords<-data.frame(
    xcor = c(625413.9542,	625423.1958,	625431.712,	625440.5346,	625445.762,	625449.3078,	625460.3975,	625465.4145,	625471.3802,	625474.4391,	625475.7256,	625477.121,	625478.2393,	625481.4835,	625485.2336,	625487.6725,	625492.0081),
    ycor = c(254292.7828,	254280.3995,	254270.5523,	254265.2055,	254259.602,	254254.1324,	254242.9907,	254237.8912,	254230.9959,	254222.6324,	254208.7998,	254186.5272,	254176.1618,	254163.1728,	254150.7701,	254139.9223,	254121.1155)
    )
    road_coords<-LV03toLocal(road_coords)[7:12,]
    # Main road:
    lines(x = c(road_coords$defx,27.5), y = c(road_coords$defy,-1), lwd = 5, col = "grey30")
    lines(x = c(60.840754, 60.8), y = c(70.353841, 78), lwd = 5, col = "grey30")
  }
  
  if (containers){
    # Crane and containers:
    rect(63.6,72.5,65.4,75.5, border = "black", lwd = 1.5)
    filledrectangle(mid = c(47.3,25.6), wx = 2, wy = 5, angle = -35, lwd = 1.5, lcol = "black", col = "white")
  }
  if (gates){
    # Gates:
    lines(x = c(0,7), y = c(-1.3,-1.3), col = "grey60", lwd = 3)
    lines(x = c(113,120), y = c(-1.3,-1.3), col = "grey60", lwd = 3)
    lines(x = c(0,7), y = c(141.3,141.3), col = "grey60", lwd = 3)
    lines(x = c(113,120), y = c(141.3,141.3), col = "grey60", lwd = 3)
    lines(x = c(24,31), y = c(-1.3,-1.3), col = "grey60", lwd = 5)
    text(c(3.5, 116.5, 3.5, 116.5), c(-3, -3, 143, 143), c("Gate 1", "Gate 4", "Gate 2", "Gate 3"), cex = 0.5)
    text(27.5, -3, "Main Gate", cex = 0.5, font = 2)
  }
  
  # Scale:
  rect(c(0,5,10,15,20,30,40,50,60,70), rep(-6,10), c(5,10,15,20,30,40,50,60,70,80), rep(-7,10), border = "black", col = rep(c("white", "black"), 5))
  text(c(0,5,10,15,20,30,40,50,60,70,80), rep(-9, 11), labels = c("0","5","10","15","20","30","40","50","60","70","80m"), cex = 0.5)
  
  # Indicate North:
  #arrows(x0 = 116, y0 = -25, x1 = 116 + (cos(0.907571)*5), y1 = -25 + (sin(0.907571)*5), length = 0.06, lwd = 1.5, angle = 20)
  #text(117.2, -25.2, "N", srt = -38, cex = 0.5)
  arrows(x0 = 116, y0 = -9, x1 = 116 + (cos(0.907571)*5), y1 = -9 + (sin(0.907571)*5), length = 0.06, lwd = 1.5, angle = 20)
  text(117.2, -9.2, "N", srt = -38, cex = 0.5)
  
}



plot_trees<-function(data,numbers=TRUE,palpha=255,psize="dbh",pcol="species"){
  if ( is.null(palpha) || is.null(psize) || is.null(pcol) ||  is.null(numbers) ) return()
  # Trees
  data<-data[!is.na(data$species),]
  if (pcol=="species") pcol= sprintf("%s%02x",treecolor(data$species),as.integer(palpha))
  if (psize == "dbh") {with(data, points(defx, defy, pch = 16, cex = (sqrt(dbh)/4)+0.1, col = pcol,xpd=TRUE)) # USE DBH
  }else {with(data, points(defx, defy, pch = 16, cex = psize, col = pcol,xpd=TRUE))}
  
  if (numbers){
    # Tree numbers:
    if (nrow(data[data$nr_pos == "b",])>0) with(data[data$nr_pos == "b",], text(defx, defy - (((sqrt(dbh)/4)+1)/2)-0.5, labels=nr, cex= 0.4))#,silent=TRUE)
    if (nrow(data[data$nr_pos == "l",])>0) with(data[data$nr_pos == "l",], text(defx - (((sqrt(dbh)/4)+1)/2)-0.9, defy, labels=nr, cex= 0.4))#,silent=TRUE)
    if (nrow(data[data$nr_pos == "t",])>0) with(data[data$nr_pos == "t",], text(defx, defy + (((sqrt(dbh)/4)+1)/2)+0.4, labels=nr, cex= 0.4))#,silent=TRUE)
    if (nrow(data[data$nr_pos == "r",])>0) with(data[data$nr_pos == "r",], text(defx + (((sqrt(dbh)/4)+1)/2)+0.9, defy, labels=nr, cex= 0.4))#,silent=TRUE)
    # Exceptional tree 
    if (nrow(data[data$nr_pos == "sp",])>0) with(data[data$nr_pos == "sp",], text(defx - (((sqrt(dbh)/4)+1)/2)+0.3, defy- (((sqrt(dbh)/4)+1)/2), labels=nr, cex= 0.4))#,silent=TRUE)
  }
}



mark_trees<-function(data,mark,markcol,pcex=1){
  data$mark <-  mark
  data$markcol <-  markcol
  markwidth<-3
  markheight<-2
  with( data[data$mark == "circle",], points(defx, defy, pch = 1, cex = 2.4+pcex-1, col = data$markcol[mark == "circle"] ,lwd=2 ))
  with( data[data$mark == "cross",],  points(defx, defy, pch = 4, cex = 2.4+pcex-1, col = data$markcol[mark == "cross"] ,lwd=2 ))
  
  with(data[data$mark == "underline" & data$nr_pos == "b",], segments(defx-markwidth/2, defy - (((sqrt(dbh)/4)+1)/2)-1,defx+markwidth/2, defy - (((sqrt(dbh)/4)+1)/2)-1,lwd=3,col=data$markcol[data$mark == "underline" & data$nr_pos == "b"]))
  with(data[data$mark == "underline" & data$nr_pos == "t",], segments(defx-markwidth/2, defy + (((sqrt(dbh)/4)+1)/2)-0.8,defx+markwidth/2, defy + (((sqrt(dbh)/4)+1)/2)-0.8,lwd=3,col=data$markcol[data$mark == "underline" & data$nr_pos == "t"]))
  with(data[data$mark == "underline" & data$nr_pos == "l",], segments(defx - (((sqrt(dbh)/4)+1)/2)-0.9-markwidth/2, defy-1 ,defx - (((sqrt(dbh)/4)+1)/2)-0.9+markwidth/2, defy-1,lwd=3,col=data$markcol[data$mark == "underline" & data$nr_pos == "l"]))
  with(data[data$mark == "underline" & data$nr_pos == "r",], segments(defx + (((sqrt(dbh)/4)+1)/2)+0.9-markwidth/2, defy-1 ,defx + (((sqrt(dbh)/4)+1)/2)+0.9+markwidth/2, defy-1,lwd=3,col=data$markcol[data$mark == "underline" & data$nr_pos == "r"]))

  with(data[data$mark == "rect" & data$nr_pos == "b",], rect(
    defx - markwidth/2,
    defy - (((sqrt(dbh)/4)+1)/2)-0.5 - markheight/2,
    defx + markwidth/2,
    defy - (((sqrt(dbh)/4)+1)/2)-0.5 + markheight/2,
    col = data$markcol[data$mark == "rect" & data$nr_pos == "b"],border=NA)
  )
  
  with(data[data$mark == "rect" & data$nr_pos == "l",], rect(
    defx - (((sqrt(dbh)/4)+1)/2)-0.9 - markwidth/2,
    defy - markheight/2,
    defx - (((sqrt(dbh)/4)+1)/2)-0.9 + markwidth/2,
    defy + markheight/2,
    col = data$markcol[data$mark == "rect" & data$nr_pos == "l"],border=NA)
  )
  
  with(data[data$mark == "rect" & data$nr_pos == "t",], rect(
    defx - markwidth/2,
    defy + (((sqrt(dbh)/4)+1)/2)+0.4 - markheight/2,
    defx + markwidth/2,
    defy + (((sqrt(dbh)/4)+1)/2)+0.4 + markheight/2,
    col = data$markcol[data$mark == "rect" & data$nr_pos == "b"],border=NA)
  )
  
  with(data[data$mark == "rect" & data$nr_pos == "l",], rect(
    defx + (((sqrt(dbh)/4)+1)/2)+0.9 - markwidth/2,
    defy - markheight/2,
    defx + (((sqrt(dbh)/4)+1)/2)+0.9 + markwidth/2,
    defy + markheight/2,
    col = data$markcol[data$mark == "rect" & data$nr_pos == "l"],border=NA)
  )
}


plot_littertraps<-function(){
  # Littertraps:
  with(traps, points(x = x, y = y, cex = 0.7, col = "grey50"))
  with(traps[traps$nr_pos == "b",], text(x = x, y = y, labels = nr, pos = 1, offset = 0.25, cex = 0.3, col = "grey50"))
  with(traps[traps$nr_pos == "l",], text(x = x, y = y, labels = nr, pos = 2, offset = 0.2, cex = 0.3, col = "grey50"))
  with(traps[traps$nr_pos == "t",], text(x = x, y = y, labels = nr, pos = 3, offset = 0.2, cex = 0.3, col = "grey50"))
  with(traps[traps$nr_pos == "r",], text(x = x, y = y, labels = nr, pos = 4, offset = 0.2, cex = 0.3, col = "grey50"))
}

plot_suctionplates<-function(){
  # Suction plates:
  with(suction, points(x = x, y = y, cex = 0.7, pch = 19, col = "deepskyblue"))
  with(suction, points(x = x, y = y, cex = 0.8, pch = "*", col = "white"))
  with(suction[suction$nr_pos == "b",], text(x = x, y = y, labels = nr, pos = 1, offset = 0.25, cex = 0.3, col = "grey50"))
  with(suction[suction$nr_pos == "l",], text(x = x, y = y, labels = nr, pos = 2, offset = 0.2, cex = 0.3, col = "grey50"))
  with(suction[suction$nr_pos == "t",], text(x = x, y = y, labels = nr, pos = 3, offset = 0.2, cex = 0.3, col = "grey50"))
  with(suction[suction$nr_pos == "r",], text(x = x, y = y, labels = nr, pos = 4, offset = 0.2, cex = 0.3, col = "grey50"))
}


plot_raintraps<-function(){
  rt_dir<-(raintraps$dir/180)*pi
  raintraps$dy <- cos(rt_dir + 0.907571 )*5
  raintraps$dx <- sin(rt_dir + 0.907571 )*5
  # Raintraps:
  with(raintraps, segments(x, y,x+dx,y+dy , lwd=2 ,col = "gray40"))
  with(raintraps, points(x = x, y = y, cex = 0.8, pch = 19, col = "gray40"))
  with(raintraps, points(x = x, y = y, cex = 0.8, pch = 1, col = "gray20"))
  with(raintraps[raintraps$nr_pos == "b",], text(x = x, y = y, labels = nr, pos = 1, offset = 0.25, cex = 0.3, col = "grey50"))
  with(raintraps[raintraps$nr_pos == "l",], text(x = x, y = y, labels = nr, pos = 2, offset = 0.2, cex = 0.3, col = "grey50"))
  with(raintraps[raintraps$nr_pos == "t",], text(x = x, y = y, labels = nr, pos = 3, offset = 0.2, cex = 0.3, col = "grey50"))
}




plot_soilpits<-function(numbers=TRUE){
  # ADD Soil Pits
  soilpits<-list(
  c(47.5,106.5,  1),   # x,y,id
  c(  36, 80.8,  2),
  c(  45, 60.8,  3),
  c(  32, 41.5,  4),
  c(71.5,105.5,  5),
  c(  94, 83.5,  6),
  c(  96, 43.5,  7),
  c(77.5,   28,  8),
  c(  75,   47,  9),
  c( 108,   69, 10), ##
  c( 86,   68, 11),
  c(  85,  122, 12),
  c(  61,  121, 13),
  c(21.5,   69, 14),
  c(  24,   39, 15))
  soilpits<-data.frame(id=sapply(soilpits,FUN=function(x)x[[3]]),x=sapply(soilpits,FUN=function(x)x[[1]]),y=sapply(soilpits,FUN=function(x)x[[2]]) )
  for (i in 1:nrow(soilpits)){
    filledrectangle(mid = c(soilpits$x[i],soilpits$y[i]), wx = 1, wy = 1.6, angle = 0, lcol = "black", col = "black")
    if (numbers) text(soilpits$x[i],soilpits$y[i],soilpits$id[i], col = "white", cex = 0.4)
  }
}

plot_roofs<-function(numbers=TRUE,plotall="ALL"){

  roofs$rf<-paste(roofs$nr,roofs$treatment)
  roofs$col<- "#00000033"
  roofs$col[roofs$treatment=="t"]<- "#FF000033"
  
  if (plotall=="Control") roofs<-roofs[!roofs$treatment=="t",]
  if (plotall=="Treatment") roofs<-roofs[roofs$treatment=="t",]

  
  rtxt<-as.data.frame(rbind(c(71, 128, "T1","t"),
  c(71, 133, "C1","f"),
  c(90, 95,  "C2","f"),
  c(90, 90,  "T2","t"),
  c(92, 65,  "C3","f"),
  c(100,65,  "T3","t"),
  c(72, 32,  "C4","f"),
  c(80, 47,  "T4","t"),
  c(35, 32,  "C5","f"),
  c(19, 32,  "T5","t"),
  c(24, 57,  "C6","f"),
  c(34, 59,  "T6","t"),
  c(45, 96,  "T7","t"),
  c(45, 90,  "C7","f")))
  names(rtxt)<-c("x","y","label","treatment")  
  rtxt$x<-as.numeric(rtxt$x)
  rtxt$y<-as.numeric(rtxt$y)
  tcex=par("cex.axis")*0.9
  
  
  if (plotall=="Control") rtxt<-rtxt[!rtxt$treatment=="t",]
  if (plotall=="Treatment") rtxt<-rtxt[rtxt$treatment=="t",]
  
    for (rf in unique(roofs$rf)){
      x   <- roofs$x[roofs$rf==rf]
      y   <- roofs$y[roofs$rf==rf]
      col <- roofs$col[roofs$rf==rf][1]
      polygon(x,y,col=col,border=NA)
    }
  if (numbers)   {
    for (i in 1:nrow(rtxt)) text(rtxt$x[i], rtxt$y[i], rtxt$label[i],cex=tcex)  
  }
    
}




plot_full_tree_measurements<-function(data,variables=c('banddm','pointdm'),year=NA){
  if (is.na(year)) year <- as.numeric(substr(Sys.Date(),1,4))
  if (nrow(data)==0) return()
  # Girth tapes:
  if ('banddm' %in% variables){
    colname<-sprintf("banddm_%s",year)
    with(data[data[,colname]== TRUE,], text(defx, defy, "G", col = "white", cex = 0.4))
  }
  # Point dendrometer:
  if ('pointdm' %in% variables){
    colname<-sprintf("pointdm_%s",year)
    with(data[data[,colname] == TRUE & data$dm_pos == "l",], arrows(x0 = defx - ((sqrt(dbh)/4)+1)/4, y0 = defy, x1 = defx - ((sqrt(dbh)/4)+1)/2, angle = 150, col = "red", length = 0.04, lwd = 1))
    with(data[data[,colname] == TRUE & data$dm_pos == "r",], arrows(x0 = defx + ((sqrt(dbh)/4)+1)/4, y0 = defy, x1 = defx + ((sqrt(dbh)/4)+1)/2, angle = 150, col = "red", length = 0.04, lwd = 1))
    with(data[data[,colname] == TRUE & data$dm_pos == "b",], arrows(x0 = defx, y0 = defy - ((sqrt(dbh)/4)+1)/4, y1 = defy - ((sqrt(dbh)/4)+1)/2, angle = 150, col = "red", length = 0.04, lwd = 1))
  }  
  
  #Soil Moisture
  if ('soilmt' %in% variables){
    colname<-sprintf("soilmt_%s",year)
    with(data[data[,colname] == TRUE,] , rasterImage(drop_icon, defx+0.8, defy+0.6, defx+1.3, defy+1.5))
  }    
  # if ('leafwp' %in% variables){
  #   colname<-sprintf("leafwp_%i",year)
  #   with(data[data[,colname]== TRUE,], text(defx, defy, "G", col = "white", cex = 0.4))
  # }
  # 
  # if ('nsc' %in% variables){
  #   colname<-sprintf("nsc_%i",year)
  #   with(data[data[,colname]== TRUE,], text(defx, defy, "G", col = "white", cex = 0.4))
  # }
  # 
  if ('sapflow' %in% variables){
    colname<-sprintf("sapflow_%s",year)
    if (nrow(data[data[,colname] == TRUE,])>0)  {
      with(data[data[,colname] == TRUE & data$dm_pos != "r",], arrows(x0 = defx+((sqrt(dbh)/4)+1)/3, y0 = defy-((sqrt(dbh)/4)+1)/3,  y1= defy+((sqrt(dbh)/4)+1)/4, angle = 30, col = "red", length = 0.04, lwd = 1))
      with(data[data[,colname] == TRUE & data$dm_pos == "r",], arrows(x0 = defx-((sqrt(dbh)/4)+1)/3, y0 = defy-((sqrt(dbh)/4)+1)/3,  y1= defy+((sqrt(dbh)/4)+1)/4, angle = 30, col = "red", length = 0.04, lwd = 1)) 
    }
  }
  # if ('phenocam' %in% variables){
  #   colname<-sprintf("phenocam_%i",year)
  #   with(data[data[,colname]== TRUE,], text(defx, defy, "G", col = "white", cex = 0.4))
  # }

}



#TO DO cable to trees
plot_sapflow<-function(cable=TRUE){
  boxdim<-1
  if (cable){
    for (log in sapflow_logger$logger){
      segments(
        sapflow_logger$x[sapflow_logger$logger==log],sapflow_logger$y[sapflow_logger$logger==log],
        sapflow_trees$defx[sapflow_trees$logger==log],sapflow_trees$defy[sapflow_trees$logger==log],
        col="grey55")
    }
  }
  ## LOGGERS
  rect(sapflow_logger$x-boxdim/2,sapflow_logger$y-boxdim/2,sapflow_logger$x+boxdim/2,sapflow_logger$y+boxdim/2,col="white",border="black")
  text(sapflow_logger$x,sapflow_logger$y,sapflow_logger$logger,cex=0.4)
}


plotmap<-function(metadata,GlobalMapOptions){
  message("Base Map")
  plot_basemap(
    crane=GlobalMapOptions$crane,
    grid=GlobalMapOptions$grid,
    footpaths=GlobalMapOptions$footpaths,
    road=GlobalMapOptions$road,
    gates=GlobalMapOptions$gates,
    fence=GlobalMapOptions$fence,
    containers=GlobalMapOptions$containers,
    presentation=GlobalMapOptions$presentation
  )
  message("+ Roofs")
  if (GlobalMapOptions$roofs) plot_roofs(GlobalMapOptions$roof_labels)
  # plot raintraps below trees
  message("+ RainTraps")
  if (GlobalMapOptions$raintraps)     plot_raintraps()
  message("+ Campaign")
  # Campaign routes
  if (!is.na( GlobalMapOptions$craneroute))if (GlobalMapOptions$craneroute=="NA" | GlobalMapOptions$craneroute=="None") GlobalMapOptions$craneroute<-NA
  if (!is.na(GlobalMapOptions$craneroute)) craneroute_trees<-plot_craneroute(GlobalMapOptions$craneroute,GlobalMapOptions$craneroutearrows)
  message("+ Trees")
  ntree_layers<-length(GlobalMapOptions$trees)
  if (ntree_layers>0){
    for (i in 1:ntree_layers){
      ## PARSE FLT
      flt<-parse_flt_string(GlobalMapOptions$trees[[i]]$flt,GlobalMapOptions$year,metadata)
      
      if (length(GlobalMapOptions$trees[[i]]$highlight_shape)==0) return()
      
      if (is.na(GlobalMapOptions$trees[[i]]$highlight_shape)){
        
        message(sprintf("  + plot tree layer %i/%i (%i flt)",i,ntree_layers,length(metadata$nr[flt])))
        
        plot_trees(metadata[flt,],numbers = GlobalMapOptions$trees[[i]]$numbers,
                   psize = ifelse(GlobalMapOptions$trees[[i]]$size_dbh,"dbh",GlobalMapOptions$trees[[i]]$customsize),
                   palpha = GlobalMapOptions$trees[[i]]$alpha,
                   pcol = ifelse(GlobalMapOptions$trees[[i]]$speciescolor,"species",GlobalMapOptions$trees[[i]]$customcolor)
                   )
        ms<-names(GlobalMapOptions$trees[[i]]$measurements) [GlobalMapOptions$trees[[i]]$measurements==TRUE]
        print(ms)
        plot_full_tree_measurements(metadata[flt,],variables=ms,year=GlobalMapOptions$year)
      }else{
        message(sprintf("  + mark tree layer %i (%i flt)",i,length(metadata$nr[flt])))
        print(GlobalMapOptions$trees[[i]]$highlight_color)
        mark_trees(metadata[flt,], GlobalMapOptions$trees[[i]]$highlight_shape,GlobalMapOptions$trees[[i]]$highlight_color)
      }
    }
  }
  if (!is.na(GlobalMapOptions$craneroute)) mark_trees(metadata[which(metadata$nr %in% craneroute_trees),],"circle","red")
  
  message("+ Suction plates")
  if (GlobalMapOptions$suctionplates) plot_suctionplates()
  message("+ Littertraps")
  if (GlobalMapOptions$littertraps)   plot_littertraps()
  message("+ Soilpits")
  if (GlobalMapOptions$soilpits)      plot_soilpits()
  if (!GlobalMapOptions$presentation){
    message("+ Legend")
    plot_legend(GlobalMapOptions)
  }
  #message("+ Tree Measnuremts")
  #plot_full_tree_measurements(metadata)
  
  message("+ Sapflow")
  if (GlobalMapOptions$sapflow_log) plot_sapflow(GlobalMapOptions$sapflow_cable)
  message("+ Title")
  plot_title(GlobalMapOptions$title,GlobalMapOptions$year)
  message("ALL DONE")
}


plot_legend<-function(GlobalMapOptions){
  longlabel<-FALSE
  # Get all Species that are put on the MAP, limit legend to species that are actually on the map
  species<-NA
  measurements<-NA
  ntree_layers<-length(GlobalMapOptions$trees)
  if (ntree_layers>0){
    for (i in 1:ntree_layers){
      flt<-parse_flt_string(GlobalMapOptions$trees[[i]]$flt,GlobalMapOptions$year,metadata)
      if (GlobalMapOptions$trees[[i]]$speciescolor==TRUE & is.na(GlobalMapOptions$trees[[i]]$highlight_shape) ){
        species<-c(species,unique(metadata$species[flt]))
        measurements<-c(measurements,names(GlobalMapOptions$trees[[i]]$measurements))#[GlobalMapOptions$trees[[i]]$measurements])
      } else {
        species<-c(species,"trees")
      }
    }
  }
  species<-unique(species[!is.na(species)])
  measurements<-unique(measurements[!is.na(measurements)])
  ## Plot Measurements
  lpos<-expand.grid(x=c(0,24,48,72,96),y=c(-15,-18,-21,-24,-27,-30))
  cp<-1
  
  ## TO DO: Add  Generic species to Treecolors
  
  if (length(species)>0){
    trees = treecolor()
    trees <- trees[trees$species %in% species,]
    np<-nrow(trees)
    if (np>0){
      tid<-as.numeric((matrix(1:(ceiling(nrow(trees)/5)*5),ncol=5,byrow=TRUE)))
      tid<-tid[tid<=np]
      points(lpos$x[1:np][tid],lpos$y[1:np][tid],pch = 16, col= trees$col)
      text(lpos$x[1:np][tid]+0.5,lpos$y[1:np][tid], font = 3, cex = 0.5, pos = 4, labels= trees$latin)
      cp<-np+1
    }
  }
  
  if (GlobalMapOptions$crane){
    rect(lpos$x[cp]-1.5,lpos$y[cp]+1.5,lpos$x[cp]+1.5,lpos$y[cp]-1.5, border = "grey40", col = "grey")
    points(lpos$x[cp],lpos$y[cp],pch = 16, col= "yellow")
    text(lpos$x[cp]+0.5,lpos$y[cp], font = 3, cex = 0.5, pos = 4, labels= "crane")
    cp<-cp+1
  }

  #lpos<-expand.grid(x=c(0,24,48,72,96),y=c(-15,-18,-21,-24,-27))
  #lpos<- rbind(lpos,c(102,-27))
  
  # Fourth line:
  #main Road
  if (GlobalMapOptions$road){
  lines(c(lpos$x[cp],lpos$x[cp]+3,lpos$x[cp]+5), c(lpos$y[cp],lpos$y[cp]-0.4,lpos$y[cp]-0.2), lwd = 5, col = "grey30")
  text(lpos$x[cp] + 4.5 , lpos$y[cp] , font = 3, cex = 0.5, pos = 4,  labels = "forest road")
  cp<-cp+1
  }
  if (GlobalMapOptions$footpaths){
  # boardwalk
  lines(c(lpos$x[cp]-0.5, lpos$x[cp]+1.5), c(lpos$y[cp]-0.5,lpos$y[cp]-0.2), lwd = 1.2, col = "grey70")
  lines(c(lpos$x[cp]-3,lpos$x[cp]-1,lpos$x[cp]-0.5), c(lpos$y[cp],lpos$y[cp]-0.5,lpos$y[cp]-0.5), lwd = 1.2, col = "grey30")
  text(lpos$x[cp] + 0.5 , lpos$y[cp] , font = 3, cex = 0.5, pos = 4,  labels = "boardwalk (metal, plastic)")
  cp<-cp+1
  longlabel=TRUE
  }
  if (GlobalMapOptions$containers){
  #container
  if (longlabel)lpos$x[cp]<-lpos$x[cp]+5
  filledrectangle(mid = c(lpos$x[cp],lpos$y[cp]), wx = 3, wy = 1.5, lwd = 3, lcol = "black", col = "white")
  text(lpos$x[cp] + 0.5 , lpos$y[cp] , font = 3, cex = 0.5, pos = 4,  labels =  "container")
  cp<-cp+1
  longlabel=FALSE
  }
  if (GlobalMapOptions$soilpits){
  #Soilpit
  filledrectangle(mid = c(lpos$x[cp],lpos$y[cp]), wx = 1, wy = 1.6, angle = 0, lcol = "black", col = "black")
  text(lpos$x[cp],lpos$y[cp], "1", col = "white", cex = 0.4)
  text(lpos$x[cp] + 0.5 , lpos$y[cp] , font = 3, cex = 0.5, pos = 4,  labels =  "soil profile")
  cp<-cp+1
  longlabel=FALSE
  }
  if (GlobalMapOptions$littertraps){
  #littertrap
  points(lpos$x[cp], lpos$y[cp], cex = 0.7, col = "grey50")
  text(lpos$x[cp] + 0.5 , lpos$y[cp] , font = 3, cex = 0.5, pos = 4,  labels =  "litter trap")
  cp<-cp+1
  longlabel=FALSE
  }
  
  if (GlobalMapOptions$sapflow_log){
    # sapflow Logger
    boxdim<-1
    rect(lpos$x[cp]-boxdim/2,lpos$y[cp]-boxdim/2,lpos$x[cp]+boxdim/2,lpos$y[cp]+boxdim/2,col="white",border="black")
    text(lpos$x[cp],lpos$y[cp],1,cex=0.4)
    text(lpos$x[cp] + 0.5 , lpos$y[cp] , font = 3, cex = 0.5, pos = 4,  labels =  "sapflow logger")
    cp<-cp+1
    longlabel=FALSE
  }
  
  
  # Fifth line:
  
  #GirthTape
  if ("banddm" %in% measurements){
    if (!lpos$x[cp]==0) if (longlabel)lpos$x[cp]<-lpos$x[cp]+5
    points(lpos$x[cp], lpos$y[cp], pch = 16, col = "forestgreen")
    text(lpos$x[cp], lpos$y[cp], "G", col = "white", cex = 0.35)
    text(lpos$x[cp]+0.5,lpos$y[cp], font = 3, cex = 0.5, pos = 4, labels = "tree with girth tape")
    cp<-cp+1+longlabel
    longlabel=FALSE
  }
  #PDM
  if ("pointdm" %in% measurements){
    if (!lpos$x[cp]==0) if (longlabel)lpos$x[cp]<-lpos$x[cp]+5
    points(lpos$x[cp], lpos$y[cp], pch = 16, col = "lightseagreen")
    arrows(x0 =lpos$x[cp], y0 = lpos$y[cp], x1 = lpos$x[cp]-0.7, angle = 150, col = "red", length = 0.04, lwd = 1)
    text(lpos$x[cp]+0.5,lpos$y[cp], font = 3, cex = 0.5, pos = 4, labels = "tree with point dendrometer")
    cp<-cp+1+longlabel
    longlabel=FALSE
  }

  if ("sapflow" %in% measurements){
    if (!lpos$x[cp]==0) if (longlabel)lpos$x[cp]<-lpos$x[cp]+5
    points(lpos$x[cp], lpos$y[cp], pch = 16, col = "gold1")
    arrows(x0 = lpos$x[cp]+0.7, y0 = lpos$y[cp]-0.7,  y1= lpos$y[cp] +0.7, angle = 30, col = "red", length = 0.04, lwd = 1)
    text(lpos$x[cp]+0.5,lpos$y[cp], font = 3, cex = 0.5, pos = 4, labels = "tree with sapflow sensor")
    cp<-cp+1
    longlabel=FALSE
  }
  #Soil Moisture
  if ("soilmt" %in% measurements){
    if (!lpos$x[cp]==0) if (longlabel)lpos$x[cp]<-lpos$x[cp]+5
    points(lpos$x[cp], lpos$y[cp], pch = 16, col = "dodgerblue2")
    rasterImage(drop_icon, lpos$x[cp]+0.8, lpos$y[cp]+0.6, lpos$x[cp]+1.3, lpos$y[cp]+1.5)
    text(lpos$x[cp]+0.5,lpos$y[cp], font = 3, cex = 0.5, pos = 4, labels = "tree with soil moisture sensor")
    cp<-cp+1
    longlabel=FALSE
  }
  
  #suction plate
  if (GlobalMapOptions$suctionplates){
    if (!lpos$x[cp]==0) if (longlabel)lpos$x[cp]<-lpos$x[cp]+5
    points(lpos$x[cp], lpos$y[cp], cex = 0.7, pch = 19, col = "deepskyblue")
    points(lpos$x[cp], lpos$y[cp], cex = 0.8, pch = "*", col = "white")
    text(lpos$x[cp]+0.5,lpos$y[cp], font = 3, cex = 0.5, pos = 4, labels = "suction plate")
    cp<-cp+1
  }
  
  #rain traps
  if (GlobalMapOptions$suctionplates){
    if (!lpos$x[cp]==0) if (longlabel)lpos$x[cp]<-lpos$x[cp]+5
    segments(lpos$x[cp], lpos$y[cp], lpos$x[cp]+5, lpos$y[cp], lwd=2 ,col = "gray40")
    points(x = lpos$x[cp], y = lpos$y[cp], cex = 0.8, pch = 19, col = "gray40")
    points(x = lpos$x[cp], y = lpos$y[cp], cex = 0.8, pch = 1, col = "gray10")
    text(lpos$x[cp] + 4.5 , lpos$y[cp] , font = 3, cex = 0.5, pos = 4,  labels = "raintrap")
    cp<-cp+1
  }
  
  #Grid
  if (GlobalMapOptions$grid){
    if (!lpos$x[cp]==0) if (longlabel)lpos$x[cp]<-lpos$x[cp]+5
    lines(x = c(lpos$x[cp]-2,lpos$x[cp]+2), y = c(lpos$y[cp],lpos$y[cp]), col = "grey", lwd = 0.5)
    lines(x = c(lpos$x[cp],lpos$x[cp]), y = c(lpos$y[cp]-1,lpos$y[cp]+1), col = "grey", lwd = 0.5)
    text(lpos$x[cp]+0.5,lpos$y[cp], font = 3, cex = 0.5, pos = 4, labels =  "10m by 10m grid")
    text(lpos$x[cp]+0.5, lpos$y[cp]-1.5, font = 3, cex = 0.5, pos = 4, labels = "(marked with orange")
    text(lpos$x[cp]+0.5, lpos$y[cp]-3, font = 3, cex = 0.5, pos = 4, labels = "posts in the field)")
    #cp<-cp+1
  }
}

plot_craneroute<-function(type="broadleaf", arrow=FALSE){
  names(craneroute)[names(craneroute)=="tree_id"]<-"nr"
  routestops<-craneroute[craneroute$type==type,]
  # Color and line width
  routescol<- sprintf("%sAA",col2hex(routestops$color[1])) 
  routeslwd<-6
  route<-merge(routestops,metadata[,c("nr","defx","defy")],by="nr",all.x = TRUE)
  route$defx[route$nr=="crane"]=68
  route$defy[route$nr=="crane"]=74
  route<-route[order(route$order),]
  offset<-1 # distance from tree centerpoint
  if (arrow) offset=1.6
  for (i in 1:(nrow(route)-1)){
    dx<-route$defx[i+1]-route$defx[i]
    dy<- route$defy[i+1]-route$defy[i]
    alpha<-atan(dx/dy)
    ox<-sin(alpha)*offset
    oy<-cos(alpha)*offset
    if (arrow || i==1 || i== (nrow(route)-1) ){  
      if (dy>0) {arrows(route$defx[i]+ox,route$defy[i]+oy,route$defx[i+1]-ox,route$defy[i+1]-oy,lwd=routeslwd,col=routescol,length=0.1)}
      else {arrows(route$defx[i]-ox,route$defy[i]-oy,route$defx[i+1]+ox,route$defy[i+1]+oy,lwd=routeslwd,col=routescol,length=0.1)  }
    }else{
      if (dy>0) {segments(route$defx[i]+ox,route$defy[i]+oy,route$defx[i+1]-ox,route$defy[i+1]-oy,lwd=routeslwd,col=routescol)}
      else {segments(route$defx[i]-ox,route$defy[i]-oy,route$defx[i+1]+ox,route$defy[i+1]+oy,lwd=routeslwd,col=routescol)  }
    }
}
  return(route$nr[!route$nr=="crane"])
}


plot_title<-function(subtitle=NULL, year=""){
# Title and credits:
text(61, 147, "Site Map Swiss Canopy Crane II, H\u00F6lstein", family = "serif", font = 2)
if (!is.null(subtitle)) text(61, 144, subtitle, family = "serif", font = 1,cex=0.8)
text(124, 0, "(c) David Basler 2022-2024, based on work by Cedric Zahnd 2020 and Urs Max Weber 2017)", pos = 2, srt = -90, cex = 0.3)
text(124, 39, sprintf("Current map for %s created on %s ",year,Sys.Date()), pos = 2, srt = -90, cex = 0.3)
}


###############################################################################################################################
# DATA IMPORT
###############################################################################################################################
mapdatapath<- getwd()
#metadatapath<-"Z:/Professuren/PPE/Hölstein/Data Archive New/4_Metadata_trees" # Metatdata is now placed in the datasheet folder
data_import (mapdatapath,mapdatapath)


###############################################################################################################################
# Example Maps (Comment this section if script is sourced externally)
###############################################################################################################################

not_run<-function(){
  
  #source(plot_scciimap_srv.r) # if used in an external script, this file can be sourced
  
  #GLOBAL OPTIONS
  GlobalMapOptions<-list(
    title               ="Overview",
    presentation        =FALSE,
    year                =2024,
    # Basic plot options
    crane               =TRUE,
    grid                =TRUE,
    footpaths           =TRUE,
    road                =TRUE,
    gates               =TRUE,
    fence               =TRUE,
    containers          =TRUE,
    soilpits            =TRUE,
    roofs               =TRUE,
    roof_labels         =TRUE,
    raintraps           =TRUE,
    # Measurements
    sapflow_log         =TRUE,
    sapflow_cable       =TRUE,
    suctionplates       =FALSE,
    littertraps         =TRUE,
    # Campaign
    craneroute          = NA, # Choice from NA, "broadleaf", "conifer", "flash"
    craneroutearrows    = TRUE,
    # Trees
    trees=list(
      list(
        flt             ="all",#"species=='fs'", do not include [_year] in filter arguments
        speciescolor    =TRUE,
        customcolor     ="#7D7F7C",
        size_dbh        =TRUE,
        customsize      =1,
        numbers         =TRUE,
        alpha           =255,
        highlight_shape =NA, #circle, rect, underline
        highlight_color ="#FF0000", #Color Selector
        measurements=
          list(pointdm  =TRUE,
               banddm      =TRUE,
               soilmt      =TRUE,
               #leafwp      =TRUE,
               #nsc         =TRUE,
               #phenocam    =TRUE,
               sapflow     =TRUE
          )
      )
    )
  )

  dir.create("./maps/", showWarnings = FALSE)
  
  
  #****** BASIC MAP FOR PRINT *****************
  #*
  #docname <- paste("./maps/", as.character(Sys.Date()), "-Hölstein-Map.jpeg", sep = "")
  #jpeg(docname, width = 8.27, height = 11.69, units = "in", quality = 100, res = 1200)
  docname <- paste("./maps/", as.character(Sys.Date()), "-Hölstein-Map.pdf", sep = "")
  pdf(docname, width = 8.27, height = 11.69, units = "in")
  GlobalMapOptions$trees[[1]]$flt<-"status=='a'"
  plotmap(metadata,GlobalMapOptions)
  dev.off()
  
  #*
  #****** CAMPAIGN ROUTES *****************
  #*
  for (campaign in c("broadleaf","conifer","flash")){
    #docname <- paste("./maps/", as.character(Sys.Date()), "-Hölstein-Map_",campaign,".jpeg", sep = "")
    #jpeg(docname, width = 8.27, height = 11.69, units = "in", quality = 100, res = 1200)
    docname <- paste("./maps/", as.character(Sys.Date()), "-Hölstein-Map_",campaign,".pdf", sep = "")
    pdf(docname, width = 8.27, height = 11.69)
    GlobalMapOptions$trees[[1]]$flt<-"status=='a'"
    GlobalMapOptions$craneroute<-campaign
    plotmap(metadata,GlobalMapOptions)
    dev.off()
  }
  
  
  #*
  #******* Figures for presentation *****************
  #*#*
  pdf("SCCII_tree_presentation.pdf",width =  8, height = 8*(141/121)-0.18)
  par(cex.lab=1.4,cex.axis=1.4,cex=1)
  GlobalMapOptions$trees[[1]]$flt<-"status=='a'"
  plotmap(metadata,GlobalMapOptions)
  dev.off()
}