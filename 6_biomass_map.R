require(rgdal)
require(raster)
require(mapplots)
require(ggplot2)
require(RColorBrewer)
require(maptools)
require(sp)
require(gstat)
require(maps)
require(mapproj)
require(GISTools)
require(bayou)
require(BiocManager)
require(tofsims)
## load shapefile created with "preparation_biomass_map_plots.R"

setwd("E:/6_clean_version_new/6_clean_version/outputs/")

my_result <- readOGR(".", "maps_6000_plot_100")

my_result
#### Plot Nr. 1
#### 
bla <- makeTransparent("white", alpha=0)

# simple plot result
setwd("E:/6_clean_version_new/6_clean_version/outputs/")

pdf("output.pdf", width=9, height=8)
par(mfrow=c(1,2), mar=c(4,4,2,4))
bla<-makeTransparent("white", alpha=0)

baseplot1 <- my_result
baseplot1$Col <- cut(baseplot1@data$bio_mean, seq(0, 300, by = 25), na.rm=TRUE)

baseplot2 <- my_result
baseplot2$Col <- cut(baseplot2@data$bio_sd, seq(0, 30, by = 5), na.rm=TRUE)

# define color scales

colr <-colorRampPalette(c("gray90", "blue","green"))(12) 
colr2 <-colorRampPalette(c("black", "gray90"))(12) 

# RF plots


baseplot1 <- my_result
baseplot1$Col <- cut(baseplot1@data$bio_mean, seq(0, 300, by = 25), na.rm=TRUE)
plot(baseplot1, col=colr[baseplot1$Col], border=bla)
north.arrow(xb=540650,yb=3565800,len=80,lab="North")
map.scale(xc=541790,yc=3565800,len=1000,units="km", ndivs=1, scol = "black", sfcol ="black")
title(main="Mean biomass [t/ha]")
legend.col(col = colr, lev = baseplot1$bio_mean)
box()

baseplot <- my_result
baseplot$Col <- cut(baseplot@data$bio_sd, seq(0, 30, by = 5))

plot(baseplot, col=colr2[baseplot$Col], border=bla)
north.arrow(xb=540650,yb=3565800,len=80,lab="North")
map.scale(xc=541790,yc=3565800,len=1000,units="km", ndivs=1, scol = "black", sfcol ="black")
title(main="SD of biomass estimates" )
legend.col(col = colr2, lev = baseplot2$bio_sd)
box()


dev.off()





### define function for color scale


legend.col <- function(col, lev){
  
  opar <- par
  
  n <- length(col)
  
  bx <- par("usr")
  
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  
  xx <- rep(box.cx, each = 2)
  
  par(xpd = TRUE)
  for(i in 1:n){
    
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
    
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(lev), max(lev), na.rm=TRUE),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = .25)
  par <- opar
}


makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

