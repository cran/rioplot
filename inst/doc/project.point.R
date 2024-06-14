## -----------------------------------------------------------------------------
library(rioplot)
data(Kenworthy99)
m1 <- lm(scale(dv) ~ scale(gdp) + scale(pov) + scale(tran) -1,data=Kenworthy99)
# Create plot with just the fitted values
rp1 <- rio.plot(m1,include.int="no",exclude.vars=1:3,col.names = "PRED.POV") 
rp1$gg.obj

## -----------------------------------------------------------------------------
# Create object with coordinates for all variables
rp2 <- rio.plot(m1,include.int="no")
# pull out those coordinates and put them into a data.frame
pdat <- data.frame(vars=c("gdp","prepov","tran","yhat"),
                   x=rp2$col.dimensions[,1],y=rp2$col.dimensions[,2])
pdat
rp1$gg.obj + 
  scale_x_continuous(limits=c(-.6,1.15)) + 
  scale_y_continuous(limits=c(-.85,.5)) +
  geom_point(data=pdat[1:3,],aes(x=x,y=y)) +
  geom_text(data=pdat[1:3,],aes(x=x,y=y,label=vars),nudge_y=.1,nudge_x=.1)

## -----------------------------------------------------------------------------
project.point(pdat[1,2:3],pdat[4,2:3]) # gdp
# points for each predictor
a1<-project.point(pdat[1,2:3],pdat[4,2:3])$newpoint # gdp
a2<-project.point(pdat[2,2:3],pdat[4,2:3])$newpoint # prepov
a3<-project.point(pdat[3,2:3],pdat[4,2:3])$newpoint # tran
# compile those points into a data.frame
p1 <- data.frame(x=as.numeric(a1[1]),y=as.numeric(a1[2]),gr=1) 
# output from project.point
p2 <- data.frame(pdat[1,2:3],gr=1) # gdp's coordinates
p3 <- data.frame(x=as.numeric(a2[1]),y=as.numeric(a2[2]),gr=2) 
# output from project.point
p4 <- data.frame(pdat[2,2:3],gr=2) # prepov's coordinates
p5 <- data.frame(x=as.numeric(a3[1]),y=as.numeric(a3[2]),gr=3) 
# output from project.point
p6 <- data.frame(pdat[3,2:3],gr=3) # tran's coordinates
pdat2 <- rbind(p1,p2,p3,p4,p5,p6)
pdat2
# the groups denote the two points that define the beginning and 
# end of the line segments.  
rp1$gg.obj + 
  scale_x_continuous(limits=c(-.6,1.15)) + 
  scale_y_continuous(limits=c(-.85,.5)) +
  geom_line(data=pdat2,aes(x=x,y=y,group=gr),color="grey65") + 
  # line segments from project.point
  geom_point(data=pdat[1:3,],aes(x=x,y=y)) +
  geom_text(data=pdat[1:3,],aes(x=x,y=y,label=vars),nudge_y=.1,nudge_x=.1) 


