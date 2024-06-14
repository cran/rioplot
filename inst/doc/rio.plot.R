## -----------------------------------------------------------------------------
library(rioplot)
data(Kenworthy99)
m1 <- lm(scale(dv) ~ scale(gdp) + scale(pov) + scale(tran) -1,data=Kenworthy99)
rp1 <- suppressMessages(rio.plot(m1,include.int="no"))
names(rp1)


## -----------------------------------------------------------------------------
rp1$gg.obj
library(ggplot2)
rp1$gg.obj + scale_x_continuous(limits=c(-.55,1.2))

## -----------------------------------------------------------------------------
rp2 <- rio.plot(m1,r1=1:15,case.names=paste(1:15),include.int="no")
rp2$gg.obj 

## -----------------------------------------------------------------------------
library(dplyr)
Kenworthy99 <- mutate(Kenworthy99,type=c("Liberal","Corp","Liberal",
                                         "SocDem","SocDem","Corp","Corp","Corp","Corp","Corp","SocDem","SocDem",
"Liberal","Liberal","Liberal"))
# Aggregate cases
rp3 <- rio.plot(m1,r1=1:15,group.cases=Kenworthy99$type,include.int="no")
rp3$gg.obj
rp3$gg.obj + scale_x_continuous(limits=c(-.7,1.3))

## -----------------------------------------------------------------------------
rp4 <- rio.plot(m1,r1=1:15,case.names=paste(1:15),include.int="no",
                col.names = paste(1:4))
rp4$gg.obj

## -----------------------------------------------------------------------------
suppressMessages(library(MASS))
data("Hilbe")
Hilbe[,2:ncol(Hilbe)]<-scale(Hilbe[,2:ncol(Hilbe)])

m2<-glm.nb(naffairs~avgmarr + hapavg + vryhap + smerel + vryrel + yrsmarr4 + 
             yrsmarr5 + yrsmarr6,data=Hilbe)
rp5 <- rio.plot(m2,model.type="nb",col.names =c("intercept",
                                                names(m2$model)[c(2:9,1)]))
rp5$gg.obj + scale_x_continuous(limits=c(-.7,.7))

## -----------------------------------------------------------------------------
data(Kenworthy99)
Kenworthy99 <- mutate(Kenworthy99,type=c("Liberal","Corp","Liberal",
                                         "SocDem","SocDem","Corp","Corp","Corp","Corp","Corp","SocDem",
                                         "SocDem","Liberal","Liberal","Liberal"))
m1 <- lm(scale(dv) ~ scale(gdp) + scale(pov) + scale(tran) -1,data=Kenworthy99)
rp6 <- rio.plot(m1,include.int="no",col.names = c("gdp","prepov","tran","POSTPOV"))
# no cases
rp6$gg.obj + 
  scale_x_continuous(limits=c(-2.45,3.25)) + 
  scale_y_continuous(limits=c(-2.2,1.6))
# create data to plot the cases
pdat <- data.frame(x=rp1$row.dimensions[,1],y=rp1$row.dimensions[,2],Kenworthy99)
rp6$gg.obj + 
  scale_x_continuous(limits=c(-2.45,3.25)) + 
  scale_y_continuous(limits=c(-2.2,1.6)) +
  geom_point(data=pdat,aes(x=x,y=y,shape=type,color=type)) + # add the cases
  theme(legend.position = "bottom") +
  labs(shape="")

## -----------------------------------------------------------------------------
data(Kenworthy99)
Kenworthy99 <- mutate(Kenworthy99,type=c("Liberal","Corp","Liberal",
                                         "SocDem","SocDem","Corp","Corp","Corp","Corp","Corp","SocDem",
                                         "SocDem","Liberal","Liberal","Liberal"))
rp7 <- rio.plot(m1,exclude.vars=1:3,include.int="no",col.names = "PRED.POV")
pdat1 <- data.frame(x=rp1$row.dimensions[,1],y=rp1$row.dimensions[,2],Kenworthy99)
# rp7 does not include dimensions for variables that are excluded from the plot
rp8 <- rio.plot(m1,include.int="no")
# can exclude row 4 since y-hat is already plotted
pdat2 <- data.frame(x=rp8$col.dimensions[1:3,1],
                    y=rp8$col.dimensions[1:3,2],
                    varnames=c("GDP","POV","TRAN"))

rp7$gg.obj + 
  scale_x_continuous(limits=c(-2.45,3.25)) + 
  scale_y_continuous(limits=c(-2.2,1.6)) +
  geom_point(data=pdat1,aes(x=x,y=y,shape=type,color=type)) + 
  theme(legend.position = "bottom") +
  labs(shape="") +
  geom_point(data=pdat2,aes(x=x,y=y)) +
  ggrepel::geom_text_repel(data=pdat1,aes(x=x,y=y,
                                          label=tolower(ISO3)),color="grey50") +
  ggrepel::geom_text_repel(data=pdat2,aes(x=x,y=y,label=varnames)) 
 

