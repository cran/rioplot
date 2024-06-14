## -----------------------------------------------------------------------------
library(rioplot)
data(Kenworthy99)
m1 <- lm(scale(dv) ~ scale(gdp) + scale(pov) + scale(tran) -1,data=Kenworthy99)
decompose.model(m1,group.by = c(rep(1,5),rep(2,5),rep(3,5)),include.int = "no")

## -----------------------------------------------------------------------------
data("Hilbe")
Hilbe <- data.frame(Hilbe,binAffairs=ifelse(Hilbe$naffairs>0,1,0)) 
m2<-glm(binAffairs ~ avgmarr + hapavg + vryhap + smerel + vryrel + yrsmarr4 + 
          yrsmarr5 + yrsmarr6,data=Hilbe, family=binomial())
decompose.model(m2,group.by = c(rep(1,201),rep(2,200),rep(3,200)),
                model.type = "logit")

## -----------------------------------------------------------------------------
m3<-glm(naffairs~avgmarr + hapavg + vryhap + smerel + vryrel + yrsmarr4 + 
          yrsmarr5 + yrsmarr6,data=Hilbe,family=poisson(link="log"))
decompose.model(m3,group.by = c(rep(1,201),rep(2,200),rep(3,200)),
                model.type="poisson")

## -----------------------------------------------------------------------------
library(MASS)
m4<-glm.nb(naffairs~avgmarr + hapavg + vryhap + smerel + vryrel + yrsmarr4 + 
             yrsmarr5 + yrsmarr6,data=Hilbe)
decompose.model(m4,group.by = c(rep(1,201),rep(2,200),rep(3,200)),model.type="nb")

