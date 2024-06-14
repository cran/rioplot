## -----------------------------------------------------------------------------
library(rioplot)
data(Kenworthy99)
m1 <- lm(scale(dv) ~ scale(gdp) + scale(pov) + scale(tran) -1,data=Kenworthy99)
rp1 <- rio.plot(m1,include.int="no",r1=1:15)

cosine(rp1$row.dimensions[15,],rp1$row.dimensions[8,]) 
# cosine similarity between USA and Ireland

cosine(rp1$row.dimensions[15,],rp1$row.dimensions[14,]) 
# cosine similarity between USA and United Kingdom

