library(gRbase)

data(Reinis,package="CoCo")

#Reinis.obs <- attr(Reinis,".observations")$counts
#n <- length(Reinis.obs)
#A <- gl(2,1,n)
#B <- gl(2,2,n)
#C <- gl(2,4,n)
#D <- gl(2,8,n)
#E <- gl(2,16,n)
#FF<- gl(2,32,n)

#Reinis.tab <- xtabs(Reinis.obs~A+B+C+D+E+FF)



#rei <- as.gmData(Reinis.tab);
rei <- as(as.table(Reinis),"gmData")
#rei <- as.gmData(as.table(Reinis))

#m1 <- hllm( .^10 ~ 1,  rei, engine="loglm")


dynamic.Graph(rei)

## factor.Graph(newhllm(m1))
