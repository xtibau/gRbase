data(HairEyeColor)
gm.hec <- as(HairEyeColor,"gmData")

varNames(gm.hec)
print(gm.hec)

m1i <- new("hllm", Hair+Eye+Sex ~ 1, gm.hec) 
m2i <- new("hllm", Hair:Eye+Sex ~ 1, gm.hec) 

m1f <- fit(m1i); class(m1f)
m2f <- fit(m2i,engine="loglin"); class(m2f)

m1i <- new("hllm", ~. , gmData=gm.hec) 
m2i <- new("hllm", ~., gm.hec) 

m1s <- new("hllm", ~.. , gmData=gm.hec) 
m2s <- new("hllm", ~.. , gmData=gm.hec) 


### Test of several models
mod.list <- c(~., ~.., ~..., ~...., ~.^1, ~.^2, ~.^3, ~.^4, ~Hair+Eye+Sex, ~Hair:Eye+Sex, ~Hair:Eye)

v1<- lapply(mod.list,
    function(m){cat("\nMODEL FORMULA GIVEN:", paste(m),"\n\n")
                mo<-new("hllm",m,gmData=gm.hec);
                mof<- fit(mo)
                print(mof)
                }
                )

v2<- lapply(mod.list,
    function(m){cat("\nMODEL FORMULA GIVEN:", paste(m),"\n\n")
                mo<-new("hllm",m,gmData=gm.hec,marginal=c("Hair","Eye"));
                mof<- fit(mo)
                print(mof)
                mof
                }
                )
