data(HairEyeColor)
gm.hec <- as(HairEyeColor,"gmData")

varNames(gm.hec)
print(gm.hec)

m1i <- new("hllm", Hair+Eye+Sex ~ 1, gm.hec) 
m2i <- new("hllm", Hair:Eye+Sex ~ 1, gm.hec) 

m1f <- fit(m1i); class(m1f)
m2f <- fit(m2i,engine="loglin"); class(m2f)

m1i <- new("hllm", . ~ 1, gmData=gm.hec) 
m2i <- new("hllm", . ~ 1, gm.hec) 

m1s <- new("hllm", .. ~ 1, gmData=gm.hec) 
m2s <- new("hllm", .. ~ 1, gmData=gm.hec) 


### Test of several models
mod.list <- c(.~1, ..~1, ...~1, ....~1, .^1~1, .^2~1, .^3~1, .^4~1, Hair+Eye+Sex~1, Hair:Eye+Sex~1, Hair:Eye~1)

v1<- lapply(mod.list,
    function(m){cat("\nMODEL FORMULA GIVEN:", paste(m)[c(2,1,3)],"\n\n")
                mo<-new("hllm",m,gmData=gm.hec);
                mof<- fit(mo)
                print(mof)
                }
                )

v2<- lapply(mod.list,
    function(m){cat("\nMODEL FORMULA GIVEN:", paste(m)[c(2,1,3)],"\n\n")
                mo<-new("hllm",m,gmData=gm.hec,marginal=c("Hair","Eye"));
                mof<- fit(mo)
                print(mof)
                mof
                }
                )
