library(gRbase)

data(reinis)
reinis <- as(reinis,"gmData")

m1 <- new("hllm",~.. , reinis) 
fit(m1,engine="loglm")

m2 <- new("hllm",~A:D:F+B:D+B:G+A:E:F, reinis) 
dynamic.Graph(m2)

