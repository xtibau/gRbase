library(gRbase)

data(reinis)
reinis <- as(reinis,"gmData")

m1 <- new("hllm",~.. , reinis) 
fit(m1,engine="loglm")

m2 <- new("hllm",~smoke*phys*protein+mental*phys+mental*family+smoke*systol*protein, reinis) 
dynamic.Graph(m2)

