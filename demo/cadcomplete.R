library(gRbase)

data(cadcomplete)
cadcomplete <- data.frame(lapply(cadcomplete,as.factor))
cad <- as(cadcomplete,"gmData")

m1 <- new("hllm",~.^.,cad,marginal=names(cadcomplete)[1:6])

#m1s <- hllm( ~Sex*STcode*AngPec + QWavecode*QWave + AMI*Sex*AngPec,  cad, marginal=names(cadcomplete)[1:6], engine="loglm")

dynamic.Graph(m1,visibleVertices=1:6)

##factor.Graph(newhllm(m1s),visibleVertices=1:6)
