
data(ksl,package="deal")
ksl.disc <- ksl[,5:7]
ksl.gmData <- as(ksl.disc,"gmData")

ksl.main <- new("hllm",~.^1,ksl.gmData)

fit(ksl.main)
fit(ksl.main,engine="loglin")
dynamic.Graph(ksl.main)
