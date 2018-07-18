library("GA")

load("distance_matrix.rdata")
station_names <- rownames(distance_matrix)  

tourLength <- function(tour, distMatrix) {
  tour <- tour + 1
  tour <- c(1, tour, 1)
  route <- embed(tour, 2)[,2:1]
  sum(distMatrix[route]) 
}
tspFitness <- function(tour, ...) 10000/tourLength(tour, ...)

GA <- ga(
  type = "permutation", 
  fitness = tspFitness, 
  distMatrix = distance_matrix,
  lower = 1, 
  upper = length(station_names) - 1, 
  popSize = 200, 
  maxiter = 20000,
  run = 1000, 
  pmutation = 0.4,
  keepBest = T,
  parallel=F)
summary(GA)

save(GA, file="GA.rdata")
