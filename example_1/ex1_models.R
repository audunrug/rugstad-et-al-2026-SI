library(gllvm)
library(tidyverse)
library(lattice)
library(gclus)
library(corrplot)
library(ggpubr)
library(readxl)
library(mefa)

# allow gllvm to run on all available CPU cores
TMB::openmp(parallel::detectCores()-1, autopar = TRUE, DLL = "gllvm")

## Loading data ####
species <- read_xlsx("example_1/data/Fernandez et al. 2021_JVS_Data.xlsx", 
                     sheet=1) |> 
  tibble::column_to_rownames("Species") |> # set species column as rownames
  t() |> as.data.frame() # transpose to site X species format

# load environemntal data
env <- read_xlsx("example_1/data/Fernandez et al. 2021_JVS_Data.xlsx", 
                 sheet=2)
colnames(env) <- c("plot","sample", "soil.organic.C", # clean column names
                   "soil.N","soil.C.N", "soil.moisture")

# fix typos is data
species[species>1000] <- species[species>1000] / 1000000
duplicates <- env[env$plot=="lig.60_C2",-c(1,2)]
env[42,-c(1,2)] <- t(as.matrix(colMeans(duplicates))) # replace first duplicate
env <- env[-44,] # remove second duplicate

# make both lowercase
rownames(species) <- stringr::str_to_lower(rownames(species))
env$plot <- stringr::str_to_lower(env$plot)

# remove all periods from names
rownames(species) <- lapply(rownames(species), FUN = gsub, 
                            pattern=".", replacement="", fixed=T)
env$plot <- gsub(env$plot, pattern=".", replacement="", fixed=T)

# scale
env[,-c(1,2)] <- scale(env[,-c(1,2)])

# filter by common sites
species_sub <- species |> filter(rownames(species) %in% env$plot)
env_sub <- env |> filter(env$plot %in% rownames(species_sub))

# remove singleton sites
species_f <- species[rowSums(species>0)>1,] 
species_sub <- species_sub[,colSums(species_sub>0)>1] 
env_sub <- env_sub[rowSums(species_sub>0)>1,]
species_sub <- species_sub[rowSums(species_sub>0)>1,]

## fitting final models for visualization (see S2 for full model selection) ####
# unconstrained ordination ###
we_1_uc_mod <- gllvm( 
  y = species_f,
  family = "tweedie", # response distribution family
  row.eff = "random", # to account for overall abundance in a plot
  num.lv = 3, # number of unconstrained LVs
  Power = NULL, # calculate power parameter for Tweedie family
  n.init = 10, # number of starting iterations (to ensure convergence)
  seed = 1, # starting seed (to ensure replicablilty)
  trace=T
)

# concurrent ordination ###
we_1_cc_mod <- gllvm(
  y = species_sub,
  X = env_sub,
  family = "tweedie",
  row.eff = "random",
  method = "LA", # Laplace approx. for better convergence
  lv.formula = ~ soil.organic.C + soil.N + # predictors for the ordination
    soil.C.N + soil.moisture, 
  num.lv.c = 2, # number of concurrent LVs
  trace = F,
  seed = 123,
  n.init = 5,
  Power = NULL
)