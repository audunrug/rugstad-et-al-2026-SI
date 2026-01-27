library(gllvm)
library(tidyverse)
library(vegan)
TMB::openmp(parallel::detectCores()-1, autopar = TRUE, DLL = "gllvm")
set.seed(1)

#### pre-processing
# load species data
load("example_2/data/species.rda")
species <- species_sub1 # overwrite old species data frame

# load environemntal data
load("example_2/data/environmental_variables.rda")

# formatting
env <- env.var_sub1
for (s in unique(env.var_sub1$site)) {
  sb <- env.var_sub1[env.var_sub1$site==s,]
  env[env$site==s,]$gf <- sb[1,]$gf
}
env$gf <- droplevels(env$gf, "ref")
env$grain_size_stand_f <- as.numeric(env$grain_size_stand_f)

## Data filtering ####
Y <- species[,colSums(species > 0)>3]

# environmental data
env_sub <- env |> 
  select(dist_int_veg, caco, slope, loi, grain_size_stand_f) |> 
  scale() |> as.data.frame()
env_sub$method <- env$method
env_sub$time <- env$years_since_n # rename the column to be shorter
env_sub$time <- (env_sub$time - mean(env_sub$time[env_sub$time<40]))/
  (sqrt(var(env_sub$time[env_sub$time<40])))

# model matrix
X <- model.matrix(~ method/time + dist_int_veg + caco + 
                    slope + loi + grain_size_stand_f, 
                  data = env_sub)

X <- as.data.frame(X[,-c(1,2,3,4,10)]) # remove time x reference interaction

colnames(X)[6:8] <- c("natXtime", "pnXtime", "seedXtime")
X$method <- factor(env_sub$method, levels=c("ref", "nat", "pn", "seed"))
X$gf <- env_sub$gf

Y <- Y/100
Y <- Y[,names(sort(colSums(Y), decreasing =T))] # sort by most common species first

#### fitting final model for visualization (see S2 for full model selection) ####
we_2_cn_mod <- gllvm(
  y = Y,
  X = X,
  studyDesign = env, # study design matrix (for row.effects)
  num.RR = 1, # constrained latent variables
  family = "orderedBeta", # ordered beta response distribution
  formula = ~ (1|gf), # region as species-specific random effect
  row.eff = ~ (1|site), # site as community-wide random effect
  method = "EVA", # extended variational approximation method
  lv.formula = ~ method + natXtime + pnXtime + # predictors
    seedXtime + dist_int_veg + caco + slope + loi + grain_size_stand_f,
  disp.formula = rep(1, ncol(Y)), # same dispersion param for all species
  n.init = 2, # number of starting iterations (fewer due to slow fitting)
  seed = 1 # seed
)