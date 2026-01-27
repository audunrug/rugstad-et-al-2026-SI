library(RColorBrewer)
library(tidyverse)
library(ggdist)
library(patchwork)
library(ggpubr)
library(ggthemes)
library(gllvm)

source("example_2/ex2_model.R") # need to run models and save as object first

## Plot 1 ####
## env loadings
lv_1RR <- getLV.gllvm(we_2_cn_mod, scale = T)
smry<-summary(we_2_cn_mod, rotate = F)
coefs <- smry$Coef.tableLV

## scaled species and site loadings
# code from the gllvm "ordiplot" function
choose.lv.coefs <- getLoadings.gllvm(we_2_cn_mod)
choose.lvs <- getLV.gllvm(we_2_cn_mod)
bothnorms <- vector("numeric",ncol(choose.lv.coefs))
for(i in 1:ncol(choose.lv.coefs)){
  bothnorms[i] <- sqrt(sum(choose.lvs[,i]^2)) * 
    sqrt(sum(choose.lv.coefs[,i]^2))
}

## Standardize both to unit norm then scale using bothnorms
scaled_cw_sites <- t(t(choose.lvs) / sqrt(colSums(choose.lvs^2)) * 
                       (bothnorms^0.5)) 
scaled_cw_species <- choose.lv.coefs
for(i in 1:ncol(scaled_cw_species)){
  scaled_cw_species[,i] <- choose.lv.coefs[,i] / 
    sqrt(sum(choose.lv.coefs[,i]^2)) * (bothnorms[i]^(0.5)) 
}

# Standardize species coefs too
coef_scaled <- (coefs[,1] / sqrt(colSums(choose.lvs^2))) * (bothnorms^(0.5)) 

# extract species loadings for species with highest and lowest loadings
load_scaled_min <- scaled_cw_species[sort(scaled_cw_species[,1], 
                                          decreasing = T, 
                                          index.return=T)[[2]],] |>  
  tail(6) |> 
  head(5) # avoid a. sylvestris as it is a huge outlier
load_scaled_max <- scaled_cw_species[sort(scaled_cw_species[,1], 
                                          index.return=T)[[2]],] |> 
  tail(5)
load_scaled_ex <- c(load_scaled_min,load_scaled_max) # loadings to plot

# add together (names from inspecting the data)
arrow_names <- c(rev(c("Grain size", "Loss on ignition",
                       "Slope",  "Canopy cover", "Dist. intact vegetation",
                       "Seeded X time","Planted-natural X time", "Natural X time",
                       "Seeded", "Planted-natural", "Natural")),
                 c("F. rubra", "P. pratense", "C. canenscens", 
                   "C. neglecta", "R. acetosa", "O. acetosella",
                   "Lichens", "Q. robur", "V. myrtillus", "M. bifolium"))

# y positions of loading arows and colors
y_pos <- c(seq(0.15,0.45, length.out=length(coefs[,1])), 
           seq(-.15,-0.45, length.out=length(load_scaled_ex)))
colrs <- c(ifelse(coefs[,4]<0.05, "red", "lightsalmon"),
           rep("steelblue", length(load_scaled_ex))) # color = significance
x_pos <- c(coef_scaled, load_scaled_ex) # x pos of arrows

## make ggplot
a <- ggplot() + 
  geom_point(aes(x=scaled_cw_sites[,1], y=0, color=env$method), 
             position = position_jitter(w = 0, h = 0.1)) +
  geom_segment(aes(x=0, y=y_pos, xend=x_pos, yend=y_pos), 
               alpha = 0.6,
               color=colrs, 
               arrow = arrow(type="open", length=unit(0.10, "inches"))) +
  geom_text(aes(x=(x_pos + ((x_pos/abs(x_pos)))*0.20), y=y_pos, 
                label=arrow_names), size=2.5, alpha=1, color=colrs) +
  theme_classic() +
  theme(axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line(),
        legend.position = "bottom") +
  labs(y="", x="Latent variable", color="Method", title="a") +
  #xlim(-7,1.8) +
  scale_color_brewer(labels = c("Natural", "Planted-natural", 
                                "Reference","Seeded"), 
                     type="qual", palette = "Set1") +
  ylim(-.45,.45)


## Plot 2 ##### extract site and random effects
site_effects <- we_2_cn_mod$params$row.params.random
region_effects <- we_2_cn_mod$params$B
region_species_effects <- as.data.frame(we_2_cn_mod$params$Br)

# add intercepts to species random effects
region_species_effects[2,] <- region_species_effects[2,] + region_effects[1]
region_species_effects[3,] <- region_species_effects[3,] + region_effects[2]
region_species_effects$region <- rownames(region_species_effects)

# pivot for ggplot
region_species_effects <- region_species_effects |> 
  pivot_longer(cols=1:98)

# plot
b <- ggplot() +
  geom_boxplot(aes(x="Site R.E.", y=site_effects, color="sites")) +
  geom_jitter(aes(x="Site R.E.", y=site_effects, color="sites"), 
              position=position_jitter(0.15)) +
  geom_boxplot(aes(x="Species R.E. (+ intercept)", 
                   y=region_species_effects$value, 
                   color=region_species_effects$region),
               outlier.shape = NA) +
  geom_jitter(aes(x="Species R.E. (+ intercept)", 
                  y=region_species_effects$value, 
                  color=region_species_effects$region),
              alpha=0.7, width=0.2) +
  labs(color="Random effect",x="", y="value", title="b") +
  theme_classic() +
  scale_color_brewer(palette="Set2",
                     labels = c("Region 1", "Region 2", "Region 3", "Sites"))



#### prediction ####
env_pred <- X
add_t <- 20/(sqrt(var(env$years_since_n[env$years_since_n<40])))
env_pred$natXtime <- ifelse(X$natXtime>0, X$natXtime+add_t, 0)
env_pred$pnXtime <- ifelse(X$pnXtime>0, X$pnXtime+add_t, 0) 
env_pred$seedXtime <- ifelse(X$seedXtime>0, X$seedXtime+add_t, 0)
env_pred$gf <- env$gf # needed for prediction

lv_scores <- scaled_cw_sites
pars <- coef_scaled

lv_scores[X$method=="nat",] = lv_scores[X$method=="nat",] + pars[4]*add_t
lv_scores[X$method=="pn",] = lv_scores[X$method=="pn",] + pars[5]*add_t
lv_scores[X$method=="seed",] = lv_scores[X$method=="seed",] + pars[6]*add_t

X$gf <- env_pred$gf # needed for prediction of baseline
pred_scores_0 <- predict.gllvm(newX=X, object = we_2_cn_mod, 
                               type = "response")
pred_scores_20 <- predict.gllvm(newX=env_pred, object = we_2_cn_mod, 
                                type = "response")
diffs <- as.data.frame(pred_scores_20-pred_scores_0) # calculate diff
diffs$method <- env_pred$method
diffs <- diffs |> filter(method != "ref") # remove reference sites

#### Plot 3 ####
lv_scores <- scaled_cw_sites
pars <- coef_scaled

lv_scores[env_mat$method=="nat",] = lv_scores[env_mat$method=="nat",] + pars[4]*add_t
lv_scores[env_mat$method=="pn",] = lv_scores[env_mat$method=="pn",] + pars[5]*add_t
lv_scores[env_mat$method=="seed",] = lv_scores[env_mat$method=="seed",] + pars[6]*add_t

# predict LV scores plot
c <- ggplot() + 
  geom_point(aes(x=lv_scores[,1], y=0, color=X$method), 
             position = position_jitter(w = 0, h = 0.1)) +
  theme_classic() +
  theme(axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line(),
        legend.position = "bottom") +
  labs(y="", x="Latent variable", color="Method", title = "c") +
  scale_color_brewer(labels = c("Natural", "Planted-natural", 
                                "Reference","Seeded"), 
                     type="qual", palette = "Set1") +
  ylim(-.2,.2)


#### plot 4 ####
p1 <- ggplot() +
  geom_density(aes(x=diffs$festuca_rubra, fill = diffs$method, 
                   color=diffs$method), alpha=0.5) +
  labs(x="predicted cover change", y="number of plots", 
       title = "Festuca rubra", fill="Method", color="Method") +
  scale_fill_manual(labels = c("Natural", "Planted-natural", "Seeded"), 
                    values = c("#E41A1C", "#377EB8", "#984EA3")) +
  scale_color_manual(labels=c("Natural", "Planted-natural","Seeded"), 
                     values = c("#E41A1C", "#377EB8", "#984EA3")) +
  theme_classic()

p2 <- ggplot() +
  geom_density(aes(x=diffs$picea_abies, fill = diffs$method, 
                   color=diffs$method), alpha=0.5) +
  labs(x="predicted cover change", y="", title = "Picea abies", 
       fill="Method", color="Method") +
  scale_fill_manual(labels = c("Natural", "Planted-natural", "Seeded"), 
                    values = c("#E41A1C", "#377EB8", "#984EA3")) +
  scale_color_manual(labels=c("Natural", "Planted-natural", "Seeded"), 
                     values = c("#E41A1C", "#377EB8", "#984EA3")) +
  theme_classic()

d <- ggarrange(p1,p2, common.legend = T, legend = "bottom")
d <- annotate_figure(d, top = text_grob("d", size = 14, hjust=18), 
                     fig.lab.pos = "top.left")

full_figure <- ggarrange(a,b,c,d)

ggsave("figures/figure_5.pdf", width=12, height=9, units = "in", device="pdf")