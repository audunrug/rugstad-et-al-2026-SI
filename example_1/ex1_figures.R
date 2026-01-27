# source model script
source("example_1/ex1_models.R")

## Set up main Figure ####
pdf("figures/figure_4.pdf", width = 12, height=9,
    colormodel = "cmyk")
par(mfrow=c(2,2), adj=0.5) # set up plotting window

### Unconstrained ordination diagram ####
col_list <- c(rep("darkblue", 8), "red", rep(rep("darkblue", 11)))
col_list_2 <- c(rep("blue", 8), "red", rep(rep("blue", 11)))
a <- ordiplot(we_1_uc_mod, biplot=T, symbols=T, predict.region = "species",
              spp.colors =  col_list,
              col.ellips = col_list_2, main="", ylim=c(-2,3), xlim=c(-5,3))
title(main = "a", adj = 0)

### correlation plot ####
cr0 <- getResidualCor(we_1_uc_mod)
b <- corrplot(cr0[order.single(cr0), order.single(cr0)], diag = TRUE, 
              type = "lower", method = "square", tl.cex = 0.8, tl.srt = 45, 
              tl.col = c("darkgrey","red", rep("darkgrey", 18)))
title(main = "b", adj = 0)

### Concurrent ordination diagram ####
col_list <- c(rep("darkblue", 5), "red", rep(rep("darkblue", 11)))
we_1_cc_mod$params$theta
a <- ordiplot(we_1_cc_mod, biplot=T, symbols=T, rotate=T,
              spp.colors =  col_list, main="", ylim=c(-2,3), xlim=c(-5,3))
title(main = "c", adj = 0)

### Soil moisture coefficient plot ####
col_list <- c(rep("black", 5), "red", rep(rep("black", 11)))
coefplot.gllvm(we_1_cc_mod, which.Xcoef = 4, order = F, cex.ylab = 0.001)
#axis(2, col.axis="white")
Map(axis, side=2, at=1:length(col_list), cex.lab =0.1, col.axis=col_list, 
    labels=rownames(we_1_cc_mod$params$theta), lwd=0, las=1)
axis(2,at=1:3,labels=FALSE)
title(main = "d", adj = 0)

## Save figure ####
dev.off()
