# Calculate area estimates and confidence interval from map and reference data from stratified random sampling
# Create a plot for comparison

library(survey) # calculate means and confidence intervals
library(reshape) # reshaping data for plotting
library(ggplot2) # plotting
theme_set(theme_bw()) # white background for ggplot2 graphs

# create a data frame with map and reference data from table 8 in Olofsson et al.
df <- data.frame(cbind("map"=c(rep("F-NF", times=66), "F-F", "NF-NF", "NF-NF", 
                                 rep("NF-F", times=55), "NF-NF", 
                                 rep("F-NF", times=5), rep("NF-F", times=8), rep("F-F", times=153), rep("NF-NF", times=9), 
                                 rep("F-NF", times=4), rep("NF-F", times=12), rep("F-F", times=11), rep("NF-NF", times=313)), 
                         "ref"=c(rep("F-NF", times=69), rep("NF-F", times=56), rep("F-F", times=175), rep("NF-NF", times=340) )))

# create a data frame with the map area per stratum
maparea <- data.frame("maparea"=c(200000, 150000, 3200000, 6450000)*0.09, "strat"=c("F-NF", "NF-F", "F-F", "NF-NF"))
mdf <- merge(df, maparea, by.x="map", by.y="strat")

# specify the survey design (stratified by the mapclasses)
surveymean <- function(x) {dstrat <- svydesign(id=~1, strata=~maparea, data=x) 
                           ce <- as.data.frame(svymean(~ref, dstrat))
                           return(ce)  
}

# calculate the survey mean and standard errors
ce <- by(mdf[, 2:3], mdf$map, surveymean)
cetable <- as.data.frame(t(sapply(ce, unlist)))
colnames(cetable) <- c("F-F_mean", "F-NF_mean", "NF-F_mean", "NF-NF_mean", "F-F_SE", "F-NF_SE", "NF-F_SE", "NF-NF_SE")
cetable$strat <- rownames(cetable)
cet <- merge(cetable, maparea, by="strat")
cetm <- cet[, c("F-NF_mean", "NF-F_mean", "F-F_mean", "NF-NF_mean", "F-NF_SE", "NF-F_SE", "F-F_SE", "NF-NF_SE")] * cet$maparea
cesum <- colSums(cetm)

ceov <- data.frame("X"=rownames(cetable), "CE"=cesum[1:4], "CE_CI" = cesum[5:8]*1.96)

comp <- data.frame("X"=maparea$strat, "map_original"=maparea$maparea, 
                   "map_adjusted"=c(21158, 11686, 285770, 581386), # if the adjusted map area estimates are not available, these need to be calculated first
                   "map_CI"=c(6158, 3756, 15510, 16282), 
                   "reference"=cesum[1:4], 
                   "reference_CI" = cesum[5:8]*1.96)

# reshape the data for use in ggplot2
plotdf <- melt(comp[, !(colnames(comp) %in% c("map_CI", "reference_CI"))], id.vars="X")
plotdf$CIs <- c(NA, NA, NA, NA, comp$map_CI, comp$reference_CI)

plotdf$X <- factor(plotdf$X, levels=c("F-F", "NF-NF", "F-NF", "NF-F"), 
                   labels=c("stable forest", "stable nonforest", "forest loss", "forest gain"))
plotdf$variable <- factor(plotdf$variable, levels=c("map_original", "map_adjusted", "reference"), 
                          labels=c("map", "adjusted", "reference"))

plotdf$value <- plotdf$value / 1000
plotdf$CIs <- plotdf$CIs / 1000
plotdf$stable <- ifelse(plotdf$X == "forest loss", "unstable", ifelse(plotdf$X == "forest gain", "unstable", "stable"))

# create the plots
pl_stable <- ggplot(plotdf[plotdf$stable == "stable",], aes(x=variable, y=value)) + 
  facet_grid(. ~ X) + 
  geom_point() +
  geom_errorbar(aes(ymin=value - CIs, ymax=value + CIs), width=.1) +
  geom_point(size=5) +
  ylab("Adjusted Area Estimates [1000 ha]") +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(size=16, angle=90),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        strip.text.x = element_text(size=16)
  )

pl_unstable <- ggplot(plotdf[plotdf$stable == "unstable",], aes(x=variable, y=value)) + 
  facet_grid(. ~ X) + 
  geom_point() +  
  geom_errorbar(aes(ymin=value - CIs, ymax=value + CIs), width=.1) +
  geom_point(size=5) +
  ylab("Adjusted Area Estimates [1000 ha]") +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(size=16, angle=90),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        strip.text.x = element_text(size=16)
  )

pl_unstable
pl_stable
