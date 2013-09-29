# PCA with function prcomp
pca1 = prcomp(USArrests, scale. = TRUE)
pca1$sdev       # sqrt of eigenvalues
pca1$rotation   # loadings
pca1$x          # PCs (aka scores)

# PCA with function princomp
pca2 = princomp(USArrests, cor=TRUE)
pca2$sdev        # sqrt of eigenvalues
pca2$loadings    # loadings
pca2$scores      # PCs (aka scores)

# PCA with function PCA
require(FactoMineR)
pca3 = PCA(USArrests, graph=FALSE)
pca3$eig         # matrix with eigenvalues
pca3$var$coord   # correlations between variables and PCs
pca3$ind$coord   # PCs (aka scores)

# PCA with function dudi.pca
require(ade4)
pca4 = dudi.pca(USArrests, nf=5, scannf=FALSE)
pca4$eig        # eigenvalues
pca4$c1         # loadings
pca4$co         # correlations between variables and PCs
pca4$li         # PCs

# PCA with function acp
require(amap)
pca5 = acp(USArrests)
pca5$sdev       # sqrt of eigenvalues
pca5$loadings   # loadings
pca5$scores     # scores

# load ggplot2
require(ggplot2)

# create data frame with scores
scores = as.data.frame(pca1$x)

# plot of observations
ggplot(data=scores, aes(x=PC1, y=PC2, label=rownames(scores))) +
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  geom_text(colour="tomato", alpha=0.8, size=4) +
  opts(title="PCA plot of USA States - Crime Rates")

# function to create a circle
circle <- function(center=c(0,0), npoints=100)
{
  r = 1
  tt = seq(0, 2*pi, length=npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
corcir = circle(c(0,0), npoints = 100)

# create data frame with correlations between variables and PCs
correlations = as.data.frame(cor(USArrests, pca1$x))

# data frame with arrows coordinates
arrows = data.frame(x1=c(0,0,0,0), y1=c(0,0,0,0),
                    x2=correlations$PC1, y2=correlations$PC2)

# geom_path will do open circles
ggplot() +
  geom_path(data=corcir, aes(x=x, y=y), colour="gray65") +
  geom_segment(data=arrows, aes(x=x1, y=y1, xend=x2, yend=y2), colour="gray65") +
  geom_text(data=correlations, aes(x=PC1, y=PC2, label=rownames(correlations))) +
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  xlim(-1.1,1.1) + ylim(-1.1,1.1) +
  labs(x="pc1 aixs", y="pc2 axis") +
  opts(title="Circle of correlations")