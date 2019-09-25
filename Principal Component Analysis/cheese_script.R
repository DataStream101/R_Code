# Loading Necessary Libraries
library(factoextra)
library(NbClust)
library(FactoMineR)

# Import data
cheese <- read.table("/..../cheese.txt",header = T, dec =".")

View(cheese)

dim(cheese)
summary(cheese)

# Data Preprocessing
new_rownames <- as.matrix(cheese[1])

rownames(cheese) <- new_rownames

cheese[1] <- NULL

cheese_st <- scale(cheese)

View(cheese_st)


## Compute PCA
# Using factorminer()
res.pca.factor <- PCA(cheese, graph = F)

# Visualize eigenvalues (the percentage of variances explained by each principal component).
fviz_eig(res.pca.factor, addlabels = T, main = "Dimensions' contribution") + theme_gray()

get_eig(res.pca.factor)

# Plot individual graph
x11()			# to visualize graphs in separated windows
fviz_pca_ind(res.pca.factor, axes=c(1,2), repel = T) + theme_grey()

# Plot variable graph
x11()
fviz_pca_var(res.pca.factor,axes=c(1,2), repel = T) + theme_gray()

# Interpreting results 
get_pca(res.pca.factor, "var")

#-----correlation variables/dimensions
res.pca.factor$var$cor

dimdesc(res.pca.factor) # useful FactorMineR function to interpret dimensions

#-----contribution of variables to the construction of the dimensions
fviz_contrib(res.pca.factor, choice = "var", axes =1)
fviz_contrib(res.pca.factor, choice = "var", axes =2)

#-----coordinates individuals/dimensions

res.pca.factor$ind$coord

#-----contribution of individuals to the construction of the dimensions
fviz_contrib(res.pca.factor, choice = "ind", axes =1)
fviz_contrib(res.pca.factor, choice = "ind", axes =2)




