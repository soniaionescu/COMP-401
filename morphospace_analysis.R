continuous_kernelDensity <- function(pca_object){
    require(MASS)
    pca <- pca_object[[1]]
    xValues <-pca$x[, "PC1"]
    yValues <- pca$x[,"PC2"]
    kernelDensity <- kde2d(xValues, yValues, n=100, lims = c(-0.30, 0.30, -0.25, 0.20))
    filled.contour(kernelDensity, color.palette = 
                       colorRampPalette(c('white','blue','yellow','red','darkred'))) #gives colour and index
    title(main="Kernel density graph of morphospace", xlab="PC1", ylab="PC2")
}
contour_kernelDensity <- function(pca_object){
    pca <- pca_object[[1]]
    numberSpecies <- nrow(pca$x)
    xValues <-pca$x[, "PC1"]
    yValues <- pca$x[,"PC2"]
    df <- data.frame(x=xValues,y=yValues,species=factor(pca_object[[2]]))
    commonTheme = list(labs(color="Density",fill="Density",
                            x="PC1", y="PC2"),
                       ggtitle(label="Contour Kernel Density of Morphospace"),
                       theme_bw(),
                       theme(legend.position = "right",
                             legend.justification = c(0,1)))
    #label of the data point !!!!!Refer to 'label for r' 
    numbers =  1:numberSpecies # because there are 56 coordinates (28 x, 28 y)
    ggplot(df,aes(x=x,y=y)) + 
        geom_point(show.legend=TRUE, aes(shape = species))+
        geom_density2d(aes(colour=..level..))+ 
        scale_shape_manual(values=1:nlevels(df$species))+
        scale_colour_gradient(low="blue",high="red")+
        guides(alpha="none")+
        commonTheme
}





mountain_graph <- function(pca_object){
    require(MASS)
    pca <- pca_object[[1]]
    xValues <-pca$x[, "PC1"]
    yValues <- pca$x[,"PC2"]
    kernelDensity <- kde2d(xValues, yValues, n=100, lims = c(-0.30, 0.30, -0.25, 0.20))
    persp(kernelDensity, phi= 5, theta = -30, d=10,
          ticktype="detailed", zlab='Density',
          ylab='PC2',xlab='PC1',main='Kernel density of morphospace') #phi=up down,theta=left right, d=dimension
}


Voronoi_polygon <- function(pca_object){
    pca <- pca_object[[1]]
    require(deldir)
    xValues <-pca$x[, "PC1"]
    yValues <- pca$x[,"PC2"]
    df <- data.frame(x=xValues,y=yValues)
    graphV <- deldir(xValues, yValues)
    graphTiles <- tile.list(graphV)
    numberSpecies <- nrow(pca$x)
    graphColors <- topo.colors(numberSpecies)
    plot(graphTiles, fillcol=graphColors, close=TRUE, ylab='PC2',xlab='PC1',main='Morphospace with Voronoi polygons')
    temp <- graphV$summary
    rownames(temp) <- pca_object[[2]]
    return(temp)
}
linear_regression <- function(filename, voronoiPolygons, desiredSite){
    require(dplyr)
    voronoiPolygons$species <- rownames(voronoiPolygons)
    fishAbundances <- read.csv(filename)
    fishAbundancesSummed <- fishAbundances %>% dplyr::group_by(Site, Common.name) %>% dplyr::summarize(Abundance = sum(Abundance))
    fishASbySite <- fishAbundancesSummed %>% dplyr::filter(Site %in% desiredSite)
    numberSpecies <- length(voronoiPolygons$species)
    fishASbySite <- fishASbySite %>% mutate(Common.name = tolower(as.character(Common.name)))
    print(unique(fishAbundancesSummed$Common.name))
    print(unique(voronoiPolygons$species))
    for(spec in fishASbySite$Common.name){
        voronoiPolygons[voronoiPolygons$species == spec, "abundances"] <- fishASbySite[fishASbySite$Common.name == spec, "Abundance"]
    }
    print(head(voronoiPolygons$abundances))
    model <- lm(data = voronoiPolygons, del.area~abundances)
    plot(data = voronoiPolygons, del.area~abundances)
    abline(model)
    summary(model)
}
