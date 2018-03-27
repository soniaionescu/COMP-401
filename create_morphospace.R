species_average <- function(filename){
    require(geomorph)
    fishies <- read.csv(filename) ## read the input
    species <- unique(fishies$species) ## make a vector with the unique filenames
    atts <- colnames(fishies)[endsWith(colnames(fishies), "x")] ## make a vector with the unique attributes
    atts <- substr(atts, 1, nchar(atts) - 1) ## amend the vector with the unique attributes
    ar <- array(dim = c(28, 2, length(species)), dimnames = list(atts, c("x", "y"), species)) ## make array and name dimensions
    gpag <- overall_average(filename) ## scale every fish using GPA
    for(i in 1:length(species)){ ## for each species, find the average fish
        scaled_species <- gpag$coords[ , , fishies$species == species[i]]
        ar[ , , species[i]] <- apply(scaled_species, c(1, 2), mean)
    }
    return(ar) ##return an array with all of the scaled, average fish
}

overall_average <- function(filename){
    require(geomorph)
    fishies <- read.csv(filename)
    n_fish <- nrow(fishies)
    ar <- array(dim = c(28, 2, n_fish))
    for(i in 1:n_fish){
        ar[1:28, 1, i] <- as.numeric(fishies[i, endsWith(colnames(fishies), "x")])
        ar[1:28, 2, i] <- as.numeric(fishies[i, endsWith(colnames(fishies), "y")])
    }
    return(gpagen(ar, print.progress = FALSE))
}
relative_warp <- function(array){
    require(ggplot2)
    require(ggfortify)
    numberSpecies <- length(array[1,1,])
    name_coords <- character(56)
    xs <- paste0(dimnames(array)[[1]], "x")
    ys <- paste0(dimnames(array)[[1]], "y")
    for(i in 1:56){
        if(i %% 2 == 1){
            name_coords[i] <- xs[ceiling(i/2)]
        }
        else{
            name_coords[i] <- ys[ceiling(i/2)]
        }
    }
    cartesianCoords <- (matrix(0, ncol = 28*2, nrow = numberSpecies, 
                               dimnames = list(dimnames(array)[[3]], name_coords)))
    for(i in 1:numberSpecies){
        counter <- 1
        for(j in 1:28){ ## put x value
            for(k in 1:2){ ## put y value
                cartesianCoords[i, counter]<-array[j, k, i]
                counter <- counter + 1
            }
        }
    }
    
    df <- data.frame(cartesianCoords, species = dimnames(array)[[3]])

    print(autoplot(prcomp(cartesianCoords), scale = FALSE, data = df, colour = 'species', main = "Morphospace"))
    return(list(prcomp(cartesianCoords), df$species))
}


