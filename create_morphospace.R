species_average <- function(filename){
    require(geomorph)
    fishies <- read.csv(filename) ## read the input
    species <- unique(fishies$species) ## make a vector with the unique filenames
    atts <- colnames(fishies)[endsWith(colnames(fishies), "x")] ## make a vector with the unique attributes
    atts <- substr(atts, 1, nchar(atts) - 1) ## amend the vector with the unique attributes
    ar <- array(dim = c(39, 2, length(species)), dimnames = list(atts, c("x", "y"), species)) ## make array and name dimensions
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
    ar <- array(dim = c(39, 2, n_fish))
    for(i in 1:n_fish){
        ar[1:39, 1, i] <- as.numeric(fishies[i, endsWith(colnames(fishies), "x")])
        ar[1:39, 2, i] <- as.numeric(fishies[i, endsWith(colnames(fishies), "y")])
    }
    return(gpagen(ar, print.progress = FALSE))
}
