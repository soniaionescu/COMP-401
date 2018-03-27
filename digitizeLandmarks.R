## pass in a csv with all of the file names of the photos you want to open. There should be no spaces in the file names. 
## the csv you're reading from, the csv you're writing to, and the photos, should be in your working directory. You can check your working directory with getwd().
## before running this, you must have the packages "jpeg" installed and loaded.
## A more detailed description of what this does is in the R markdown file called Description of how to digitize landmarks.
digitize_landmark <- function(filename){
    require(dplyr)
    ## perform this function for each file within the csv
    fishPhotos <- read.csv(file=filename, stringsAsFactors = FALSE, header=FALSE)
    ## create a data frame with labels for each labeled thing; this order is from Jonathan's report. This should be the order the fish is clicked on, later.
    df <- data.frame(matrix(ncol = 80, nrow = 0))
    cnames<- c("species", "file", "aMouthTipx", "aMouthTipy", "pMouthTipx", "pMouthTipy", "dBarbelx", "dBarbely", "aEyex", "aEyey", "pEyex", "pEyey", "vHeadx", "vHeady", "pHeadx", "pHeady", "dHeadx", "dHeady", "cPecx", "cPecy", "pPecx", "pPecy", "vPecx", "vPecy", "iPelx", "iPely", "dPelx", "dPely", "aiAnx", "aiAny", "vdAnx", "vdAny", "piAnx", "piAny", "viCaudx", "viCaudy", "dvCaudx", "dvCaudy", "pdvdCaudx", "pdvdCaudy", "ddCaudx", "ddCaudy", "diCaudx", "diCaudy", "piDorx", "piDory", "pDorx", "pDory", "ctDorx", "ctDory", "dsDorx", "dsDory", "aiDorx", "aiDory", "maxBHx", "maxBHy", "ddAdix", "ddAdiy")
    colnames(df) <- cnames
    for(i in 1:nrow(fishPhotos)){
        plot_jpeg(fishPhotos[i,1])
        fileNameOfFish <- fishPhotos[i, 1]
        speciesOfFish <- readline(prompt="What species is this fish?") ## ask the species of the fish
        print("click the anterior tip of mouth, posterior tip of the mouth, distal tip of parbel projected at 80 degrees of the body, anterior margin of eye, and posterior margin of eye")
        locator1 <- locator(5, type="p")
        print("click the ventral margin at the end of head, posterior margin at the end of head, dorsal margin at the end of head, central point at baseline of pectoral fin, and posterior tip of pectoral fin")
        locator2 <- locator(5, type="p")
        print("click the ventral margin of pectoral fin, insertion of pelvic fin, distal tip of pelvic fin, anterior insertion of anal fin, and ventral distal tip of anal fin")
        locator3 <- locator(5, type="p")
        print("click the posterior insertion of anal fil, ventral insertion of caudal fin, distal tip of ventral love of caudal fin, posterior distal margin between ventral and dorsal caudal loves, and distal tip of dorsal lobe of caudal fin")
        locator4 <- locator(5, type = "p")
        print("click the dorsal insertion of caudal fin, posterior insertion of the last dorsal fin, posterior tip of the dorsal fin, transition between spines and soft rays (if 1 dorsal fin) or central point on the dorsal margin of body between dorsal fins (if two), and distal tip of the first spine of first dorsal fin")
        locator5 <- locator(5, type = "p")
        print("click the anterior insertion of first dorsal fin, point at maximum body height on the dorsal margin of the body, and dorsal distal tip of adipose fin")
        locator6 <- locator(3, type = "p")
        allpoints <- data.frame(apply(cbind(locator1, locator2, locator3, locator4, locator5, locator6), 1, unlist))
        print("click two points an inch apart on the ruler")
        scale <- locator(2, type = "p") ## then, locate two points an inch apart on the ruler
        inch <- sqrt((scale$x[1]- scale$x[2])^2 + (scale$y[1] - scale$y[2])^2) ## calculate what an inch is in pixels using the Euclidean distance formula
        scaled <- list(allpoints$x/inch, allpoints$y/inch) ## now scale your list of points
        dataFrameOfCoord <- data.frame(x=scaled[[1]], y = scaled[[2]]) ## this creates a data frame with x coordinates in the x column and y coordinates in the y column
        ## now to put this dataFrame in the correct format
        df <- df %>% rbind(data.frame("species" = speciesOfFish, "file"=fileNameOfFish, "aMouthTipx"=dataFrameOfCoord[1,1], "aMouthTipy"=dataFrameOfCoord[1,2], "pMouthTipx"=dataFrameOfCoord[2,1], "pMouthTipy"=dataFrameOfCoord[2,2], "dBarbelx"=dataFrameOfCoord[3,1], "dBarbely"=dataFrameOfCoord[3,2], "aEyex"=dataFrameOfCoord[4,1], "aEyey"=dataFrameOfCoord[4,2], "pEyex"=dataFrameOfCoord[5,1], "pEyey"=dataFrameOfCoord[5,2], "vHeadx"=dataFrameOfCoord[6,1], "vHeady"=dataFrameOfCoord[6,2], "pHeadx"=dataFrameOfCoord[7,1], "pHeady"=dataFrameOfCoord[7,2], "dHeadx"=dataFrameOfCoord[8,1], "dHeady"=dataFrameOfCoord[8,2], "cPecx"=dataFrameOfCoord[9,1], "cPecy"=dataFrameOfCoord[9,2], "pPecx"=dataFrameOfCoord[10,1], "pPecy"=dataFrameOfCoord[10,2], "vPecx"=dataFrameOfCoord[11,1], "vPecy"=dataFrameOfCoord[11,2], "iPelx"=dataFrameOfCoord[12,1], "iPely"=dataFrameOfCoord[12,2], "dPelx"=dataFrameOfCoord[13,1], "dPely"=dataFrameOfCoord[13,2], "aiAnx"=dataFrameOfCoord[14,1], "aiAny"=dataFrameOfCoord[14,2], "vdAnx"=dataFrameOfCoord[15,1], "vdAny"=dataFrameOfCoord[15,2], "piAnx"=dataFrameOfCoord[16,1], "piAny"=dataFrameOfCoord[16,2], "viCaudx"=dataFrameOfCoord[17,1], "viCaudy"=dataFrameOfCoord[17,2], "dvCaudx"=dataFrameOfCoord[18,1], "dvCaudy"=dataFrameOfCoord[18,2], "pdvdCaudx"=dataFrameOfCoord[19,1], "pdvdCaudy"=dataFrameOfCoord[19,2], "ddCaudx"=dataFrameOfCoord[20,1], "ddCaudy"=dataFrameOfCoord[20,2], "diCaudx"=dataFrameOfCoord[21,1], "diCaudy"=dataFrameOfCoord[21,2], "piDorx"=dataFrameOfCoord[22,1], "piDory"=dataFrameOfCoord[22,2], "pDorx"=dataFrameOfCoord[23,1], "pDory"=dataFrameOfCoord[23,2], "ctDorx"=dataFrameOfCoord[24,1], "ctDory"=dataFrameOfCoord[24,2], "dsDorx"=dataFrameOfCoord[25,1], "dsDory"=dataFrameOfCoord[25,2], "aiDorx"=dataFrameOfCoord[26,1], "aiDory"=dataFrameOfCoord[26,2], "maxBHx"=dataFrameOfCoord[27,1], "maxBHy"=dataFrameOfCoord[27,2], "ddAdix"=dataFrameOfCoord[28,1], "ddAdiy"=dataFrameOfCoord[28,2]))
    }
    write.csv(df, file = "MyData.csv",row.names=FALSE)
}
plot_jpeg <- function(path, add=FALSE){
    require('jpeg')
    jpg = readJPEG(path, native=T) # read the file
    res = dim(jpg)[2:1] # get the resolution, [x, y]
    if (!add) # initialize an empty plot area if add==FALSE
        plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(jpg,1,1,res[1],res[2])
}
