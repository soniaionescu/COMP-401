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
    cnames<- c("species", "file", "ftLipx", "ftLipy", "bMouthx", "bMouthy", "fbLipx", "fbLipy", "bGillSlitx", "bGillSlity", "bGillx", "bGilly", "tGillSlitx", "tGillSlity", "fPecx", "fPecy", "bPecx", "bPecy", "fPelx", "fPely", "bPelx", "bPely", "fAnx", "fAny", "lAnx", "lAny", "bAnx", "bAny", "bCaudPenx", "bCaudPeny", "lCaudx", "lCaudy", "blLobeCaudx", "blLobeCaudy", "cCaudx", "cCaudy", "buLobeCaudx", "buLobeCaudy", "tCaudx", "tCaudy", "tCaudPenx", "tCaudPeny", "bSDx", "bSDy", "bmSDx", "bmSDy", "mSDx", "mSDy", "uSDx", "uSDy", "fSDx", "fSDy", "bSpDx", "bSpDy", "lSpDx", "lSpDy", "uSpDx", "uSpDy", "mSpDx", "mSpDy", "fSpDx", "fSpDy", "c30x", "c30y", "tHeadx", "tHeady", "lEyex", "lEyey", "bEyex", "bEyey", "rEyex", "rEyey", "tEyex", "tEyey", "m35x", "m35y", "nx", "ny", "rHeadx", "rHeady")
    colnames(df) <- cnames
    for(i in 1:nrow(fishPhotos)){
        plot_jpeg(fishPhotos[i,1])
        fileNameOfFish <- fishPhotos[i, 1]
        speciesOfFish <- readline(prompt="What species is this fish?") ## ask the species of the fish
        print("click the front top lip, back side of mouth, front bottom lip, bottom gill slip, and backmost part gil")
        locator1 <- locator(5, type="l")
        print("click the top gill slit, front of pectoral fin, back of pectoral fin, front of pelvic fin, back of pelvic fin")
        locator2 <- locator(5, type="l")
        print("click the front base of anal fin, lowest point of anal fin, back base of anal fin, bottom of caudal pedunchle, and lowest point of caudal fin")
        locator3 <- locator(5, type="l")
        print("click the back most point of lower lobe caudal fin, center/indentation of caudal fin, back most point of upper lobe caudal fin, top most point of caudal fin, and top of caudal peduncle")
        locator4 <- locator(5, type = "l")
        print("click the back base of soft dorsal, back most point of soft dowsal, midpoint contour of soft dowsal, uppermost point of soft dowsal, and front base of soft dorsal")
        locator5 <- locator(5, type = "l")
        print("click the back base of spint dorsal fin, left most point of spiny dorsal fin, upper most point of spiny dorsal fin, midpoint between summit and base of spliny dorsal fin, and the front base of spiny dorsal fin")
        locator6 <- locator(5, type = "l")
        print("click the center of the back, the top of the head, the left most point of the eye, the bottom most point of the eye, the right most point of the eye")
        locator7 <- locator(5, type="l")
        print("click the top most point of the eye, the midpoint between the right most and top most point of the eye projected on the top of the body, the nostril projected on the top of the body, and the right most point of the head")
        locator8 <- locator(4, type = "l")
        allpoints <- data.frame(apply(cbind(locator1, locator2, locator3, locator4, locator5, locator6, locator7, locator8), 1, unlist))
        print("click two points an inch apart on the ruler")
        scale <- locator(2, type = "l") ## then, locate two points an inch apart on the ruler
        inch <- sqrt((scale$x[1]- scale$x[2])^2 + (scale$y[1] - scale$y[2])^2) ## calculate what an inch is in pixels using the Euclidean distance formula
        scaled <- list(allpoints$x/inch, allpoints$y/inch) ## now scale your list of points
        dataFrameOfCoord <- data.frame(x=scaled[[1]], y = scaled[[2]]) ## this creates a data frame with x coordinates in the x column and y coordinates in the y column
        ## now to put this dataFrame in the correct format
        df <- df %>% rbind(data.frame("species" = speciesOfFish, "file"=fileNameOfFish, "ftLipx"=dataFrameOfCoord[1,1], "ftLipy"=dataFrameOfCoord[1,2], "bMouthx"=dataFrameOfCoord[2,1], "bMouthy"=dataFrameOfCoord[2,2], "fbLipx"=dataFrameOfCoord[3,1], "fbLipy"=dataFrameOfCoord[3,2], "bGillSlitx"=dataFrameOfCoord[4,1], "bGillSlity"=dataFrameOfCoord[4,2], "bGillx"=dataFrameOfCoord[5,1], "bGilly"=dataFrameOfCoord[5,2], "tGillSlitx"=dataFrameOfCoord[6,1], "tGillSlity"=dataFrameOfCoord[6,2], "fPecx"=dataFrameOfCoord[7,1], "fPecy"=dataFrameOfCoord[7,2], "bPecx"=dataFrameOfCoord[8,1], "bPecy"=dataFrameOfCoord[8,2], "fPelx"=dataFrameOfCoord[9,1], "fPely"=dataFrameOfCoord[9,2], "bPelx"=dataFrameOfCoord[10,1], "bPely"=dataFrameOfCoord[10,2], "fAnx"=dataFrameOfCoord[11,1], "fAny"=dataFrameOfCoord[11,2], "lAnx"=dataFrameOfCoord[12,1], "lAny"=dataFrameOfCoord[12,2], "bAnx"=dataFrameOfCoord[13,1], "bAny"=dataFrameOfCoord[13,2], "bCaudPenx"=dataFrameOfCoord[14,1], "bCaudPeny"=dataFrameOfCoord[14,2], "lCaudx"=dataFrameOfCoord[15,1], "lCaudy"=dataFrameOfCoord[15,2], "blLobeCaudx"=dataFrameOfCoord[16,1], "blLobeCaudy"=dataFrameOfCoord[16,2], "cCaudx"=dataFrameOfCoord[17,1], "cCaudy"=dataFrameOfCoord[17,2], "buLobeCaudx"=dataFrameOfCoord[18,1], "buLobeCaudy"=dataFrameOfCoord[18,2], "tCaudx"=dataFrameOfCoord[19,1], "tCaudy"=dataFrameOfCoord[19,2], "tCaudPenx"=dataFrameOfCoord[20,1], "tCaudPeny"=dataFrameOfCoord[20,2], "bSDx"=dataFrameOfCoord[21,1], "bSDy"=dataFrameOfCoord[21,1], "bmSDx"=dataFrameOfCoord[22,1], "bmSDy"=dataFrameOfCoord[22,2], "mSDx"=dataFrameOfCoord[23,1], "mSDy"=dataFrameOfCoord[23,2], "uSDx"=dataFrameOfCoord[24,1], "uSDy"=dataFrameOfCoord[24,2], "fSDx"=dataFrameOfCoord[25,1], "fSDy"=dataFrameOfCoord[25,2], "bSpDx"=dataFrameOfCoord[26,1], "bSpDy"=dataFrameOfCoord[26,2], "lSpDx"=dataFrameOfCoord[27,1], lSpDy=dataFrameOfCoord[27,2], "uSpDx"=dataFrameOfCoord[28,1], "uSpDy"=dataFrameOfCoord[28,2], "mSpDx"=dataFrameOfCoord[29,1], "mSpDy"=dataFrameOfCoord[29,2], "fSpDx"=dataFrameOfCoord[30,1], "fSpDy"=dataFrameOfCoord[30,2], "c30x"=dataFrameOfCoord[31,1], "c30y"=dataFrameOfCoord[31,2], "tHeadx"=dataFrameOfCoord[32,1], "tHeady"=dataFrameOfCoord[32,2], "lEyex"=dataFrameOfCoord[33,1], "lEyey"=dataFrameOfCoord[33,2], "bEyex"=dataFrameOfCoord[34,1], "bEyey"=dataFrameOfCoord[34,2], "rEyex"=dataFrameOfCoord[35,1], "rEyey"=dataFrameOfCoord[35,2], "tEyex"=dataFrameOfCoord[36,1], "tEyey"=dataFrameOfCoord[36,2], "m35x"=dataFrameOfCoord[37,1], "m35y"=dataFrameOfCoord[37,2], "nx"=dataFrameOfCoord[38, 1], "ny"=dataFrameOfCoord[38, 2], "rHeadx"=dataFrameOfCoord[39, 1], "rHeady"=dataFrameOfCoord[39,2]))
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
