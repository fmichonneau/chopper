##' Creates a set of alignments for given a list of extractions and
##' markers.
##'
##' This function takes a list of extractions and creates alignments
##' from them. When more than one marker is specified, empty sequences
##' might be added if they are not available for this particular
##' marker, to create alignments with the same number of sequences
##' across all markers. The alignment is being performed by
##' MUSCLE. This function can also call Gblocks to remove ambiguous
##' parts in the alignment. To function properly R needs to be able to
##' access MUSCLE and Gblocks with the command system(), this means
##' that the executables need to be in your PATH.
##' @title Merge sequences
##' @param listFiles names (vector of character) of the individual
##' sequence files to be merged.
##' @param output folder where the write the output files. If the
##' folder doesn't exist, the user is asked whether it should be
##' created.
##' @param seqFolder the folder that contains the individual files
##' being stored in their own folders.
##' @param markers the list of markers to be used to build the
##' alignemnt. They need to match the subfolders found in
##' \sQuote{seqFolder}
##' @param convertEnds Should the gaps at the extremities of the
##' sequences be replaced by missing data?
##' @param checkAmbiguity Should the sequences be checked for
##' ambiguities before merging?
##' @param gblocks If different from NULL, gblocks should be a named
##' list with the marker as its names and the parameters to be passed
##' to Gblocks as its values. This function assumes that the extension
##' of the file created by Gblocks is \sQuote{-gb} so you don't want
##' to use the \sQuote{-e=} flag.
##' @param gapchar Characters to be used to create empty sequences for
##' files that are not sequenced for a given marker.
##' @param justCheck If FALSE, no alignment is created.
##' @param returnData If TRUE, a data frame that contains TRUE/FALSE
##' indicating whether a particular extraction (element of listFiles)
##' is found for each of the markers.
##' @return Mostly used for its side effect of creating alignment for
##' a list of extractions, given a list of markers. If returnData is
##' TRUE, it also returns a data frame that includes which extraction
##' has been sequenced for which marker.
##' @author François Michonneau
##' @export
mergeSeq <- function(listFiles, output, seqFolder="~/Documents/seqRepository",
                     markers=c("16S", "16Sc"), convertEnds=TRUE,
                     checkAmbiguity=TRUE, gblocks=NULL, gapchar="?",
                     justCheck=FALSE, returnData=TRUE) {

    if (any(duplicated(listFiles))) {
        stop("duplicated sequence names: ",
             paste(listFiles[duplicated(listFiles)], sep="", collaspe=", "))
    }

    if (!file.exists(output)) {
        md <- readline("output location doesn't exist. Create it? (y/n)")
        if (md == "y") {
            system(paste("mkdir", output))
        }
        else stop("Stopped by user. Create the folder specified in \'output\'.")
    }

    if (!is.null(gblocks)) {
        stopifnot(inherits(gblocks, "list"))
        stopifnot(all(names(gblocks) %in% markers))
    }

    timeAlg <- format(Sys.time(), "%Y%m%d-%H%M%S")
    allFiles <- list.files(seqFolder, include.dirs=TRUE, full.names=TRUE)
    allSeqFolder <- allFiles[file.info(allFiles)$isdir]
    allSeqFolderShort <- sapply(allSeqFolder, function(x) {
        xx <- unlist(strsplit(x, "/"))
        xx <- xx[-c(1,2)]
        file.path(xx)
    })
    allSeqFolderShort <- unname(allSeqFolderShort)

    summarySeq <- array(, dim=c(length(listFiles), length(markers)))
    dimnames(summarySeq) <- list(listFiles, markers)

    if (length(markers) > 0) {
        if (! all(markers %in% allSeqFolderShort)) {
            mrkr <- markers[! markers %in% allSeqFolderShort]
            mrkr <- ifelse(length(mrkr > 1), paste(mrkr, collapse=", "), mrkr)
            stop(mrkr, " not found in the seqFolder: ", seqFolder)
        }
    }
    else {
        markers <- allSeqFolderShort
    }

    listAlgFile <- character(length(markers))

    allSeq <- character(0)
    for (j in 1:length(markers)) {
        tmpPth <- file.path(seqFolder, markers[j])
        tmpFiles <- list.files(path=tmpPth)
        allSeq <- union(allSeq, tmpFiles)
    }

    for (j in 1:length(markers)) {
        seqNo <- character(0)
        algName <- paste(timeAlg, markers[j], sep="-")
        listAlgFile[j] <- algName
        algFile <- paste(output, "/", algName, ".fas", sep="")
        algOut <- paste(output, "/", algName, ".afa", sep="")
        existSeq <- character(0)
        for (i in 1:length(listFiles)) {
            if (length(grep(",", listFiles[i]) > 0)) {
                fnm <- unlist(strsplit(listFiles[i], ","))
            }
            else fnm <- listFiles[i]
            fnm <- sapply(fnm, function(x) gsub("^\\s+|\\s+$", "", x))

            seqNm <- file.path(seqFolder, markers[j], fnm)
            seqExists <- file.exists(seqNm)
            seqNm <- seqNm[seqExists]
            if (length(seqNm) > 1) stop("duplicate sequence?:", seqNm)
            if (length(seqNm) == 1) {
                tmpSeq <- read.dna(file=seqNm, format="fasta")
                if (dim(tmpSeq)[1] > 1)
                    warning("Wrong dimension for:", listFiles[i], dim(tmpSeq))
                dimnames(tmpSeq)[[1]][1] <- fnm[1] ## always use the first ext ID for the seq name
                if(!justCheck)
                    write.dna(tmpSeq, file=algFile, format="fasta", colsep="",
                              append=TRUE)
                summarySeq[i, j] <- TRUE
                existSeq <- c(existSeq, seqNm)
            }
            else {
                summarySeq[i, j] <- FALSE
                if (! any(fnm %in% allSeq)) next
                seqNo <- c(seqNo, fnm[1])
            }
        }
        if (length(existSeq) == 0) stop("no sequence for ", markers[j])
        if (length(existSeq) <= 3) warning("Less than 3 sequences for: ", markers[j])
        if (!justCheck) {
            cmdMuscle <- paste("muscle -in ", algFile, " -out ", algOut, sep="")
            system(cmdMuscle)
            if (!is.null(gblocks) && markers[j] %in% names(gblocks)) {
                inGblocks <- algOut
                outGblocks <- paste(inGblocks, "-gb", sep="")
                paramsGblocks <- gblocks[markers[j]]
                cmdGblocks <- paste("Gblocks", inGblocks, paramsGblocks)
                system(cmdGblocks)
                cmdRename <- paste("mv", algOut, paste(algOut, "-beforeGblocks", sep=""))
                system(cmdRename)
                cmdRename2 <- paste("mv", outGblocks, algOut)
                system(cmdRename2)
            }
            if (checkAmbiguity) {
                amb <- checkAmbiguity(algOut)
                if (length(amb) > 0) {
                    warning("There are some abiguities in your alignment:",
                            algOut)
                }
            }
            if (convertEnds) {
                convertEndsToMissingAlg(algOut, algOut)
            }
            algSeq <- read.dna(file=algOut, format="fasta")
            lAlg <- dim(algSeq)[2]
            if (length(seqNo) > 0) {
                for (k in 1:length(seqNo)) {
                    cat(">", seqNo[k], "\n", sep="", file=algOut, append=TRUE)
                    cat(rep(gapchar, lAlg), "\n\n", sep="", file=algOut, append=TRUE)
                }
            }
        }
    }
    if (returnData)
        as.data.frame(summarySeq)
}
