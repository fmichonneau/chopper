##' puts vertical bars next to monophyletic clades in a tree.
##'
##' Based on a tree (as a \code{phylo} object) and a character-vector,
##' this function draws vertical bars next to each monophyletic
##' group. Each element of the vector must contain a character string
##' that matches (through \code{\link{grep}}) all the tips that are
##' included in each of the monophyletic group. In other words, this
##' function does not identify the monophyletic groups for you. Also,
##' the length of the vector should be of the same length as the number
##' of monophyletic groups in your tree.
##'
##' The element of the vector are considered labels and will be
##' displayed next to the vertical bars.
##' @title barMonophyletic
##' @param groupLabel a vector of mode \code{character} that indicates
##' the labels for each of the monophyletic group.
##' @param groupMatch a vector of mode \code{character} that contains
##' a way to identify each of the monophyletic clade. The function
##' \code{\link{grep}} is used for this process.
##' @param tree a phylogenetic tree a \code{phylo} object.
##' @param cex.plot the \code{\link[par]{cex}} (in \code{par}) value
##' for plotting the tree.
##' @param cex.text the \code{\link[par]{cex}} (in \code{par}) value
##' for the text next to the vertical bars.
##' @param include.tip.label should the tip labels be displayed on the
##' tree? (default is FALSE)
##' @param extra.space amount of extra space (additive)
##' @param coef.space amount of space (multiplicative)
##' @param draw should a plot be produce? (default is TRUE)
##' @param text.offset how much space to add between the vertical bar
##' and the text.
##' @param font type of font to be used for the text next to the
##' vertical bars.
##' @param font.col color of the font to be used for the text next to
##' the vertical bars.
##' @param seg.col color of the vertical bar.
##' @param srt the string rotation in degrees (see
##' \code{\link[par]{srt}} in \code{par}) of the text next to the
##' vertical bars.
##' @param bar.at.tips should the vertical bars be plotted right next
##' to the tips? (default FALSE)
##' @return Function mostly used for its side effect of plotting a phylogeny
##' but it also returns invisibly the total width of the tree. 
##' @export
##' @author François Michonneau
barMonophyletic <- function(groupLabel, groupMatch, tree, cex.plot, cex.text=.8,
                            include.tip.label=FALSE, extra.space=0, coef.space=1,
                            draw=TRUE, text.offset=1.02, font=1, font.col=1,
                            seg.col=1, srt=0, bar.at.tips=FALSE) {
 
  font.col <- rep(font.col, length(groupLabel))
  font <- rep(font, length(groupLabel))
  seg.col <- rep(seg.col, length(groupLabel))

  stopifnot(class(tree) == "phylo")
  stopifnot(length(groupLabel) == length(groupMatch))
  
  getTipOrderPlot <- function(tr) {
    tr$tip.label[tr$edge[tr$edge[, 2] <= length(tr$tip.label), 2]]
  }

  findMaxWidth <- function(tr, grpLbl, cex.plot, include.tip.label) {
    offLabel <- ifelse(include.tip.label, strwidth(tr$tip.label, cex=cex.plot), 0)
    max(branching.times(tr)) + extra.space + offLabel 
  }
  
  grpID <- vector("list", length(groupLabel))
  names(grpID) <- groupLabel
  
  for (i in 1:length(groupMatch)) {
    xx <- grep(groupMatch[i], tree$tip.label, value=TRUE)
    grpID[[i]] <- match(xx, getTipOrderPlot(tree))
  }

  maxTip <- length(tree$tip.label)
 
  barPos <- findMaxWidth(tr=tree, groupLabel, cex.plot=cex.plot, include.tip.label=include.tip.label) * coef.space
 
  if (draw) {
    for (i in 1:length(groupLabel)) {
      if (length(grpID[[i]]) == 1) {
        rg <- c(grpID[[i]] - 0.2, grpID[[i]] + 0.2)      
      }
      else {
        rg <- range(grpID[i])
      }
      if (bar.at.tips) {
        tips <- range(grpID[i])
        offLabel <- ifelse(include.tip.label, max(strwidth(tree$tip.label[tips], cex=cex.text)), 0)
        lastPP <- get("last_plot.phylo", envir=.PlotPhyloEnv)
        barPos <- max(lastPP$xx[tips]) + offLabel + extra.space
      }
      text(barPos*text.offset, mean(rg), groupLabel[i], adj=0, font=font[i], col=font.col[i], srt=srt)
      segments(barPos, rg[1], barPos, rg[2], lwd=3, col=seg.col[i])
    }
  }
  
  toRet <- barPos + max(strwidth(groupLabel, cex=cex.text))
  names(toRet) <- "TotalWidth"
  invisible(toRet)
}
