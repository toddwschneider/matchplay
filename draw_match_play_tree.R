library(ggplot2)
library(grid)

generatePoints = function(numberOfHoles=18) {
  xvals = 0:numberOfHoles
  yvals = -numberOfHoles:numberOfHoles
  
  pts = expand.grid(xvals, yvals)
  names(pts) = c("holes", "score")
  
  pts = pts[abs(pts$score) <= pts$holes,]
  pts = pts[abs(pts$score) <= (numberOfHoles - pts$holes + 2),]
  pts = pts[order(pts$holes, pts$score),]
  pts$terminal = (abs(pts$score) > (numberOfHoles - pts$holes)) | (pts$holes == numberOfHoles)
  pts$color = sapply(pts$terminal, function(test) ifelse(test, "#ff0000", "#000000"))
  
  return(pts)
}

plotMatchPlayGrid = function(numberOfHoles=18, arrow=FALSE, size=2.5) {
  pts = generatePoints(numberOfHoles)
  ptsNotTerminal = subset(pts, !terminal)
  lwd = 0.75
  segmentColor = "#999999"
  
  if (arrow) {
    segmentArrow = arrow(angle = 15, length = unit(0.35, "cm"), type="closed")
  } else {
    segmentArrow = NULL
  }
  
  p = ggplot(data = pts, aes(x=holes, y=score, color=color)) +
        geom_segment(data = ptsNotTerminal, aes(xend=holes + 1, yend=score - 1), lwd=lwd, color=segmentColor, arrow=segmentArrow) +
        geom_segment(data = ptsNotTerminal, aes(xend=holes + 1, yend=score), lwd=lwd, color=segmentColor, arrow=segmentArrow) +
        geom_segment(data = ptsNotTerminal, aes(xend=holes + 1, yend=score + 1), lwd=lwd, color=segmentColor, arrow=segmentArrow) +
        geom_point(aes(color=terminal), size=size) +
        scale_color_manual(guide=FALSE, values=c("black", "red")) +
        scale_y_continuous("Score", breaks=min(pts$score):max(pts$score), minor_breaks=NULL) +
        scale_x_continuous("\nHoles Played", breaks=min(pts$holes):max(pts$holes), minor_breaks=NULL) +
        labs(title=gsub("NUM", numberOfHoles, "NUM Hole Match Play Tree\n")) +
        theme_bw(base_size=20)
  
  return(p)
}

# plotMatchPlayGrid()
# plotMatchPlayGrid(3, arrow=TRUE, size=4)
