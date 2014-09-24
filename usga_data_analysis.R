library(ggplot2)
library(grid)
library(reshape)
library(scales)
library(plyr)
source("calculate_match_play_paths.R")
source("draw_match_play_tree.R")

usga = read.csv("usga_hole_by_hole_data.csv", header=TRUE)
usga$result = usga$next_score - usga$score
usga = subset(usga, is.na(result) | abs(result) < 2) # remove a single bad data point
final_results = subset(usga, is.na(next_score))
total_holes_played = sum(!is.na(usga$result))

possible_final_scores = -10:10
terminal_points = numberOfPaths()$terminalPoints

totalsByScore = data.frame(score = possible_final_scores,
                           theoretical = sapply(possible_final_scores, function(s) { sum(terminal_points[paste(10:18, s, sep=",")], na.rm=TRUE) }),
                           actual = sapply(possible_final_scores, function(s) { sum(final_results$score == s) }))

totalsByScore$theoretical = totalsByScore$theoretical / sum(totalsByScore$theoretical)
totalsByScore$actual = totalsByScore$actual / sum(totalsByScore$actual)
totalsMelted = melt(totalsByScore, id="score")
totalsMelted$variable = factor(totalsMelted$variable, labels=c("theoretical uniform", "USGA amateur actual"))

ggplot(data=totalsMelted, aes(x=score, y=value)) +
  geom_bar(stat="identity") +
  facet_wrap(~variable, ncol=1) +
  scale_y_continuous("distribution of paths\n", labels=percent) +
  scale_x_continuous("\nFinal Match Score") +
  theme_gray(base_size=20) +
  labs(title="Total Number of Paths by Final Score\n")

back9 = subset(usga, hole > 9 & !is.na(next_score))
possible_current_scores = -9:9

raw = lapply(possible_current_scores, function(s) {
  s_data = subset(back9, score == s)
  
  win = sum(s_data$result == 1)
  lose = sum(s_data$result == -1)
  halve = sum(s_data$result == 0)
  tot = win + lose + halve
  
  return(c(score = s, win = win / tot, lose = lose / tot, halve = halve / tot, total = tot))
})

probabilities = subset(data.frame(do.call(rbind, raw)), total > 250)

ggplot(data = subset(melt(probabilities, id="score"), variable != "total"), aes(x=score, y=value, color=variable)) +
  geom_line(size=1) +
  scale_y_continuous("Probability of Next Hole Outcome\n", labels=percent, lim=c(0, 0.5)) +
  scale_x_continuous("\nCurrent Match Score", breaks=seq(-4,4,by=2)) +
  scale_color_discrete("") +
  labs(title="Probability of Next Hole Outcome for Player 1\n") +
  theme_gray(base_size=20)

counts = ddply(subset(usga, !is.na(next_score)), c("hole", "score", "next_score"), summarize, count=length(hole))
counts = subset(counts, hole + abs(score) <= 18)
nColors = 100
colors = colorRampPalette(c("#555555", "#0000ff", "#ffffff"))(nColors)
counts$colorIx = pmax(1, ceiling(nColors * log(counts$count) / max(log(counts$count))))
counts$color = colors[counts$colorIx]
counts$size = pmax(sqrt(counts$count) / 20, 0.25)
pts = generatePoints(numberOfHoles=18)
pts$count = NA
for(i in which(pts$terminal)) {
  pts$count[i] = sum(final_results$hole == pts$holes[i] & final_results$score == pts$score[i])
}
for(i in which(pts$terminal)) {
  pts$color[i] = colorRampPalette(c("#0000ff", "#ffffff"))(50)[pmax(1, ceiling(50 * pts$count[i] / max(pts$count, na.rm=TRUE)))]
}

ggplot(data = subset(pts, terminal), aes(x=holes, y=score, color=color)) +
  geom_segment(data = counts, aes(x=hole, xend=hole + 1, y=score, yend=next_score, color=color, size=size)) +
  geom_point(aes(size=sqrt(count / 10))) +
  scale_color_identity() +
  scale_size_identity() +
  scale_y_continuous("Score", breaks=seq(-10, 10, by=2), minor_breaks=NULL) +
  scale_x_continuous("\nHoles Played", breaks=seq(0, 18, by=2), minor_breaks=NULL) +
  labs(title="USGA Amateur Championships 2010-2014\n") +
  theme_bw(base_size=20) +
  theme(panel.grid.major=element_blank(), panel.background=element_rect(color=NA, fill="black"))
