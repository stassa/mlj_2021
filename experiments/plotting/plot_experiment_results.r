library(Hmisc)

source('experiment_data.r', local=T)

# Line types and colours
# 1: sorts, 2: matrices, 3: punches, 4: baseline.
# The baseline is drawn without points but needs a point type in pnt.typs
# otherwise the legend function will raise an error.
plot.type <- 'b'
lin.typs <- 1:4 
pnt.typs <- 1:4
systems.cols <- c('red','blue','magenta','brown')

# Plot title and labels
# Uncomment for plot identification.
# title <- paste('Learning target:',target)
title <- ''
x.lab <- 'Sort metarules' 
# Adjust y-label to metric.
if (metric == 'Time'){
	y.lab <- 'Time (sec.)'
} else {
	y.lab <- metric
}

# Legend
leg.pos <- 'topleft'
# Last legend item is the empty hypothesis. Can't find how to render \U2205,
# "Empty Set" correctly so using \U00D8 "Latin capital letter O with stroke" as
# an alternative.
leg.text <- c('Sorts', 'Matrices', 'Punches','H = \U00D8')
leg.lin.cols <- systems.cols
leg.lin.typs <- lin.typs
# 26 is the very-small-dot point type for the baseline that shouldn't have any
# point type really.
leg.pnt.typs <- c(1,2,3,26)
leg.lwd <- 4.0
# Increased legend text size
leg.cex <- 3

# Error bar line and point types
bar.type = 'o'
sort.bar.col = 'gray48'
matrix.bar.col = 'brown4'
punch.bar.col = 'darkgreen'

# Large axis, ticks, label, line and point sizes
# Better for papers. Tweak for other uses.
cex.axis <- 2.70
cex.lab <- 2.8
cex <- 2.5
lwd.ticks=3.0
lwd <- 3.0
# Increased errorbar sizes.
cap <- 0.025

# The x-axis enumerates results.
results.size <- length(sort.means)
x.axis <- 1:results.size

# Calculate standard errors.
sort.ses <- sort.sds / sqrt(results.size)
matrix.ses <- matrix.sds / sqrt(results.size)
punch.ses <- punch.sds / sqrt(results.size)

# Calculate the accuracy of the empty hypothesis as the baseline of predictive
# accuracy. When an experiment times out we return the accuracy of the empty
# hypothesis.
baseline <- function(pos, neg) {
	base.line <- neg / (pos + neg)
}

base.line <- baseline(pos,neg)

# Calculate plot limits
y.lim.max <- max(sort.means+sort.ses, matrix.means+sort.ses, punch.means+punch.ses)
y.lim.min <- min(base.line,sort.means-sort.ses, matrix.means-sort.ses, punch.means-punch.ses)
# Must call before getting legend size.
plot.new()
leg.size <- legend(leg.pos, inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, cex=leg.cex, lwd=leg.lwd, plot=F)
# Note legend height added to y-axis limit
y.lim <- c(y.lim.min, y.lim.max + leg.size$rect$h + 0.01)
x.lim <- c(1, results.size + 0.5)

p <- par()
par(mar=c(6,6.1,1.0,0.8), mgp=c(4,1,0) )

# Main plot lines
plot(x.axis, main=title, sort.means, ylim=y.lim, type=plot.type, lty=lin.typs[1], pch=pnt.typs[1], col=systems.cols[1], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, cex.main=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)
lines(x.axis, matrix.means, ylim=y.lim, type=plot.type, lty=lin.typs[2], pch=pnt.typs[2], col=systems.cols[2], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)
lines(x.axis, punch.means, ylim=y.lim, type=plot.type, lty=lin.typs[3], pch=pnt.typs[3], col=systems.cols[3], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)

# Baseline - omitted when the metric is running time becaue time doesn't have a
# baseline. I'm sure there's an existential joke in there somewhere.
if(metric != 'Time') {
	abline(h=base.line, type=plot.type, lty=lin.typs[4], pch=pnt.typs[4], col=systems.cols[4], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)
}

# Error bars
errbar(x.axis, sort.means, yplus=sort.means+sort.ses, yminus=sort.means-sort.ses, col=0, pch=1, type=bar.type, errbar.col=sort.bar.col, add=T, cap=cap, lwd=lwd)
errbar(x.axis, matrix.means, yplus=matrix.means+matrix.ses, yminus=matrix.means-matrix.ses, col=0, pch=1, type=bar.type, errbar.col=matrix.bar.col, add=T, cap=cap, lwd=lwd)
errbar(x.axis, punch.means, yplus=punch.means+punch.ses, yminus=punch.means-punch.ses, col=0, pch=1, type=bar.type, errbar.col=punch.bar.col, add=T, cap=cap, lwd=lwd)

# Plot axis, adjusted to contents.
axis(1, at=x.axis, labels=rev(x.axis), cex.axis=cex.axis, cex.lab=cex.lab, padj=0.5, lwd.ticks=lwd.ticks)

# There's no baseline for time, so we adjust the legend elements.
if (metric == 'Time') {
	leg.text <- leg.text[1:3]
	leg.lin.typs <- leg.lin.typs[1:3]
	leg.pnt.typs <- leg.pnt.typs[1:3]
	leg.lin.cols <- leg.lin.cols[1:3]
	legend(leg.pos, inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, col=leg.lin.cols, cex=leg.cex, lwd=leg.lwd)
} else {
	legend(leg.pos, inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, col=leg.lin.cols, cex=leg.cex, lwd=leg.lwd)
}

par(p)
