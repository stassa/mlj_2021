library(Hmisc)

source('experiment_data.r', local=T)

# Line types and colours
# Two-value vectors because the first one was originally for Thelma
plot.type <- 'b'
lin.typs <- 1:3
pnt.typs <- 1:3
systems.cols <- c('red','blue','magenta')

# Plot title and labels
title <- paste('Learning target:',target)
x.lab <- 'Sort metarules' 
y.lab <- metric

# Legend
leg.text <- c('Sorts', 'Matrices', 'Punches')
leg.lin.cols <- systems.cols
leg.lin.typs <- lin.typs
leg.pnt.typs <- pnt.typs
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

results.size <- length(sort.means)
x.axis <- 1:results.size

# Calculate standard errors.
sort.ses <- sort.sds / sqrt(results.size)
matrix.ses <- matrix.sds / sqrt(results.size)
punch.ses <- punch.sds / sqrt(results.size)

# Calculate plot limits
y.lim.max <- max(sort.means+sort.ses, matrix.means+sort.ses, punch.means+punch.ses)
y.lim.min <- min(sort.means-sort.ses, matrix.means-sort.ses, punch.means-punch.ses)
# Must call before getting legend size.
plot.new()
leg.size <- legend('topleft', inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, cex=leg.cex, lwd=leg.lwd, plot=F)
y.lim <- c(y.lim.min, y.lim.max + leg.size$rect$h + 0.01)
x.lim <- c(1, results.size + 0.5)

p <- par()
par(mar=c(5.3,6.1,4.0,0.8), mgp=c(4,1,0) )

plot(x.axis, main=title, sort.means, ylim=y.lim, type=plot.type, lty=lin.typs[1], pch=pnt.typs[1], col=systems.cols[1], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, cex.main=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)
lines(x.axis, matrix.means, ylim=y.lim, type=plot.type, lty=lin.typs[2], pch=pnt.typs[2], col=systems.cols[2], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)
lines(x.axis, punch.means, ylim=y.lim, type=plot.type, lty=lin.typs[3], pch=pnt.typs[3], col=systems.cols[3], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)

errbar(x.axis, sort.means, yplus=sort.means+sort.ses, yminus=sort.means-sort.ses, col=0, pch=1, type=bar.type, errbar.col=sort.bar.col, add=T, cap=cap, lwd=lwd)
errbar(x.axis, matrix.means, yplus=matrix.means+matrix.ses, yminus=matrix.means-matrix.ses, col=0, pch=1, type=bar.type, errbar.col=matrix.bar.col, add=T, cap=cap, lwd=lwd)
errbar(x.axis, punch.means, yplus=punch.means+punch.ses, yminus=punch.means-punch.ses, col=0, pch=1, type=bar.type, errbar.col=punch.bar.col, add=T, cap=cap, lwd=lwd)

axis(1, at=x.axis, labels=rev(x.axis), cex.axis=cex.axis, cex.lab=cex.lab, padj=0.5, lwd.ticks=lwd.ticks)

legend('topleft', inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, col=leg.lin.cols, cex=leg.cex, lwd=leg.lwd)

par(p)
