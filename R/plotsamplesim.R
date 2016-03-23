plotsamplesim <-
function(name = "Simul", layout = "mpd", yaxis = "fixed", bg = T,
col.bg = "lightgray", col.box = "white", family = "serif", cex.main = ifelse(layout == "spd", 1, 2), cex.axis = ifelse(layout == "spd", .75, 1.25),
    cex.lgd = ifelse(layout == "spd", .75, 1.25), cex.lab = ifelse(layout == "spd", 1, 1.25),
    cex.text = ifelse(layout == "spd", 1, 2), ...){

	if (length(which(dir() == name)) == 0)
		stop("Select the appropriate folder or simulation name.")

	setwd(name)

	par(family = family)

	log <- readLines("logfile.txt")
	int <- strsplit(log[grep("width:", log)], ": ")[[1]][2]
	type <- strsplit(log[grep("sampling:", log)], ": ")[[1]][2]

	if (length(grep("Modified", log)) == 2){
		modwhich <- strsplit(log[grep("Modified source:", log)], ": ")[[1]][2]
		modname <- strsplit(log[grep("Modified source name:", log)], ": ")[[1]][2]
	}else{
		modwhich <- NULL
	}

	load("medians.Rdata")
	load("widths.Rdata")

	sources <- dimnames(medians)[[2]]
	nbsources <- length(sources)
	sizes <- unlist(lapply(strsplit(dimnames(medians)[[3]], "\\."), function(x) x[2]))
	nbsizes <- length(sizes)
	nrep <- dim(medians)[1]

	interval <- (nbsources - 1) * 0.2 + 1
	startp <- 0.5

	xpoints <- matrix(NA, nbsources, nbsizes)

	for (i in 1 : nbsources){
		xpoints[i, ] <- (c(1 : nbsizes) - 1) * interval + startp
		startp <- startp + 0.2
	}
	adj <- diff(c(0, (max(xpoints) + 0.2)))*0.005

	colour <- rainbow(250)[round(seq(1, 250, length.out = nbsources + 1))]
	colour <- colour[1 : nbsources]

	if (layout == "mpd"){

		layout(matrix(c(rep(1, 2), 2:3, rep(4, 2)), 3, 2, byrow = T), widths = c(3, 3), heights = c(1, 3, 1))

		par(mar = c(0, 0, 0, 0))
		plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), col = "white", axes = F, bty = "n", xlab = "", ylab = "")

		polygon(x = c(-1.045, -1.045, 1.045, 1.045, -1.045),y = c(-0.35, 0.35, 0.35, -0.35, -0.35), col = "lightgray", border = "darkgray")
		text(0, 0, "Influence of sample size on mixing model solutions", font = 2, family = family, cex = cex.main)

		legend("bottom", sources, lwd = 2, col = colour, horiz = T, cex = cex.lgd, bty = "n", text.font = 4)
	}

	### PLOT OF WIDTH OF CREDIBLE INTERVALS ###

	txt <- paste("Width of the ", int, "% credible intervals", sep = "")

	if ((nbsources %% 2) == 0){

		xtck <- (xpoints[nbsources %/% 2, ] + xpoints[(nbsources %/% 2) + 1, ])/2

	}else{

		xtck <- xpoints[(nbsources %/% 2) + 1, ]
	}

	if (yaxis == "fixed") ylim <- c(0, 1)
	if (yaxis == "adaptive"){
		ylim <- round(c(min(widths), max(widths)), 2)
		inc <- (max(ylim) - min(ylim)) / 40
		ylim[1] <- ylim[1] - inc ; ylim[2] <- ylim[2] + inc
	}

	ytck <- seq(min(ylim), max(ylim), length.out = 11)

	# Empty plot
	if (layout == "mpd") par(mar = c(5, 4, 0, 0))
	plot(0, col = "white", axes = F, bty = "n", xlim = c(0, (max(xpoints) + 0.5)), ylim = ylim, xlab = "", ylab = "", font.lab = 2, family = family)

	axis(1, pos = min(ylim), at = xtck, labels = sizes, family = family, cex.axis = cex.axis)
	mtext(side = 1, "Sample size", line = 1.5, family = family, font = 2, cex = cex.lab)

	axis(2, pos = 0, at = ytck, labels = round(ytck, 2),las = 1, family = family, cex.axis = cex.axis)
	mtext(side = 2, txt, line = 1.5, family = family, font = 2, cex = cex.lab)

	# Background
	if (bg){

		polygon(x = c(0, 0, max(xpoints) + 0.5, max(xpoints) + 0.5, 0),y = c(min(ylim), max(ylim), max(ylim), min(ylim), min(ylim)),col = col.bg, border = col.box, lwd = ifelse(col.box == "black", 1, 2))

		for (i in seq(2, 10, by = 2))
			points(x = c(0, max(xpoints) + 0.5), y = c(ytck[i], ytck[i]), type = "l", col = col.box, lty = 3)

		for (i in seq(3, 9, by = 2))
			points(x = c(0, max(xpoints) + 0.5), y = c(ytck[i], ytck[i]), type = "l", col = col.box, lty = 1)

		for (i in 1 : (length(xtck) - 1))
			points(x = c((xtck[i] + xtck[i+1]) / 2, (xtck[i] + xtck[i+1]) / 2), y = c(min(ylim), max(ylim)), type = "l", col = col.box)

		for (k in 1 : ncol(xpoints))
			points(x = c(xpoints[1, k], xpoints[nrow(xpoints), k]), y = c(min(ylim), min(ylim)), type = "l", lwd = 2, col = "black")
	}

	# Adding informations
	for (j in 1 : nbsources){

		points(xpoints[j, ], apply(widths[ , j, ], 2, mean), pch = 4, col = colour[j], cex = 0.75)

		for (k in 1 : ncol(xpoints)){

			points(c(xpoints[j, k], xpoints[j, k]), c(apply(widths[ , j, ], 2, min)[k], apply(widths[ , j, ], 2, max)[k]), type = "l", col = colour[j])
			xpts <- c(xpoints[j, k] - adj, xpoints[j, k] + adj)
			points(xpts, c(apply(widths[ , j, ], 2, min)[k], apply(widths[ , j, ], 2, min)[k]), type = "l", col = colour[j], lwd = 1)
			points(xpts, c(apply(widths[ , j, ], 2, max)[k], apply(widths[ , j, ], 2, max)[k]), type = "l", col = colour[j], lwd = 1)
		}
	}

	if (layout == "spd"){
		legend("top", sources, lwd = 2, col = colour, horiz = F, bg = "white",
            bty = "o", text.font = 4, cex = cex.lgd, ncol = 3, box.col = "white")
		title(main = "Influence of sample size on mixing model solutions", font = 2, family = family, cex.main = cex.main)
	}

	### PLOT OF MEDIAN OF POSTERIOR DISTRIBUTION ###

	if (layout == "spd"){
        graphoutcome <- try(windows(), silent = TRUE)
        if(length(graphoutcome) > 0) graphoutcome <- try(dev.new(), silent = TRUE)
        if(length(graphoutcome) > 0) graphoutcome <- try(x11(), silent = TRUE)
        if(length(graphoutcome) > 0) print("Graphics unavailable on this workstation")
	}
	if (yaxis == "fixed") ylim <- c(0, 1)
	if (yaxis == "adaptive"){
		ylim <- round(c(min(medians), max(medians)), 2)
		inc <- (max(ylim) - min(ylim)) / 40
		ylim[1] <- ylim[1] - inc ; ylim[2] <- ylim[2] + inc
	}

	ytck <- seq(min(ylim), max(ylim), length.out = 11)

	# Empty plot
	if (layout == "mpd") par(mar = c(5, 4, 0, 0))
	plot(0, col = "white", axes = F, bty = "n", xlim = c(0, (max(xpoints) + 0.5)), ylim = ylim, xlab = "", ylab = "", font.lab = 2, family = family)

	axis(1, pos = min(ylim), at = xtck, labels = sizes, family = family, cex.axis = cex.axis)
	mtext(side = 1, "Sample size", line = 1.5, family = family, font = 2, cex = cex.lab)

	axis(2, pos = 0, at = ytck, labels = round(ytck, 2),las = 1, family = family, cex.axis = cex.axis)
	mtext(side = 2, "Median of posterior distributions", line = 1.5, family = family, font = 2, cex = cex.lab)

	# Background
	if (bg){

		polygon(x = c(0, 0, max(xpoints) + 0.5, max(xpoints) + 0.5, 0),y = c(min(ylim), max(ylim), max(ylim), min(ylim), min(ylim)),col = col.bg, border = col.box, lwd = ifelse(col.box == "black", 1, 2))

		for (i in seq(2, 10, by = 2))
			points(x = c(0, max(xpoints) + 0.5), y = c(ytck[i], ytck[i]), type = "l", col = col.box, lty = 3)

		for (i in seq(3, 9, by = 2))
			points(x = c(0, max(xpoints) + 0.5), y = c(ytck[i], ytck[i]), type = "l", col = col.box, lty = 1)

		for (i in 1 : (length(xtck) - 1))
			points(x = c((xtck[i] + xtck[i+1]) / 2, (xtck[i] + xtck[i+1]) / 2), y = c(min(ylim), max(ylim)), type = "l", col = col.box)

		for (k in 1 : ncol(xpoints))
			points(x = c(xpoints[1, k], xpoints[nrow(xpoints), k]), y = c(min(ylim), min(ylim)), type = "l", lwd = 2, col = "black")
	}

	# Adding informations
	for (j in 1 : nbsources){

		points(xpoints[j, ], apply(medians[ , j, ], 2, mean), pch = 4, col = colour[j], cex = 0.75)

		for (k in 1 : ncol(xpoints)){

			points(c(xpoints[j, k], xpoints[j, k]), c(apply(medians[ , j, ], 2, min)[k], apply(medians[ , j, ], 2, max)[k]), type = "l", col = colour[j])
			xpts <- c(xpoints[j, k] - adj, xpoints[j, k] + adj)
			points(xpts, c(apply(medians[ , j, ], 2, min)[k], apply(medians[ , j, ], 2, min)[k]), type = "l", col = colour[j], lwd = 1)
			points(xpts, c(apply(medians[ , j, ], 2, max)[k], apply(medians[ , j, ], 2, max)[k]), type = "l", col = colour[j], lwd = 1)
		}
	}

	if (layout == "spd"){
		legend("top", sources, lwd = 2, col = colour, horiz = F, bg = "white",
            bty = "o", text.font = 4, cex = cex.lgd, ncol = 3, box.col = "white")
		title(main = "Influence of sample size on mixing model solutions", font = 2, family = family, cex.main = cex.main)
	}

	if (layout == "mpd"){
		par(mar = c(3, 0, 0, 0))
		plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), col = "white", axes = F, bty = "n", xlab = "", ylab = "")
		if (!is.null(modwhich)){
			txt <- paste("For this plot source ", modwhich, " (", modname,")", " has been modified\nSample sizes have been modified for ", type, "\n\n", sep = "")
			text(0, 0, txt, font = 2, family = family, cex = cex.text)
		}else{
			txt <- paste("Sample sizes have been modified for", type, "\n\n")
			text(0, 0, txt, font = 2, family = family, cex = cex.text)
		}
	}
	setwd("..")
}
