format_sources <-
function(data, labels){

	if (!is.list(labels)){
		stop("Argument 'labels' has to be a list.")
	}

	tab <- as.data.frame(matrix(nrow = length(labels), ncol = ((ncol(data) - 1) * 2) + 1))

	for (i in 1:length(labels)){

		tab[i, 1] <- as.character(names(labels)[i])
		tmp <- data[data[ , 1] %in% labels[[i]], ]

		for (j in 1:(ncol(data) - 1)){

			if (i == 1){

				colnames(tab)[1]         <- "Sources"
				colnames(tab)[j * 2]     <- paste0("Mean", colnames(data)[j + 1])
				colnames(tab)[j * 2 + 1] <- paste0("SD",   colnames(data)[j + 1])
			}
			tab[i, j * 2]     <- round(mean(tmp[ , j + 1]), 1)
			tab[i, j * 2 + 1] <- round(sd(tmp[ , j + 1]), 1)
		}
	}
	tab
}
