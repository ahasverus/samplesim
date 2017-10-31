meansd.nd <- function(
	data,
	n,
	input
) {

	if (input == "means") {

		mn <- grep("mean", tolower(colnames(data)))
		sd <- grep("sd", tolower(colnames(data)))

		data.s <- data

		for (i in 1:length(mn)) {

			tmp <- rnorm(n, data[1, mn[i]], data[1, sd[i]])
			data.s[1, mn[i]] <- mean(tmp)
			data.s[1, sd[i]] <- sd(tmp)
		}

		return(data.s)

	} else {

		mn <- grep("mean", tolower(colnames(data)))
		sd <- grep("sd", tolower(colnames(data)))

		data.s <- data

		tmp <- data.frame()

		for (i in 1:length(mn)) {

			if (i == 1) {

				tmp <- rnorm(n, data[1, mn[i]], data[1, sd[i]])

			} else {

				tmp <- cbind(tmp, rnorm(n, data[1, mn[i]], data[1, sd[i]]))
			}
		}
		colnames(tmp) <- gsub("Mean", "", colnames(data[mn]))

		return(tmp)
	}
}
