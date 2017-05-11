meansd.nd <-
function(data, n){

	mn <- grep("mean", tolower(colnames(data)))
	sd <- grep("sd", tolower(colnames(data)))

	data.s <- data

	for (i in 1:length(mn)){

		tmp <- rnorm(n, data[1, mn[i]], data[1, sd[i]])
		data.s[1, mn[i]] <- mean(tmp)
		data.s[1, sd[i]] <- sd(tmp)
	}

	data.s
}
