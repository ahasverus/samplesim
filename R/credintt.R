credintt <-
function(data, interval){
	
	out <- round((length(data) - (interval/100) * length(data)) / 2)	
	return(c(sort(data)[out + 1], sort(data)[length(data) - out - 1]))
}
