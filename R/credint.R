credint <- function(
	data,
	interval
) {

	return(
		apply(
			X        = data,
			MARGIN   = 2,
			FUN      = credintt,
			interval = interval
		)
	)
}
