s <- read.csv('./data/sp18-bcs-A.csv', stringsAsFactors=F)
a <- read.csv('./data/answers.csv', stringsAsFactors=F)$answer			# Read the answers in to a list

# From s create a data-frame r which details whether the response to each question was right or wrong (T or F)
r <- s[,c("rid", "qid")]

# Loop over the 30 answers
for (i in 1:30) {

	key <- paste("X", i, sep="")		# Create the X prefixed column name for the i-th question
	r[[key]] = s[[key]] == a[i]			# Compare the value in s$Xi to the actual answer (a[i]) and store it in 'r'
										# Note the use of [[]] instead of $ (which is a short-hand for [[]]) to access the column of the data-frame when using a variable to hold the column name
}
