s <- read.csv('./data/sp18-bcs-A.csv', stringsAsFactors=F)
a <- read.csv('./data/answers.csv', stringsAsFactors=F)$answer			# Read the answers in to a list

# Sort s on the basis of 'rid'
s <- s[,-1]					# Remove id column because we are about to sort
s <- s[order(s$rid),]
rownames(s) <- NULL			# Update the row ids to unscramble them

# Replace the empty responses with NR (No Response)
s[s == ''] <- 'NR'

# From s create a data-frame r which details whether the response to each question was right or wrong (T or F)
r <- s[,c("rid", "qid")]

# Loop over the 30 answers
for (i in 1:30) {

	key <- paste("X", i, sep="")		# Create the 'X' prefixed column name for the i-th question
	r[[key]] = s[[key]] == a[i]			# Compare the value in s$qi to the actual answer (a[i]) and store it in 'r'
										# Note the use of [[]] instead of $ (which is a short-hand for [[]]) to access the column of the data-frame when using a variable to hold the column name
}

# Calculate the total for each student by summing over the Xi entries (which are columns 3 to 32)
r$tot <- rowSums(r[,3:32])		


# Graph data
library(ggplot2)
library(plyr)		# for 'count' function

# Plot side-by-side histograms of the total scores earned for each qid (type / version mix)
# p <- ggplot(r, aes(tot, fill=qid)) + geom_histogram(alpha = 0.5, breaks=seq(0,30,by=2), aes(y = ..count..), position ='dodge')
# p2 <- ggplot(r[r$qid == "v2" | r$qid == "v3",], aes(tot, fill=qid)) + geom_histogram(alpha = 0.5, breaks=seq(0,30,by=2), aes(y = ..count..), position ='dodge')

# Create a data-frame containing the question number and the number of correct response to each question
# For the latter we use 'colSums' to calculate the number of correct results.
# df = data.frame( question = seq(1,30), correct = colSums( r[,3:32] ) )
# p3 = ggplot(df, aes(x = question, y = correct)) + geom_col() + ggtitle('Number of correct responses to each question') + xlab('Question #') + ylab('Number Correct')
# p3 = ggplot(df, aes(x = question, y = correct)) + geom_col() + 
# 												ggtitle('Number of correct responses to each question') + 
# 												scale_x_discrete("Question #", limits=seq(1,30)) +
# 												scale_y_discrete("Number Correct", limits=seq(0,25,5))


# Function for creating bar plot showing frequency of students who chose a particular response (or NR)
plot_responses <- function(num, ans) {

	cs = c("grey50", "grey50", "grey50", "grey50", "grey50", "grey50")		# List of colors all the default
	correct_response = ans[num]

	index = paste("q", num, sep="")
	data = count( s[,c(index)] )
	ans_index = match(correct_response, data$x)
	cs[ans_index] = "green4"		# change color of correct response

	# Find index of correct_response and use it to replace the relevant color to "green4" in 'cs' so that the correct response bar becomes green

	p = ggplot(data, aes(x = x, y = freq, fill = x)) + geom_col() +				# Each value of x can have a different fill color
																ggtitle( paste('Responses to Q.', num, sep="") ) +
																xlab("Response") +
																ylab("Frequency") + 
																scale_fill_manual(values = cs) +		# Manually specify the fill colors
																theme(legend.position = "none")		# Remove legend introduced by manual fill

	return (p)
}
