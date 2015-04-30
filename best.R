globalData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

best <- function(state, outcome) {
	## Read outcome data
	## Check that state and outcome are valid
	## Return hospital name in that state with lowest 30-day death
	## rate

	if  (!is.element(state, globalData$State)){
		stop("invalid state")
	}

	data = subset(globalData, globalData$State == state)
	vec = vector() 
	

	if (outcome == "heart attack"){
		vec = as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
	}

	else if (outcome == "heart failure"){
		vec = as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
	}

	else if (outcome == "pneumonia"){
		vec = as.numeric(data[,23])
	}

	else (stop("invalid outcome"))

	indices = which(vec == min(vec, na.rm = TRUE) )

	return(sort(data$Hospital.Name[indices])[1])

}
