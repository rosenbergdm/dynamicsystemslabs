# Vivian Choi
# Chapter 0 Homework
# vchoi.chapter0.R

# 1.)
fibK <- function(k) {

	x0=0
	x1=1
	while(x1 <= k) {
		x2=x0+x1
		x0=x1
		x1=x2
	}
	x0	
}

closeToK <- function(k, x, y) {
	x1=abs(k-x)
	y1=abs(k-y)
	if(x1>y1) {
		y
	} else if (y1>x1) {
		x
	} else {
		cat(x, "and", y, "are equally close to", k)
	}
}

fibClosestToK <- function(k) {

	x0=0
	x1=1
	while(x1 <= k) {
		x2=x0+x1
		x0=x1
		x1=x2
	}
	closeToK(k, x0, x1)	
}


# 2.)
sort2 <- function(vec) {
	if (vec[1] > vec[2]) {
		vecSort = c(vec[2], vec[1])
	} 
	vecSort
}

sort <- function(vec) {
	vecSort = c(vec[1])	
	iVec = 2
	while (iVec <= length(vec)) {
		i = 1
		while((i <= length(vecSort)) & (vec[iVec] >= vecSort[i])) {
			i = i+1
		}
		if (i == 1) {
			vecSort = c(vec[iVec], vecSort)
		} else if (i > length(vecSort)) {
			vecSort = c(vecSort, vec[iVec])
		} else {
			vecSort = c(vecSort[1:i-1], vec[iVec], vecSort[i:length(vecSort)])
		}
		iVec = iVec + 1
	}
	vecSort
}

combine2 <- function(v1, v2) {
	v1 = c(v1, v2)
	sort(v1)
}



# 3.)
solutions = function(k) {
	i = 1
	solns = c()
	while (i <= k) {
		solution = complex(real = cos(6.28/i), imaginary=sin(6.28/i))
		solns = c(solution, solns)
		i = i+1
	}
	solns
}