## This function stores functions for the creation and caching of the inverse of a matrix object.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){ #changes the matrix in the main function for storing the cache of a matrix
                x<<-y
                m<<-NULL #empties previously cached inverse if matrix is replaced
        }
        get<-function() x #returns the matrix x stored in the main function
        setinverse<-function(inverse) inv<<-inverse #stores the inverse of a matrix in the main function
        getinverse<-function() inv #returns the inverse of a matrix in the main function 
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) #list makes formulas call-able with $ operator               
}

## This function will return a cached inverse of a matrix if one is stored, otherwise it will solve for a new matrix and cache the result.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){ #verifies value of inverse if already stored in cache in main formula makeCacheMatrix
                message("getting cached data")
                return(inv)
        }
        data<-x$get() #else, runs get() function from main function to retrieve matrix object entered as input in the main function
        inv<-solve(data,...) #solves for the inverse of the matrix from the retrieved matrix input
        x$setinverse(inv) #sets the value of the inverse in the cache of the main function
        inv
}

## citing sources used in this assignment: 
## (1) "Programming Assignment 2" assignment page by Roger D. Peng, R Programming, Johns Hopkins Bloomberg School of Public Health, accessed 5/17/2015, https://class.coursera.org/rprog-014/human_grading/view/courses/973495/assessments/3/submissions
## (2) "PA2-clarifying_instructions" by DanieleP, GitHub, accessed 5/17/2015, https://github.com/DanieleP/PA2-clarifying_instructions
