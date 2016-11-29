## Functions creates "matrix" object that we can cache the inverse of for repeated use.
##First fuction creates the cached matrix.  Second function returns inverse of cached matrix, either from cache,
## or calculates inverse and puts it in cache



makeCacheMatrix <- function(x = matrix())
{
        inverseM<-NULL
        set<-function(y)
        {
                x<<-y
                inverseM<<-NULL
        }
        get<-function()
        {
                x
        }
        setMatrix<-function(matrix)
        {
                inverseM<<-matrix
        }
        getMatrix<-function()
        {
                inverseM
        }
        list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
        

}


## Returns inverse of "matrix" passed in.  Calculates if necessary.
##argument is List() NOT matrix, must run makecachematrix(x) first!

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}
