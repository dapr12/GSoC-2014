
#rm(list=ls(all=TRUE))

fclass<- function (mdata, argvals = NULL, rangeval = NULL, names = NULL, fdata2d = FALSE) 
{

  if (is.list(argvals))  { fdata2d = TRUE }
  
  if (is.list(rangeval)) { fdata2d = TRUE }

  if (is.array(mdata)) { 
    
    dm <- dim(mdata)
    
    if (length(dm) > 2) {
    
      fdata2d = TRUE
    
    }
  
  }
  
  if (fdata2d) {
    
    out = list(data = NULL)
    
  if (length(class(mdata)) > 1) 
    
     { class(mdata) <- class(mdata)[1] }
    
    #Switch evaluates EXPR and accordingly chooses one of the further arguments (in ...). 
    
    out <- switch(class(mdata), matrix = {
    
      out[["data"]] <- mdata
      
      out
      
    }, data.frame = {
      out[["data"]] <- mdata
      out
    }, fdata = stop("The data could not be converted into fdata class"), 

                  numeric = {
                    
                    out[["data"]] <- matrix(mdata, nrow = 1)
                    
                    out
                    
                  }, integer = {
                    
                    out[["data"]] <- matrix(mdata, nrow = 1)
                    
                    out                    
                  }, array = {
                    
                    out[["data"]] = mdata
                    
                    out
                  })
    
    
    dm <- dim(out[["data"]])
    
    len.dm <- length(dm)
    
    if (is.null(argvals)) {
    
      argvals <- list()
      
      if (len.dm > 2) {
      
        len.argvals <- len.dm - 1
        
        for (i in 2:len.dm) {
        
          if (is.null(dimnames(out[["data"]][i]))) 
          
            argvals[[i - 1]] <- 1:dm[i]
        }
      
      }
      
      else {
        
        len.argvals <- len.dm
        
        for (i in 1:len.dm) {
          
          if (is.null(dimnames(out[["data"]][i]))) 
            
            argvals[[i]] <- 1:dm[i]
          
          if (is.null(names(argvals[[i]]))) 
            
            names(argvals[[i]]) <- paste(nam, 1:dm[i], sep = "")
        }
        
      }
      
      out[["argvals"]] <- argvals
      
    }
    
    else {
      
      if (is.list(argvals)) {
        
        len.argvals <- length(argvals)
        
        if (len.dm > 2) {
          
          for (i in 1:len.argvals) {
            
            if (length(argvals[[i]]) != dm[i + 1]) 
              
              stop("Incorrect dimension in between mdata and argvals arguments")
          }
          
        }
        
        else {
          
          for (i in 1:len.argvals) {
            
            if (length(argvals[[i]]) != dm[i]) 
              
              stop("Incorrect dimension in between mdata and argvals arguments")
            
          }
          
        }
        
      }
      
      else stop("The argument argvals must be a list")
      
      if (is.null(names(argvals))) 
        
        names(argvals) <- paste(drop(nam), 1:len.argvals, sep = "")
      
      out[["argvals"]] <- argvals
      
    }
    
    if (is.null(rangeval)) {
      
      rangeval <- list()
      
      for (i in 1:len.argvals) {
        
        rangeval[[i]] <- range(argvals[i])
        
      }
      
      if (is.null(names(rangeval))) 
        
        names(rangeval) <- names(argvals)
      
      out[["rangeval"]] <- rangeval
      
      len.rangeval <- length(out[["rangeval"]])
      
    }
    
    else {
      
      if (is.list(rangeval)) {
        
        len.rangeval <- length(rangeval)
        
        if (len.rangeval != len.argvals) 
          
          stop("Incorrect dimension in rangeval argument")
      }
      
      else stop("The argument reangeval must be a list")
      
      if (is.null(names(rangeval))) 
        
        names(rangeval) <- names(argvals)
      
      out[["rangeval"]] <- rangeval
      
    }
    
    if (is.null(names)) {
      
      names <- list()
      
      names[["xlab"]] <- paste("argvals ", 1:len.argvals, sep = "")
      
      names[["ylab"]] <- paste("values of ", nam, len.argvals, sep = "")
      
      names[["main"]] <- paste(nam, len.argvals, sep = "")
      
      out[["names"]] <- names
    }
    
    else {
      
      if (is.list(names)) {
        
        len.names <- length(names[["xlab"]])
        
        if (len.rangeval != len.names) 
          
          stop("Incorrect dimension in names argument")
        
      }
      
      else stop("The argument names must be a list")
      
      out[["names"]] <- names
      
    }
    
    #class(out) = c("fdata", "fdata2d")
    class(out) = c("fdata")
    
    return(out)
    
  }

  #else {
    out = list(data = NULL)
    if (length(class(mdata)) > 1) 
      class(mdata) <- class(mdata)[1]
    out <- switch(class(mdata), matrix = {
      out[["data"]] = mdata
      out
    }, data.frame = {
      out[["data"]] = as.matrix(mdata)
      out
    }, fdata = stop("The data could not be converted into fdata class"), 
                  numeric = {
                    out[["data"]] = matrix(mdata, nrow = 1)
                    out
                  }, integer = {
                    out[["data"]] = matrix(mdata, nrow = 1)
                    out
                  }, fd = {
                    r = mdata$basis[[3]]
                    if (is.null(argvals)) argvals = seq(r[1], r[2], 
                                                        len = mdata$basis$nbasis)
                    nb <- length(argvals)
                    tt = argvals
                    out[["data"]] = t(eval.fd(tt, mdata))
                    if (!is.null(mdata$fdnames$reps)) rownames(out[["data"]]) = mdata$fdnames$reps else rownames(out[["data"]]) = 1:nrow(out[["data"]])
                    if (!is.null(mdata$fdnames$time)) {
                      colnames(out[["data"]]) = 1:ncol(out[["data"]])
                    } else {
                      colnames(out[["data"]]) = 1:ncol(out[["data"]])
                    }
                    out
                  }, fds = {
                    out[["data"]] = mdata$y
                    if (is.null(mdata$time)) out[["argvals"]] = 1:ncol(out[["data"]]) else out[["argvals"]] = seq(mdata$time[1], 
                                                                                                                  mdata$time[length(mdata$time)], len = length(mdata$time))
                    out
                  }, fts = {
                    out[["data"]] = mdata$y
                    if (is.null(mdata$time)) out[["argvals"]] <- 1:ncol(out[["data"]]) else out[["argvals"]] <- seq(mdata$time[1], 
                                                                                                                    mdata$time[length(mdata$time)], len = length(mdata$time))
                    out
                  }, sfts = {
                    out[["data"]] = mdata$y
                    if (is.null(mdata$time)) out[["argvals"]] = 1:ncol(out[["data"]]) else out[["argvals"]] = seq(mdata$time[1], 
                                                                                                                  mdata$time[length(mdata$time)], len = length(mdata$time))
                    out
                  })
    nc <- nrow(out[["data"]])
    np <- ncol(out[["data"]])
    if (is.null(argvals)) {
      if (is.null(colnames(out[["data"]]))) {
        out[["argvals"]] = 1:ncol(out[["data"]])
      }
      else {
        out[["argvals"]] = 1:ncol(out[["data"]])
      }
    }
    else out[["argvals"]] = argvals
    lentt = length(out[["argvals"]])
    if (is.null(rangeval)) 
      rangeval = range(out[["argvals"]])
    out[["rangeval"]] <- rangeval
    if ((np != lentt) && (nc == lentt)) {
      out[["data"]] = matrix(out[["data"]], ncol = nc)
      nc <- 1
      print("Warning: The transposed data is returned")
    }
    else out[["data"]] = out[["data"]]
    if (is.null(dimnames(mdata))) {
      colnames(out[["data"]]) = round(out[["argvals"]], 
                                      4)
    }
    out[["names"]] <- list(main = "fdataobj", xlab = "t", 
                           ylab = "X(t)")
    if (!is.null(names$main)) 
      out$names$main <- names$main
    if (!is.null(names$xlab)) 
      out$names$xlab <- names$xlab
    if (!is.null(names$ylab)) 
      out$names$ylab <- names$ylab
    class(out) = "fdata"
    return(out)
  }



#a1<-seq(0,1,by=.01)
#a2=rnorm(length(a1),sd=0.2)
#f1<-(sin(2*pi*a1))+rnorm(length(a1),sd=0.2)
#nc<-10
#np<-length(f1)
#tt=seq(0,1,len=101)
#mdata<-matrix(NA,ncol=np,nrow=nc)
#for (i in 1:nc) mdata[i,]<- (sin(2*pi*a1))+rnorm(length(a1),sd=0.2)
#fdataobj1<-fclass(mdata,tt)
#fdataobj1
#fdataobj2<-fclass(mdata)
#fdataobj2
