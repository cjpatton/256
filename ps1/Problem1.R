
# DES.R:  R routines for discrete-event simulation (DES), with an example

# matrix version; data frame allows character event types, but much too slow

# all data is stored in an R environment variable that will be referrred
# to as simlist below

# the simlist will consist of the following components:
#
#       currtime:  current simulated time
#       evnts:  the events list, a matrix
#       reactevent:  event handler, user-supplied; creates new
#                    events upon the occurrence of an old one;
#                    e.g. job arrival triggers either start of 
#                    service for the job or queuing it; call form is
#                    reactevent(evnt,simlist)
#       dbg:  if TRUE, will print evnts above after each event
#             scheduling action, and enter R browser for single-stepping
#             etc.

# the application code can add further application-specific data to
# simlist, e.g. total job queuing time 

# each event will be represented by a matrix row consisting of: 
# 
#    occurrence time
#    event type (user-defined numeric code)
#
# and application-specific information, if any

# library functions (do not alter):
# 
#       newsim:  create a new simlist
#       insevnt:  insert a new event into evnts in the simlist
#       schedevnt:  schedule a new event (determine its occurrence time
#                   and call insevnt())
#       getnextevnt:  pulls the earliest event from the event list,
#                     process it, and update the current simulated
#                     time
#       mainloop:  as the name implies
#       appendtofcfsqueue:  append job to a FCFS queue
#       delfcfsqueue:  delete head of a FCFS queue

# outline of a typical application:

#    mysim <- newsim()    create the simlist
#    set reactevent in mysim
#    set application-specific variables in mysim, if any
#    set the first event in mysim$evnts
#    mainloop(mysim,mysimtimelim)
#    print results

# create a simlist, which will be the return value, an R environment
newsim <- function(dbg=F) {
   simlist <- new.env()
   simlist$currtime <- 0.0  # current simulated time
   simlist$evnts <- NULL  # event list
   simlist$dbg <- dbg
   simlist
}

# insert event evnt into evnts in simlist
insevnt <- function(evnt,simlist) {
   # if the event list is empty, set it to consist of evnt and return
   if (is.null(simlist$evnts)) {
      simlist$evnts <- matrix(evnt,nrow=1)
      return()
   }
   # otherwise, find insertion point
   inspt <- binsearch(simlist$evnts[,1],evnt[1])
   # now "insert," by reconstructing the matrix; we find what portion of
   # the current matrix should come before evnt and what portion should 
   # come after it, then string everything together
   before <- if (inspt == 1) NULL else simlist$evnts[1:(inspt-1),]
   nr <- nrow(simlist$evnts)
   after <- if (inspt <= nr) simlist$evnts[inspt:nr,] else NULL  
   simlist$evnts <- rbind(before,evnt,after)  
   rownames(simlist$evnts) <- NULL
}

# schedule new event in evnts in simlist; evnttime is the time at
# which the event is to occur; evnttype is the event type; appdata is
# a vector of numerical application-specific data
schedevnt <- function(evnttime,evnttype,simlist,appdata=NULL) {
   evnt <- c(evnttime,evnttype,appdata)
   insevnt(evnt,simlist)  
}

# start to process next event (second half done by application
# programmer via call to reactevnt() from mainloop())
getnextevnt <- function(simlist) {
   head <- simlist$evnts[1,]
   # delete head
   if (nrow(simlist$evnts) == 1) simlist$evnts <- NULL else 
      simlist$evnts <- simlist$evnts[-1,,drop=F]  
   return(head)
}

# main loop of the simulation
mainloop <- function(simlist,simtimelim) {
   while(simlist$currtime < simtimelim) {
      head <- getnextevnt(simlist)  
      # update current simulated time
      simlist$currtime <- head[1]  
      # process this event (programmer-supplied ftn)
      simlist$reactevent(head,simlist)  
      if (simlist$dbg) {
         print("event occurred:")
         print(head)
         print("events list now")
         print(simlist$evnts)
         browser()
      }
   }
}

# binary search of insertion point of y in the sorted vector x; returns
# the position in x before which y should be inserted, with the value
# length(x)+1 if y is larger than x[length(x)]; this could be replaced
# by faster C code
binsearch <- function(x,y) {
   n <- length(x)
   lo <- 1
   hi <- n
   while(lo+1 < hi) {
      mid <- floor((lo+hi)/2)
      if (y == x[mid]) return(mid)
      if (y < x[mid]) hi <- mid else lo <- mid
   }
   if (y <= x[lo]) return(lo)
   if (y < x[hi]) return(hi)
   return(hi+1)
}

# appendtofcfsqueue() and delfcfsqueuehead() below assume the
# application code has one or more queues, each queue stored as a
# list-of-lists, with each individual list being the information for one
# queued job; note that one must initialize the list-of-lists as NULL 

# appends jobtoqueue to the given queue, assumed of the above form;
# the new, longer list is returned
appendtofcfsqueue <- function(queue,jobtoqueue) {
   lng <- length(queue)
   queue[[lng+1]] <- jobtoqueue
   queue
}

# deletes head of queue; assumes list-of-lists structure as decribed
# above; returns the head and new queue
delfcfsqueuehead <- function(queue) {
   qhead <- queue[[1]]
   newqueue <- queue[-1]
   # careful!--an empty list is not NULL  
   if (length(queue) == 1) newqueue <- NULL
   list(qhead=qhead,newqueue=newqueue)
}




## Start of our code.



# Event types
# 1 - machine failed
# 2 - machine repaired
factory <- function(u, r, k, timelim, dbg=F) {
  simlist <- newsim(dbg)
  simlist$reactevent <- factoryreact
  
  simlist$lambda_u = 1/u
  simlist$lambda_r = 1/r
  simlist$k <- k # total machines
  simlist$i <- k # up machines
  simlist$time <- rep(0, k)

  # First event is a machine failure. 
  ttf <- min(rexp(k, 1/u))
  simlist$totaltime <- ttf
  simlist$time[k] <- ttf
  schedevnt(ttf, 1, simlist)

  # Enter main loop. 
  mainloop(simlist, timelim)

  # Report average number of machines running. 
  simlist$time <- simlist$time / simlist$totaltime
  w <- 0
  for (i in 1 : k) 
  {
    w <- w + (simlist$time[i] * i)    
  }
  print("Average number of machines running")
  print(w)
}

# Our reactevent(). Transition to new state 
# and generate next event. 
factoryreact <- function(evnt, simlist) {
  etype <- evnt[2] 
  
  # Transition state. 

  if (etype == 1) # failure
  {
    simlist$i = simlist$i - 1
  }

  else if (etype == 2) # repair
  {
    simlist$i = simlist$i + 1
  }

  # Choose next event. 

  if (simlist$i == 0) 
  {
    tte <- min(rexp(simlist$k, simlist$lambda_r))
    etype <- 2
  }
  
  else if (simlist$i == simlist$k)
  {
    tte <- min(rexp(simlist$k, simlist$lambda_u))
    etype <- 1
    simlist$time[simlist$i] <- simlist$time[simlist$i] + tte
  }

  else 
  {
    ttf <- min(rexp(simlist$i, simlist$lambda_u))
    ttr <- min(rexp(simlist$k - simlist$i, simlist$lambda_r))
    if (ttf < ttr)
    {
      tte <- ttf
      etype <- 1
    }
    else 
    {
      tte <- ttr
      etype <- 2
    }
    simlist$time[simlist$i] <- simlist$time[simlist$i] + tte
  }

  schedevnt(simlist$currtime + tte, etype, simlist)
  simlist$totaltime <- simlist$totaltime + tte

  #print("----------")
  #print(simlist$i)
  #print("next event")
  #print(etype)
  #print(simlist$currtime)

}
