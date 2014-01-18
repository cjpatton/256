
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

#1.c
#event: curtime eventtype(1=fail,2=repair) machine_num(1,2) timeUp timeDown
#repairman: status(1=offsite,2=onsite) time_when_will_be_available

repairman <- function(u, r, c, timelim, dbg=F) {
  simlist <- newsim(dbg)
  simlist$reactevent <- repairmanreact
  simlist$lambda_u = 1/u
  simlist$lambda_r = 1/r
  simlist$c <- c #time it take for the repairman to get to the machines
  repairman <- c(1, 0) #offsite, available since time 0 [only matters for onsite]
  simlist$repairman <- repairman

  #start with both machines running. find time when each one will fail
  ttf1 <- rexp(1, simlist$lambda_u)
  ttf2 <- rexp(1, simlist$lambda_u)

  #schedule them (scheduer will sort them by whichever occurs first)
  schedevnt(ttf1, 1, simlist, c(1)) #c(1st machine)
  schedevnt(ttf2, 1, simlist, c(2)) 

  simlist$results = c(0,0,0) # times for 0 machines simlist$results[1], 1 machine: simlist$results[2], 2 machines working, simlist$results[3] 
  simlist$totaltime = 0
  simlist$lastnumofmachines = 2
  simlist$lasttimeup = 0
  simlist$lasttimedown = 0 
  #Enter main loop. event types and all logic happens via repairmanreact function
  mainloop(simlist, timelim) #note that the last even with time over the time limit with break the simulation and the stats for the other machine will not be complete(as it doesn't get to finish the last event)

  print("total time:")
  print (simlist$totaltime)
  print("pi for each state: ")
  print(simlist$results/simlist$totaltime)
}

repairmanreact <- function(evnt, simlist) {
  curtime <- evnt[1]
  etype <- evnt[2] 
  machnum <- evnt[3]


  if (etype == 1){ #machine breaks

    delta_uptime <- curtime - simlist$lasttimeup 
    if (simlist$lastnumofmachines == 2){
      simlist$lastnumofmachines = 1
      simlist$results[3] <- simlist$results[3] + delta_uptime
      simlist$totaltime <- simlist$totaltime + delta_uptime
    } else { #1 machine was up
      simlist$lastnumofmachines = 0
      simlist$results[2] <- simlist$results[2] + delta_uptime
      simlist$totaltime <- simlist$totaltime + delta_uptime
    }
        
    if (simlist$repairman[1]==1){ #remairman is offsite
      ttr <- rexp(1, simlist$lambda_r) #time till repair end
      waittime <- simlist$c

    } else{ #repairer is on site
      ttr <- rexp(1, simlist$lambda_r) #time till repair end
      waittime <- simlist$repairman[2]
    } 
    
    simlist$repairman <- c(2,ttr + waittime) #new waittime till repairman will be free
    schedevnt(curtime + ttr + waittime, 2, simlist, c(machnum))

    simlist$lasttimedown <- curtime

    print ("--------current time-----------")
    print (curtime)
    print ("---machine broke---")
    print (machnum)
    print ("time till fix: ")
    print (ttr + waittime)
    print ("stats - time in each state {0,1,2}")
    print (simlist$results)

  } else { #a machine is repaired

    delta_downtime <- curtime - simlist$lasttimedown
    
    if (simlist$lastnumofmachines == 0){ #[the downtime of this machine is how long we had 1 machines running] 
      simlist$lastnumofmachines = 1
      simlist$results[1] <- simlist$results[1] + delta_downtime
      simlist$totaltime <- simlist$totaltime + delta_downtime
    } else { #1 machine was up [the downtime of this machine is how long we had 1 machine running]
      simlist$lastnumofmachines = 2
      simlist$results[2] <- simlist$results[2] + delta_downtime
      simlist$totaltime <- simlist$totaltime + delta_downtime
    }

    if (curtime >= simlist$repairman[2]){ #repairman is free to go, the other machine is still up, because if it went down the repairman's availability time would have been updated
      simlist$repairman <- c(1,0)
    } else{
      #nothing changes for the repairman if he isn't free to go
    }
    ttf <- rexp(1, simlist$lambda_u) #time till next failure
    schedevnt(simlist$currtime + ttf, 1, simlist, c(machnum))

    simlist$lasttimeup <- curtime

    print ("------current time-------------")
    print (curtime)
    print ("---machine repaired---")
    print (machnum)
    print ("stats - time in each state {0,1,2}")
    print (simlist$results)

  }
}

######## end of problem 1c ####
