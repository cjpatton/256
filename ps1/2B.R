
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


# Nurse Problem
# Parameters
# n :: limit on active nurses
# q :: limit on queue size
# p :: timeout time
# d :: mean of call duration (exponential)
# r :: mean of call arrival (uniform? [0, 2r])
# (stated in problem that there is an issue with exponential arrival, but I don't see why)
# (Is it that it doesn't make sense as a policy if arrival time is exponential?)


cc2b <- function(d, r, n, q, p, timelim, dbg=F) {
  # Event types: 
  # 1 - call arrived
  # 2 - call ended
  # 3 - timeout

  # Set up simulation list, specify event handler. 
  simlist <- newsim(dbg)
  simlist$reactevent <- cc2breact
  
  # Parameters required by factoryreact()
  simlist$lambda_d = 1/d
  simlist$r = r #for uniform
  simlist$lambda_r = 1/r #for exp

  simlist$n <- n # max active nurses
  simlist$q <- q # max queue size
  simlist$p <- p

  # initial conditions : one active nurse, no calls in queue
  simlist$i_n <- 1 # active nurse
  simlist$i_i <- 1 # idle nurses
  simlist$i_q <- 0 # queued calls




  # We must generate the first event and handle it. 
  # Since the simulation starts the queue will be empty,
  # the first event will be a call arriving
  tta <- runif(1, 0, 2*r)

  # tta <- rexp(1, 1/r) # for exp

  ## Is this necessary? Couldn't you just use the timelim passed to the system?
  ## Ohhh, right, it's used to tally up the total time each system exists. I can use that.

  simlist$lasttime <- 0.0
  simlist$activeTime <- 0.0
  simlist$idleTime <- 0.0
  schedevnt(tta, 1, simlist)

  # Flag for timeout
  simlist$reset <- F
  simlist$nextTimeout <- p

  # The way I'm configuring this, there is also a running timeout event.
  # This will just continually run, setting the next timeout to the value above.
  # so I need to start this event as well.

  if(p > 0){ # only start it if there is a positive timeout value! Otherwise, ignore.
  	schedevnt(p, 3, simlist)
  }
  # Running totals for dropped calls
  simlist$rej <- 0
  simlist$tot <- 0

  # Enter main loop (calls factoryreact()). 
  mainloop(simlist, timelim)

  # Report average number of machines running. 
  simlist$time <- simlist$idleTime / simlist$activeTime
  proRej <- simlist$rej / simlist$tot	
  print("Proportion of calls rejected")
  print(proRej)
  print("Proportion of nurse idle time")
  print(simlist$time)
}

# Our reactevent(). Transition to new state 
# and generate next event. 
cc2breact <- function(evnt, simlist) {
  etype <- evnt[2] 
  
  # Transition state. 

  if (etype == 1){ # call arrived

	  simlist$reset <- T
    simlist$nextTimeout <- simlist$currtime + simlist$p
	  simlist$tot <- simlist$tot + 1

    print ("-----Call arrived at time-----")
    print (evnt[1])

	  if(simlist$i_i > 0){ # if idle nurses to take call (queue empty)
		
  		# active time calculations

  		# Essentially, I want to do this calculation whenever the idle or active 
  		# list changes, for any reason. $lasttime will be set to the last time it changed.

  		delta <- simlist$currtime - simlist$lasttime
  		
  		simlist$activeTime <- simlist$activeTime + (simlist$i_n * delta)
  		simlist$idleTime <- simlist$idleTime + (simlist$i_i * delta)
  		simlist$lasttime <- simlist$currtime

  		# Now that that's sorted, we can move on

  		simlist$i_i <- simlist$i_i - 1

  		# new event: call ended
  		tte <- rexp(1, simlist$lambda_d)
  		schedevnt(simlist$currtime + tte, 2, simlist)
  		
      print ("an idle nurse takes the call")
		
	  } else if(simlist$i_q < simlist$q){ # if queue not full, no nurses to take call
		
		    simlist$i_q <- simlist$i_q + 1
      
      print ("no nurse call take the call, queue is not full")
	  } else { # queue full, call dropped, new active nurse
    
		  simlist$rej <- simlist$rej + 1

		  if(simlist$i_n < simlist$n){ # if nurse limit not reached

  			# active time calculations

  			delta <- simlist$currtime - simlist$lasttime
  		
  			simlist$activeTime <- simlist$activeTime + (simlist$i_n * delta)
  			simlist$idleTime <- simlist$idleTime + (simlist$i_i * delta)
  			simlist$lasttime <- simlist$currtime


  			# Add one nurse, take call from queue
  			simlist$i_n = simlist$i_n + 1
  			simlist$i_q = simlist$i_q - 1

  			# new event: call ended
  			tte <- rexp(1, simlist$lambda_d)
  			schedevnt(simlist$currtime + tte, 2, simlist)
  		}

      print ("queue is full, grab new nurse, drop top call")
    }

    print ("Stats so far:")
    print ("total calls")
    print (simlist$tot)
    print ("total rejected calls")
    print (simlist$rej)
    print ("active nurses")
    print (simlist$i_n)
    print ("idle nurses")
    print (simlist$i_i)
    print ("active nurse time")
    print (simlist$activeTime)
    print ("idle nurse time")
    print (simlist$idleTime)

  	# new event: call arrival
  	tta <- runif(1, 0, 2*simlist$r)
  	schedevnt(simlist$currtime + tta, 1, simlist)

  } else if (etype == 2) { # call ended

    print ("-----Call ended at time-----")
    print (evnt[1])

  	if(simlist$i_q > 0){ # calls in queue

  		simlist$i_q <- simlist$i_q - 1
  		
   		# new event: call ended
  		tte <- rexp(1, simlist$lambda_d)
  		schedevnt(simlist$currtime + tte, 2, simlist)

      print ("there are calls in queue ")

  	} else { # queue empty, new idle nurse

  		# active time calculations

  		delta <- simlist$currtime - simlist$lasttime
  		
  		simlist$activeTime <- simlist$activeTime + (simlist$i_n * delta)
  		simlist$idleTime <- simlist$idleTime + (simlist$i_i * delta)
  		simlist$lasttime <- simlist$currtime

  		# new idle nurse

  		simlist$i_i <- simlist$i_i + 1

  		if(simlist$p <= 0 && simlist$i_n > 1){ # if no timeout value and more than 1 active nurse
  			simlist$i_i <- simlist$i_i - 1
  			simlist$i_n <- simlist$i_n - 1
  		}

      print("queue is empty - new idle nurse")
	  }

    print ("Stats so far:")
    print ("total calls")
    print (simlist$tot)
    print ("total rejected calls")
    print (simlist$rej)
    print ("active nurses")
    print (simlist$i_n)
    print ("idle nurses")
    print (simlist$i_i)
    print ("active nurse time")
    print (simlist$activeTime)
    print ("idle nurse time")
    print (simlist$idleTime)
  } else if (etype == 3) { # timeout
    
    print ("-----Timeout happened at time-----")
    print (evnt[1])

  	if(simlist$reset) {

      print ("previously arrived call has reset this arrival")
  		# timeout has been reset by a call arriving
  		simlist$reset <- F
  	} else if (simlist$i_n == 1) {
  		# only one nurse left
  		# Reset timeout
  		simlist$nextTimeout <- simlist$currtime + simlist$p

      print ("only one nurse is left, reset timeout") #probably don't even need to reset it, the next call that arrives will reset it
  	} else if (simlist$i_i > 0) { # Some idle nurses ( >1)
  		# active time calculations	

  		delta <- simlist$currtime - simlist$lasttime
  		
  		simlist$activeTime <- simlist$activeTime + (simlist$i_n * delta)
  		simlist$idleTime <- simlist$idleTime + (simlist$i_i * delta)
  		simlist$lasttime <- simlist$currtime

  		# remove nurses from active pool

  		simlist$i_n <- simlist$i_n - 1
  		simlist$i_i <- simlist$i_i - 1

  		# reset timeout
  		simlist$nextTimeout <- simlist$currtime + simlist$p
      print ("there are idle nurses, one of them is sent to inactive")
  	} else { # no idle nurses
  		simlist$nextTimeout <- simlist$currtime + simlist$p
      print ("no idle nurses - reset timer?") # is this correct logic? what if there are active nurses that are on the phone and when the next call is finished, a nurse should be sent home?
  	}
  	# new event : next timeout

  	schedevnt(simlist$nextTimeout, 3, simlist)

    print ("Stats so far:")
    print ("total calls")
    print (simlist$tot)
    print ("total rejected calls")
    print (simlist$rej)
    print ("active nurses")
    print (simlist$i_n)
    print ("idle nurses")
    print (simlist$i_i)
    print ("active nurse time")
    print (simlist$activeTime)
    print ("idle nurse time")
    print (simlist$idleTime)

  }


  # Okay, here's where my design is different, with DES.
  # Yes, I know the memoryless property says you can schedule things
  # such that there is exactly one event rolling. But honestly,
  # that just obscures intuition to me. The intuition is that all
  # of these things run in parallel, so I schedule multiple events
  # at once. Because of the memoryless property, this is equivalent
  # to scheduling them sequentially.

}
