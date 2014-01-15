evntreacht <- function(evnt, simlist){
	etype = event[2]
	if (etype == 1) { #machine breakdown
		simlist$machineup = simlist$machineup - 1 }
	else if (type == 2) { #machine repair
		simlist$machineup = simlist$machineup + 1 }
		
		timeofnextbreak = simlist$currtime + min(rexp(simlist$machineup,simlist$breakrate))
		if(simlist$machineup < simlist$maxmachine) #some machines are broken
		{ timetorepair = simlist$currtime + min(rexp(simlist$maxmacine - simlist$machineup,simlist$repairrate))
		timeofnextevnt = min(timeofnextbreak, timetorepair)
		#determine which event occurs next
		if (timeofnextevent == timeofnextbreak) eventtype = 1  
		else eventtype = 2 }
		else { timeofnextevnt = timeofnextbreak; eventtype = 1}
		
		jobnum = simlist$jobnum + 1
		simlist$jobnum = jobnum
		schedevnt(timeofnextevent,eventtype,simlist,c(timeofnextevent,jobnum))
}
