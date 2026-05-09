simulacia_GGnm <- function(arrivalsdist, servicedist, n, m, max_time) {
  time <- 0
  next_arrival <- arrivalsdist()
  next_departures <- c(Inf)
  queue <- 0
  servers <- 0
  
  log <- data.frame(time = numeric(), event = character(), queue = integer(), server = integer())
  
  while (time < max_time) {
    if (next_arrival < next_departures[1]) {
      # Príchod zákazníka
      time <- next_arrival
      next_arrival <- time + arrivalsdist()
      if (servers ==0) {
      next_departures <- next_departures[!is.infinite(next_departures)]
      }
      if (servers < n ) {
        servers <- servers + 1
        next_departures <- append(next_departures, time + servicedist()) 
        next_departures <- sort(next_departures)
        log <- rbind(log, data.frame(time, event = "arrival", queue, server = servers))
      }
      else if (queue < m-n){
        queue <- queue + 1
        log <- rbind(log, data.frame(time, event = "arrival", queue, server = servers))
      }
      else {
        log <- rbind(log, data.frame(time, event = "rejection", queue, server = servers))
      }
    }
    else if (next_arrival > next_departures[1]) {
      # Odchod zákazníka
      time <- next_departures[1]
      next_departures <- next_departures[-1]
      
      if (queue > 0) {
        queue <- queue - 1
        next_departures <- append(next_departures, time + servicedist())
        next_departures <- sort(next_departures)
      } 
      else {
        if (servers > 1){
          servers <- servers - 1
        }
        else {
          servers <- servers - 1
          next_departures <- append(next_departures, Inf)
        }
      }
      log <- rbind(log, data.frame(time, event = "departure", queue, server = servers))
    }
    else if (next_arrival == next_departures[1]) {
      # Nepravdepodobná remíza
      time <- next_arrival
      next_arrival <- time + arrivalsdist()
      next_departures <- next_departures[-1]
      if (queue > 0) {
        next_departures <- append(next_departures, time + servicedist())
        next_departures <- sort(next_departures)
      } 
      else {
        if (servers > 1){
          servers <- servers - 1
        }
        else {
          servers <- servers - 1
          next_departures <- append(next_departures, Inf)
        }
      }
      log <- rbind(log, data.frame(time, event = "departure", queue, server = servers))
      if (servers ==0) {
        next_departures <- next_departures[!is.infinite(next_departures)]
      }
      if (servers < n ) {
        servers <- servers + 1
        next_departures <- append(next_departures, time + servicedist()) 
        next_departures <- sort(next_departures)
        log <- rbind(log, data.frame(time, event = "arrival", queue, server = servers))
      }
      else if (queue < m-n){
        queue <- queue + 1
        log <- rbind(log, data.frame(time, event = "arrival", queue, server = servers))
      }
      else {
        log <- rbind(log, data.frame(time, event = "rejection", queue, server = servers))
      }
    }
  }
  return(log)
}

charakteristiky_simulacie <- function(arrivalsdist, servicedist, n, m, max_time, reps){
  results <- data.frame(
    system_avg = numeric(reps),
    queue_avg = numeric(reps),
    servers_avg = numeric(reps),
    P_rejection = numeric(reps),
    P_wait = numeric(reps),
    P_immediate = numeric(reps),
    queue_wait_avg = numeric(reps),
    avg_time_in_system = numeric(reps))
  for (r in 1:reps) {
    log <- simulacia_GGnm(arrivalsdist, servicedist, n, m, max_time)
    log$dt <- c(diff(log$time), 0)
    duration <- max(log$time)
    system_avg  <- sum(log$dt * (log$queue + log$server)) / duration
    queue_avg <- sum(log$dt * log$queue) / duration 
    servers_avg <- sum(log$dt * log$server) / duration
    number_attempts  <- sum(log$event %in% c("arrival", "rejection"))
    number_rejected  <- sum(log$event == "rejection")
    number_entered   <- sum(log$event == "arrival")
    number_immediate <- sum(log$event == "arrival" & log$queue == 0) 
    number_waited    <- sum(log$event == "arrival" & log$queue > 0)  
    P_rejection <- number_rejected  / number_attempts
    P_immediate <- number_immediate / number_attempts
    P_wait      <- number_waited    / number_attempts
    attempt_times <- log$time[log$event %in% c("arrival", "rejection")]
    interarrival_avg <- mean(diff(attempt_times))
    servicetime_avg <- mean(replicate(10000, servicedist())) 
    queue_join_times <- log$time[log$event == "arrival" & log$queue > 0]
    service_start_times <- log$time[log$event == "departure" & log$server == n]
    matched_length <- min(length(queue_join_times),length(service_start_times))
    queue_wait_times <- service_start_times[seq_len(matched_length)] - queue_join_times[seq_len(matched_length)]
    queue_wait_avg <- sum(queue_wait_times) / (number_entered - log$queue[nrow(log)])
    avg_time_in_system <- queue_wait_avg + servicetime_avg
    results[r, ] <- c(
      system_avg,
      queue_avg,
      servers_avg,
      P_rejection,
      P_wait,
      P_immediate,
      queue_wait_avg,
      avg_time_in_system)
  }
  final_results <- colMeans(results)
  cat("Priemerný počet zákazníkov v systéme:", final_results["system_avg"], "\n")
  cat("Priemerný počet zákazníkov vo fronte:", final_results["queue_avg"], "\n")
  cat("Priemerný počet obsadených liniek:", final_results["servers_avg"], "\n")
  cat("Pravdepodobnosť odmietnutia zákazníka:", final_results["P_rejection"], "\n")
  cat("Pravdepodobnosť, že zákazník bude čakať vo fronte:", final_results["P_wait"], "\n")
  cat("Pravdepodobnosť, že zákazník nebude čakať vo fronte a nebude odmietnutý:", final_results["P_immediate"], "\n")
  cat("Priemerná doba čakania vo fronte:", final_results["queue_wait_avg"], "\n")
  cat("Priemerná doba strávená v systéme:", final_results["avg_time_in_system"], "\n")
}
# Príklad využitia:
charakteristiky_simulacie(arrivalsdist = function() rexp(1, rate = 3),
                          servicedist = function() rexp(1, rate = 4),
                          n = 1, m = Inf, max_time = 1000, reps = 5)
