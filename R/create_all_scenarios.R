#' Creates specific scenarios for Accra and Sao Paulo
#'
#' Creates five prespecified scenarios from the baseline for Accra and Sao Paulo
#'
#' @param trip_set data frame of baseline trips
#'
#' @return list of scenarios
#'
#' @export
create_all_scenarios <- function(trip_set){

  # Default city is set to accra
  #----
  if(CITY == 'accra'){

    ###############################################################
    rdr <- trip_set
    tt <- length(unique(rdr$trip_id))
    rd_list <- list()
    rd_list[[1]] <- rdr
    # Scenario 1
    source_modes <- c('bus', 'walking')
    target_modes <- c('car')
    source_percentages <- round(c(0.16, 0.49)* tt)
    rdr <- create_scenario(rdr, scen_name = 'Scenario 1', source_modes = source_modes,
                           target_modes = target_modes, source_distance_cats = DIST_CAT,
                           source_trips = source_percentages)
    rd_list[[2]] <- rdr
    ###############################################################
    # Scenario 2
    rdr <- rd_list[[2]]
    # 35 % of all trips are bus.
    # These come from car and taxi.
    # All car and taxi trips > 6 km go to bus. Then 35 car and taxi trips 0--6 km go to bus.
    source_modes <- c('car', 'taxi')
    target_modes <- c('bus')
    target_new_trips <- round(0.35 * tt - sum(rdr$trip_mode=='bus'))
    total_car_trips <- filter(rdr, trip_mode %in% source_modes)
    long_trips <- sum(total_car_trips$trip_distance_cat!=DIST_CAT[1])
    long_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T,
                                             target_modes = target_modes, source_distance_cats = DIST_CAT[2:3],source_trips = long_trips)
    short_trips <- min( target_new_trips - long_trips, sum(total_car_trips$trip_distance_cat==DIST_CAT[1]))
    if(short_trips>0){
      short_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T,
                                                target_modes = target_modes, source_distance_cats = DIST_CAT[1],source_trips = short_trips)
      long_car_trips_sample <- rbind(long_car_trips_sample, short_car_trips_sample)
    }
    bus_trips <- long_car_trips_sample

    # Update selected rows for mode and duration
    rdr$trip_mode[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_mode
    rdr$trip_distance[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_distance
    rdr$stage_mode[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_mode
    rdr$stage_distance[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_distance
    rdr$stage_duration[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_duration
    rdr$trip_distance_cat[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_distance_cat

    rdr$scenario <- "Scenario 2"
    rd_list[[3]] <- rdr
    ###############################################################
    # Scenario 3
    rdr <- rd_list[[2]]
    # 16 % bus remain as is
    # 10 % Mcycle increase
    # x decrease car
    source_modes <- c('car')
    target_modes <- c('motorcycle')
    target_new_trips <- max(round(0.1 * tt) - sum(rdr$trip_mode=='motorcycle'),1)
    mcycle_trips_sample <- create_scenario(rdr, scen_name = 'Scenario 3', source_modes = source_modes,
                                           combined_modes = T, target_modes = target_modes,
                                           source_distance_cats = DIST_CAT, source_trips = target_new_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$trip_mode
    rdr$trip_distance[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$trip_distance
    rdr$stage_mode[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_mode
    rdr$stage_distance[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_distance
    rdr$stage_duration[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_duration
    rdr$scenario <- "Scenario 3"
    rd_list[[4]] <- rdr
    #return(rd_list)
    ###############################################################
    # Scenario 4
    rdr <- rd_list[[2]]
    # 3.5 % Cycle
    source_modes <- c('motorcycle', 'car', 'taxi')
    target_modes <- c('bicycle')
    mtrips <- max(min(52,sum(rdr$trip_mode == 'motorcycle')),1)
    btrips <- sum(rdr$trip_mode == 'bicycle')
    ctrips <- max(min(round(0.035 * tt) - btrips - mtrips, sum(rdr$trip_mode %in% c('car', 'taxi')&rdr$trip_distance_cat==DIST_CAT[1])),1)
    target_new_trips <- c(mtrips, ctrips)
    mbike_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes[1],combined_modes = T,
                                   target_modes = target_modes,source_distance_cats = DIST_CAT,source_trips = target_new_trips[1])
    car_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = c(source_modes[2], source_modes[3]),combined_modes = T,
                                 target_modes = target_modes,source_distance_cats = DIST_CAT[1],source_trips = target_new_trips[2])
    car_mbike_trips <- rbind(mbike_trips, car_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$trip_mode
    rdr$trip_distance[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$trip_distance
    rdr$stage_mode[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_mode
    rdr$stage_distance[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_distance
    rdr$stage_duration[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_duration
    rdr$scenario <- "Scenario 4"
    rd_list[[5]] <- rdr
    ###############################################################
    # Scenario 5
    rdr <- rd_list[[2]]
    # 3.5 % Cycle
    source_modes <- c('car', 'taxi')
    target_modes <- c('walking')
    target_new_trips <- min(round(0.54 * tt) - sum(rdr$trip_mode == target_modes), sum(rdr$trip_mode%in%source_modes&rdr$trip_distance_cat==DIST_CAT[1]))
    motorised_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes, combined_modes = T,
                                       target_modes = target_modes,source_distance_cats = DIST_CAT[1],source_trips = target_new_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$trip_mode
    rdr$trip_distance[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$trip_distance
    rdr$stage_mode[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_mode
    rdr$stage_distance[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_distance
    rdr$stage_duration[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_duration
    rdr$scenario <- "Scenario 5"
    rd_list[[6]] <- rdr


    return(rd_list)
  }
  #----
  if(CITY == 'sao_paulo'){

    ###############################################################
    rdr <- trip_set
    tt <- length(unique(rdr$trip_id))
    rd_list <- list()
    rd_list[[1]] <- rdr
    # Scenario 1
    source_modes <- c('bus', 'walking')
    target_modes <- c('car')
    source_percentages <- round(c(0.15, 0.20)* tt)
    rdr <- create_scenario(rdr, scen_name = 'Scenario 1', source_modes = source_modes,
                           target_modes = target_modes, source_distance_cats = DIST_CAT,
                           source_trips = source_percentages)
    rd_list[[2]] <- rdr
    ###############################################################
    # Scenario 2
    rdr <- rd_list[[2]]
    # 35 % of all trips are bus.
    # These come from car and taxi.
    # All car and taxi trips > 6 km go to bus. Then 35 car and taxi trips 0--6 km go to bus.
    source_modes <- c('car', 'taxi')
    target_modes <- c('bus')
    target_new_trips <- round(0.35 * tt - sum(rdr$trip_mode=='bus'))
    total_car_trips <- filter(rdr, trip_mode %in% source_modes)
    long_trips <- sum(total_car_trips$trip_distance_cat!=DIST_CAT[1])
    long_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T,
                                             target_modes = target_modes, source_distance_cats = DIST_CAT[2:3],source_trips = long_trips)
    short_trips <- min( target_new_trips - long_trips, sum(total_car_trips$trip_distance_cat==DIST_CAT[1]))
    if(short_trips>0){
      short_car_trips_sample <- create_scenario(total_car_trips, scen_name = 'Scenario 2', source_modes = source_modes, combined_modes = T,
                                                target_modes = target_modes, source_distance_cats = DIST_CAT[1],source_trips = short_trips)
      long_car_trips_sample <- rbind(long_car_trips_sample, short_car_trips_sample)
    }
    bus_trips <- long_car_trips_sample

    # Update selected rows for mode and duration
    rdr$trip_mode[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_mode
    rdr$trip_distance[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_distance
    rdr$stage_mode[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_mode
    rdr$stage_distance[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_distance
    rdr$stage_duration[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$stage_duration
    rdr$trip_distance_cat[match(bus_trips$trip_id,rdr$trip_id)] <- bus_trips$trip_distance_cat

    rdr$scenario <- "Scenario 2"
    rd_list[[3]] <- rdr

    ###############################################################
    # Scenario 3
    rdr <- rd_list[[2]]
    # 16 % bus remain as is
    # 10 % Mcycle increase
    # x decrease car
    source_modes <- c('car')
    target_modes <- c('motorcycle')
    target_new_trips <- max(round(0.1 * tt) - sum(rdr$trip_mode=='motorcycle'),1)
    mcycle_trips_sample <- create_scenario(rdr, scen_name = 'Scenario 3', source_modes = source_modes,
                                           combined_modes = T, target_modes = target_modes,
                                           source_distance_cats = DIST_CAT, source_trips = target_new_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$trip_mode
    rdr$trip_distance[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$trip_distance
    rdr$stage_mode[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_mode
    rdr$stage_distance[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_distance
    rdr$stage_duration[match(mcycle_trips_sample$trip_id,rdr$trip_id)] <- mcycle_trips_sample$stage_duration
    rdr$scenario <- "Scenario 3"
    rd_list[[4]] <- rdr


    ###############################################################
    # Scenario 4
    rdr <- rd_list[[2]]
    # 3.5 % Cycle
    source_modes <- c('motorcycle', 'car', 'taxi')
    target_modes <- c('bicycle')
    mtrips <- max(min(52,sum(rdr$trip_mode == 'motorcycle')),1)
    btrips <- sum(rdr$trip_mode == 'bicycle')
    ctrips <- max(min(round(0.035 * tt) - btrips - mtrips, sum(rdr$trip_mode %in% c('car', 'taxi')&rdr$trip_distance_cat==DIST_CAT[1])),1)
    target_new_trips <- c(mtrips, ctrips)
    mbike_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes[1],combined_modes = T,
                                   target_modes = target_modes,source_distance_cats = DIST_CAT,source_trips = target_new_trips[1])
    car_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = c(source_modes[2], source_modes[3]),combined_modes = T,
                                 target_modes = target_modes,source_distance_cats = DIST_CAT[1],source_trips = target_new_trips[2])
    car_mbike_trips <- rbind(mbike_trips, car_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$trip_mode
    rdr$trip_distance[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$trip_distance
    rdr$stage_mode[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_mode
    rdr$stage_distance[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_distance
    rdr$stage_duration[match(car_mbike_trips$trip_id,rdr$trip_id)] <- car_mbike_trips$stage_duration
    rdr$scenario <- "Scenario 4"
    rd_list[[5]] <- rdr
    ###############################################################
    # Scenario 5
    rdr <- rd_list[[2]]
    # 3.5 % Cycle
    source_modes <- c('car', 'taxi')
    target_modes <- c('walking')
    target_new_trips <- min(round(0.54 * tt) - sum(rdr$trip_mode == target_modes), sum(rdr$trip_mode%in%source_modes&rdr$trip_distance_cat==DIST_CAT[1]))
    motorised_trips <- create_scenario(rdr, scen_name = 'Scenario 4', source_modes = source_modes, combined_modes = T,
                                       target_modes = target_modes,source_distance_cats = DIST_CAT[1],source_trips = target_new_trips)
    # Update selected rows for mode and duration
    rdr$trip_mode[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$trip_mode
    rdr$trip_distance[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$trip_distance
    rdr$stage_mode[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_mode
    rdr$stage_distance[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_distance
    rdr$stage_duration[match(motorised_trips$trip_id,rdr$trip_id)] <- motorised_trips$stage_duration
    rdr$scenario <- "Scenario 5"
    rd_list[[6]] <- rdr

    return(rd_list)
  }

  #----
  if(CITY == 'bogota'){

    ###############################################################
    # Scenario 1: Ciudad expandida y dependiente del carro
    rdr <- trip_set
    # tt <- length(unique(rdr$trip_id))
    tt <- nrow(trip_set) # Total number of trips
    rd_list <- list()
    rd_list[[1]] <- rdr # Baseline

    # New distribution of trips
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.0595, 0.2857, 0.1993, 0.0664, 0.0414, 0.3477))
    # Difference of trips between baseline and scenario 1
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Redistribution of Walking trips to Bicycle on short distances (0-5km)
    source_modes <- c('walking')
    target_modes <- c('bicycle')
    w2b_trips <- create_scenario(rdr, scen_name = 'Scenario 1',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT[1],
                                 source_trips = diff_trips[1])

    # Redistribution of remaining Walking trips to motorcycle on medium to long
    # distances (6-10km or 11+ km)
    source_modes <- c('walking')
    target_modes <- c('motorcycle')
    remaining_w <- (diff_trips[6]+diff_trips[1])*-1
    w2m_trips <- create_scenario(rdr, scen_name = 'Scenario 1',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT[-1],
                                 source_trips = remaining_w)

    # Redistribution of Taxi trips to motorcycle
    source_modes <- c('taxi')
    target_modes <- c('motorcycle')
    t2m_trips <- create_scenario(rdr, scen_name = 'Scenario 1',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = diff_trips[5]*-1)

    # Redistribution of Bus trips to motorcycle
    source_modes <- c('bus')
    target_modes <- c('motorcycle')
    mtripsleft <- diff_trips[4] - (remaining_w + (diff_trips[5]*-1))
    b2m_trips <- create_scenario(rdr, scen_name = 'Scenario 1',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = mtripsleft)

    # Redistribution of Bus trips to car
    source_modes <- c('bus')
    target_modes <- c('car')
    remaining_b <- (diff_trips[2]+mtripsleft)*-1
    # Bus trips shouldnt be the same as those chosen to be motorcycle
    rdr2 <- rdr[-match(b2m_trips$id,rdr$id),]
    b2c_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = remaining_b)

    redistribute_trips <- rbind(w2b_trips, w2m_trips, t2m_trips, b2m_trips, b2c_trips)

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    rdr$scenario <- "Scenario 1"
    rdr$id <- NULL
    rd_list[[2]] <- rdr

    # ###############################################################
    # Scenario 2: Ciudad densa y transporte publico
    rdr <- trip_set
    # tt <- length(unique(rdr$trip_id))
    tt <- nrow(trip_set) # Total number of trips

    # New distribution of trips
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.066054, 0.422773, 0.068210, 0.028398,
                              0.028875, 0.385689))
    # Difference of trips between baseline and scenario 1
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # First attempt
    # # Redistribution of Taxi trips to Walking
    # # I tried on short distances (0-5km) but there aren't enough trips to replace
    # source_modes <- c('taxi')
    # target_modes <- c('walking')
    # t2w_trips <- create_scenario(rdr, scen_name = 'Scenario 2',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT,
    #                              source_trips = diff_trips[5]*-1)
    #
    # # Redistribution of Motorcycle trips to Walking
    # # I tried on short distances (0-5km) but there aren't enough trips to replace
    # source_modes <- c('motorcycle')
    # target_modes <- c('walking')
    # m2w_trips <- create_scenario(rdr, scen_name = 'Scenario 2',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT,
    #                              source_trips = diff_trips[4]*-1)
    #
    # # Redistribution of Car trips to Bus on medium to long
    # # distances (6-10km or 11+ km)
    # source_modes <- c('car')
    # target_modes <- c('bus')
    # c2b_trips <- create_scenario(rdr, scen_name = 'Scenario 2',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[-1],
    #                              source_trips = diff_trips[2])
    #
    # # Redistribution of Car trips to Bicycle on short distances (0-5km)
    # source_modes <- c('car')
    # target_modes <- c('bicycle')
    # c2bb_trips <- create_scenario(rdr, scen_name = 'Scenario 2',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[1],
    #                              source_trips = diff_trips[1])
    #
    #
    # # Redistribution of Car trips to Walking on short distances (0-5km)
    # source_modes <- c('car')
    # target_modes <- c('walking')
    # remaining_c <- (diff_trips[3]+ diff_trips[2] + diff_trips[1])
    # # Bus trips shouldnt be the same as those chosen to be motorcycle
    # rdr2 <- rdr[-match(c2bb_trips$id,rdr$id),]
    # c2w_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[1],
    #                              source_trips = remaining_c)
    #
    # redistribute_trips <- rbind(t2w_trips, m2w_trips, c2b_trips, c2bb_trips, c2w_trips)
    #
    # # Update selected rows for mode and duration
    # rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    # rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    # rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    # rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    # rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    # rdr$scenario <- "Scenario 1"
    # rdr$id <- NULL
    # rd_list[[3]] <- rdr

    # Second attempt
    # Redistribution of Taxi trips to Bus
    source_modes <- c('taxi')
    target_modes <- c('bus')
    t2b_trips <- create_scenario(rdr, scen_name = 'Scenario 2',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = diff_trips[5]*-1)

    # Redistribution of Motorcycle trips to Bus
    source_modes <- c('motorcycle')
    target_modes <- c('bus')
    m2b_trips <- create_scenario(rdr, scen_name = 'Scenario 2',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = diff_trips[4]*-1)

    # Redistribution of Car trips to Walking on short to medium distances
    # (0-5km, 6-10km)
    source_modes <- c('car')
    target_modes <- c('walking')
    c2w_trips <- create_scenario(rdr, scen_name = 'Scenario 2',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT[-3],
                                 source_trips = diff_trips[6])

    # Redistribution of Car trips to Bicycle
    source_modes <- c('car')
    target_modes <- c('bicycle')
    # Bicycle trips shouldnt be the same as those chosen to be walking
    rdr2 <- rdr[-match(c2w_trips$id,rdr$id),]
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = diff_trips[1])

    # Redistribution of Car trips to Bus
    source_modes <- c('car')
    target_modes <- c('bus')
    remaining_c <- (diff_trips[3]+ diff_trips[6] + diff_trips[1])*-1
    # Bus trips shouldnt be the same as those chosen to be walking or cycling
    rdr3 <- rdr2[-match(c2bb_trips$id,rdr2$id),]
    c2b_trips <- create_scenario(rdr3, scen_name = 'Scenario 2',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = remaining_c)

    redistribute_trips <- rbind(t2b_trips, m2b_trips, c2w_trips, c2bb_trips, c2b_trips)

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    rdr$scenario <- "Scenario 2"
    rdr$id <- NULL
    rd_list[[3]] <- rdr

    # ###############################################################
    # Scenario 3: Ciudad compartida
    rdr <- trip_set
    # tt <- length(unique(rdr$trip_id))
    tt <- nrow(trip_set) # Total number of trips

    # New distribution of trips
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.068633343549923, 0.403478865465086,
                              0.103079628556659, 0.036028743699033,
                              0.033239150814647, 0.355540267914652))
    # Difference of trips between baseline and scenario 1
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Redistribution of Taxi trips to Walk on short distances
    source_modes <- c('taxi')
    target_modes <- c('walking')
    t2w_trips <- create_scenario(rdr, scen_name = 'Scenario 3',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT[1],
                                 source_trips = diff_trips[6])

    # Redistribution of Taxi trips to Bicycle
    source_modes <- c('taxi')
    target_modes <- c('bicycle')
    remaining_t <- (diff_trips[5]+ diff_trips[6])*-1
    # Bicycle trips shouldnt be the same as those chosen to be walking
    rdr2 <- rdr[-match(t2w_trips$id,rdr$id),]
    t2b_trips <- create_scenario(rdr2, scen_name = 'Scenario 3',
                                   source_modes = source_modes, combined_modes = T,
                                   target_modes = target_modes,
                                   source_distance_cats = DIST_CAT,
                                   source_trips = remaining_t)

    # Redistribution of Motorcycle trips to Bus
    source_modes <- c('motorcycle')
    target_modes <- c('bus')
    m2b_trips <- create_scenario(rdr, scen_name = 'Scenario 3',
                                   source_modes = source_modes, combined_modes = T,
                                   target_modes = target_modes,
                                   source_distance_cats = DIST_CAT,
                                   source_trips = diff_trips[4]*-1)

    # Redistribution of Car trips to Bicycle
    source_modes <- c('car')
    target_modes <- c('bicycle')
    remaining_b <- diff_trips[1] - remaining_t
    c2bb_trips <- create_scenario(rdr, scen_name = 'Scenario 3',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = remaining_b)

    # Redistribution of Car trips to Bus
    source_modes <- c('car')
    target_modes <- c('bus')
    remaining_c <- (diff_trips[3] + remaining_b)*-1
    # bus trips shouldnt be the same as those chosen to be bicycle
    rdr2 <- rdr[-match(c2bb_trips$id,rdr$id),]
    c2b_trips <- create_scenario(rdr2, scen_name = 'Scenario 3',
                                 source_modes = source_modes, combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT,
                                 source_trips = remaining_c)

    redistribute_trips <- rbind(t2w_trips, t2b_trips, m2b_trips, c2bb_trips, c2b_trips)

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    rdr$scenario <- "Scenario 3"
    rdr$id <- NULL
    rd_list[[4]] <- rdr

    return(rd_list)
  }
}
