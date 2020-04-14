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
  if (CITY == 'bogota') {

    ###############################################################
    # Scenario 1: mujeres hicieran la misma proporcion de viajes que los hombres
    rd_list <- list()

    rdr_full <- trip_set

    rd_list[[1]] <- rdr_full # Baseline

    # names(rdr)
    # cbind(table(trip_set$trip_mode, trip_set$sex),
    #       prop.table(table(trip_set$trip_mode, trip_set$sex),margin = 2)) #Proportion of each sex
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)

    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$sex == "female",]

    # Total number of trips made by female
    tt <- nrow(rdr)

    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.08320987654321,
                              0.31483380816714,
                              0.12560303893637,
                              0.01912630579297,
                              0.05394112060779,
                              0.40328584995252))

    # Difference of trips between baseline and scenario 1
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(motivo %in% c(4,7,8,10)) &
              (age >= 16 & age <= 62) &
              (strptime(hora_inicio, "%H:%M") >= strptime("05:30", "%H:%M") &
              strptime(hora_inicio, "%H:%M") <= strptime("23:30", "%H:%M")) &
              (limitacion == 0))

    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('bicycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)

    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('bicycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
                                 source_modes = source_modes,
                                 combined_modes = T,
                                 target_modes = target_modes,
                                 source_distance_cats = DIST_CAT[-3],
                                 source_trips = diff_trips[3]*-1)

    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('bicycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)

    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('bicycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)

    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('walking')
    target_modes <- c('bicycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)

    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips,
                                t2bb_trips, w2bb_trips)

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration

    rdr_full <- rbind(rdr_full[rdr_full$sex == "male",], rdr[,-ncol(rdr)])
    rdr_full$scenario <- "Scenario 1"
    rd_list[[2]] <- rdr_full


    # ###############################################################
    # Scenario 2
    # Todos los estratos hacen la misma proporción de viajes que el estrato
    # que mas hace
    rdr_full <- trip_set

    # cbind(table(trip_set$trip_mode, trip_set$estrato),
    #       prop.table(table(trip_set$trip_mode, trip_set$estrato),margin = 2)) #Proportion of each sex
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)

    # Estrato 1
    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$estrato == 1,]

    # Total number of trips made by estrato 1
    tt <- nrow(rdr)

    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.06478094773632,
                              0.38925465333659,
                              0.03966512232789,
                              0.05405185727058,
                              0.01715028854751,
                              0.43509713078111))

    # Difference of trips between baseline and scenario 2
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(motivo %in% c(4,7,8,10)) &
              (age >= 16 & age <= 62) &
              (strptime(hora_inicio, "%H:%M") >= strptime("05:30", "%H:%M") &
              strptime(hora_inicio, "%H:%M") <= strptime("23:30", "%H:%M")) &
              (limitacion == 0))

    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('bicycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)

    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('bicycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[3]*-1)

    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('bicycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)

    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('bicycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)

    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('walking')
    target_modes <- c('bicycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)

    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips,
                                w2bb_trips)

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration

    # Save updated trips in other object
    rdr_1 <- rdr

    #####
    ### Estrato 3
    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$estrato == 3,]

    # Total number of trips made by estrato 3
    tt <- nrow(rdr)

    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.06475496198422,
                              0.31332403109605,
                              0.15126576871601,
                              0.04399578551699,
                              0.05225389412535,
                              0.37440555856138))

    # Difference of trips between baseline and scenario 2
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(motivo %in% c(4,7,8,10)) &
              (age >= 16 & age <= 62) &
              (strptime(hora_inicio, "%H:%M") >= strptime("05:30", "%H:%M") &
              strptime(hora_inicio, "%H:%M") <= strptime("23:30", "%H:%M")) &
              (limitacion == 0))

    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('bicycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)

    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('bicycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[3]*-1)

    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('bicycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)

    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('bicycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)

    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('walking')
    target_modes <- c('bicycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)

    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips,
                                w2bb_trips)

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration

    # Save updated trips in other object
    rdr_3 <- rdr

    ####
    # Estrato 4
    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$estrato == 4,]

    # Total number of trips made by estrato 4
    tt <- nrow(rdr)

    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.06473755047106,
                              0.26029609690444,
                              0.30282637954240,
                              0.01749663526245,
                              0.09185733512786,
                              0.26278600269179))

    # Difference of trips between baseline and scenario 2
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(motivo %in% c(4,7,8,10)) &
              (age >= 16 & age <= 62) &
              (strptime(hora_inicio, "%H:%M") >= strptime("05:30", "%H:%M") &
              strptime(hora_inicio, "%H:%M") <= strptime("23:30", "%H:%M")) &
              (limitacion == 0))

    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('bicycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)

    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('bicycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[3]*-1)

    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('bicycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)

    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('bicycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)

    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('walking')
    target_modes <- c('bicycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)

    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips,
                                w2bb_trips)

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration

    # Save updated trips in other object
    rdr_4 <- rdr

    #####
    # Estrato 5
    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$estrato == 5,]

    # Total number of trips made by estrato 5
    tt <- nrow(rdr)

    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.06467926537131,
                              0.17913228639872,
                              0.42134681927070,
                              0.01916422677668,
                              0.09049773755656,
                              0.22517966462603))

    # Difference of trips between baseline and scenario 2
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(motivo %in% c(4,7,8,10)) &
              (age >= 16 & age <= 62) &
              (strptime(hora_inicio, "%H:%M") >= strptime("05:30", "%H:%M") &
              strptime(hora_inicio, "%H:%M") <= strptime("23:30", "%H:%M")) &
              (limitacion == 0))

    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('bicycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)

    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('bicycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[3]*-1)

    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('bicycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)

    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('bicycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)

    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('walking')
    target_modes <- c('bicycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)

    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips,
                                w2bb_trips)

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration

    # Save updated trips in other object
    rdr_5 <- rdr

    # Estrato 6
    # subset of trips that are going to be changed
    rdr <- trip_set[trip_set$estrato == 6,]

    # Total number of trips made by estrato 6
    tt <- nrow(rdr)

    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.06473489519112,
                              0.10789149198520,
                              0.44358816276202,
                              0.01202219482121,
                              0.08477188655980,
                              0.28699136868064))

    # Difference of trips between baseline and scenario 2
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(motivo %in% c(4,7,8,10)) &
              (age >= 16 & age <= 62) &
              (strptime(hora_inicio, "%H:%M") >= strptime("05:30", "%H:%M") &
              strptime(hora_inicio, "%H:%M") <= strptime("23:30", "%H:%M")) &
              (limitacion == 0))

    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('bicycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[2]*-1)

    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('bicycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[3]*-1)

    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('bicycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)

    # Redistribution of taxi trips to bicycle on any distance
    source_modes <- c('taxi')
    target_modes <- c('bicycle')
    t2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[5]*-1)

    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('walking')
    target_modes <- c('bicycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)

    redistribute_trips <- rbind(b2bb_trips, c2bb_trips, m2bb_trips, t2bb_trips,
                                w2bb_trips)

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration

    # Save updated trips in other object
    rdr_6 <- rdr

    # Join all trips in a single dataset
    rdr_full <- rbind(rdr_full[rdr_full$estrato == 2,], rdr_1[,-ncol(rdr_1)],
                      rdr_3[,-ncol(rdr_3)], rdr_4[,-ncol(rdr_4)],
                      rdr_5[,-ncol(rdr_5)], rdr_6[,-ncol(rdr_6)])
    rdr_full$scenario <- "Scenario 2"
    rd_list[[3]] <- rdr_full


    ###############################################################
    # Scenario 3: Duplicar viajes en bici, todos vienen del carro
    rdr_full <- trip_set

    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)

    # subset of trips that are going to be changed
    rdr <- rdr_full

    # Total number of trips made by female
    tt <- nrow(rdr)

    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.10325623400733,
                              0.31955245597311,
                              0.10440018062315,
                              0.04491495660027,
                              0.04970147007175,
                              0.37817470272440))

    # Difference of trips between baseline and scenario 3
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(motivo %in% c(4,7,8,10)) &
              (age >= 16 & age <= 62) &
              (strptime(hora_inicio, "%H:%M") >= strptime("05:30", "%H:%M") &
              strptime(hora_inicio, "%H:%M") <= strptime("23:30", "%H:%M")) &
              (limitacion == 0))

    # Define weights to give priority to short and medium distances
    rdr2$w <- NA
    rdr2$w[rdr2$trip_distance_cat == "0-6 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "7-15 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "16+ km"] <- 1

    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('bicycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 3',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT,
                                  source_trips = diff_trips[3]*-1)

    redistribute_trips <- c2bb_trips

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration

    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 3"
    rd_list[[4]] <- rdr_full

    # cbind(table(rd_list[[4]]$trip_mode),
    #       prop.table(table(rd_list[[4]]$trip_mode)))


    ###############################################################
    # Scenario 4: Duplicar viajes en bici, todos vienen de transporte privado,
    # Carro y moto
    rdr_full <- trip_set

    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)

    # subset of trips that are going to be changed
    rdr <- rdr_full

    # Total number of trips made by female
    tt <- nrow(rdr)

    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.10325623400733,
                              0.31955245597311,
                              0.11687321258341,
                              0.03244192464001,
                              0.04970147007175,
                              0.37817470272440))

    # Difference of trips between baseline and scenario 3
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(motivo %in% c(4,7,8,10)) &
              (age >= 16 & age <= 62) &
              (strptime(hora_inicio, "%H:%M") >= strptime("05:30", "%H:%M") &
              strptime(hora_inicio, "%H:%M") <= strptime("23:30", "%H:%M")) &
              (limitacion == 0))

    # Define weights to give priority to short and medium distances
    rdr2$w <- NA
    rdr2$w[rdr2$trip_distance_cat == "0-6 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "7-15 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "16+ km"] <- 1

    # Redistribution of car trips to bicycle on any distance
    source_modes <- c('car')
    target_modes <- c('bicycle')
    c2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 4',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT,
                                  source_trips = diff_trips[3]*-1)

    # Redistribution of motorcycle trips to bicycle on any distance
    source_modes <- c('motorcycle')
    target_modes <- c('bicycle')
    m2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 4',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[4]*-1)

    redistribute_trips <- rbind(c2bb_trips, m2bb_trips)

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration

    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 4"
    rd_list[[5]] <- rdr_full

    # cbind(table(rd_list[[5]]$trip_mode),
    #       prop.table(table(rd_list[[5]]$trip_mode)))

    ###############################################################
    # Scenario 5: Duplicar viajes en bici, todos vienen del transporte publico
    rdr_full <- trip_set

    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)

    # subset of trips that are going to be changed
    rdr <- rdr_full

    # Total number of trips made by female
    tt <- nrow(rdr)

    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.10325623400733,
                              0.26792433896945,
                              0.15602829762681,
                              0.04491495660027,
                              0.04970147007175,
                              0.37817470272440))

    # Difference of trips between baseline and scenario 3
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(motivo %in% c(4,7,8,10)) &
              (age >= 16 & age <= 62) &
              (strptime(hora_inicio, "%H:%M") >= strptime("05:30", "%H:%M") &
              strptime(hora_inicio, "%H:%M") <= strptime("23:30", "%H:%M")) &
              (limitacion == 0))

    # Define weights to give priority to short and medium distances
    rdr2$w <- NA
    rdr2$w[rdr2$trip_distance_cat == "0-6 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "7-15 km"] <- 10
    rdr2$w[rdr2$trip_distance_cat == "16+ km"] <- 1

    # Redistribution of bus trips to bicycle on any distance
    source_modes <- c('bus')
    target_modes <- c('bicycle')
    b2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 5',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT,
                                  source_trips = diff_trips[2]*-1)

    redistribute_trips <- b2bb_trips

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration

    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 5"
    rd_list[[6]] <- rdr_full


    ###############################################################
    # Scenario 6: Duplicar viajes en bici, todos vienen a pie
    rdr_full <- trip_set

    # cbind(table(trip_set$trip_mode), prop.table(table(trip_set$trip_mode)))
    # table(trip_set$trip_distance_cat, trip_set$trip_mode)

    # subset of trips that are going to be changed
    rdr <- rdr_full

    # Total number of trips made by female
    tt <- nrow(rdr)

    # Define new proportions
    # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    new_trips <- round(tt * c(0.10325623400733,
                              0.31955245597311,
                              0.15602829762681,
                              0.04491495660027,
                              0.04970147007175,
                              0.32654658572074))

    # Difference of trips between baseline and scenario 3
    diff_trips <- new_trips - table(rdr$trip_mode)

    # Create new id to avoid duplicates at the end of the redistribution
    rdr$id <- 1:nrow(rdr)

    # Subset dataset with restrictions
    rdr2 <- subset(rdr, !(motivo %in% c(4,7,8,10)) &
              (age >= 16 & age <= 62) &
              (strptime(hora_inicio, "%H:%M") >= strptime("05:30", "%H:%M") &
              strptime(hora_inicio, "%H:%M") <= strptime("23:30", "%H:%M")) &
              (limitacion == 0))

    # Redistribution of walking trips to bicycle on any distance
    source_modes <- c('walking')
    target_modes <- c('bicycle')
    w2bb_trips <- create_scenario(rdr2, scen_name = 'Scenario 6',
                                  source_modes = source_modes,
                                  combined_modes = T,
                                  target_modes = target_modes,
                                  source_distance_cats = DIST_CAT[-3],
                                  source_trips = diff_trips[6]*-1)

    redistribute_trips <- w2bb_trips

    # Update selected rows for mode and duration
    rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration

    rdr_full <- rdr[,-ncol(rdr)]
    rdr_full$scenario <- "Scenario 6"
    rd_list[[7]] <- rdr_full

    return(rd_list)
    # cbind(table(rd_list[[7]]$trip_mode),
    #     prop.table(table(rd_list[[7]]$trip_mode)))

    #### Estos es la distribucion de escenarios que iguala toda la distribucion
    #### modal de mujeres a hombres. Pero esto no era lo que necesitaba
    # # Redistribution of walking trips to bicycle on short distances
    # source_modes <- c('walking')
    # target_modes <- c('bicycle')
    # w2bb_trips <- create_scenario(rdr, scen_name = 'Scenario 1',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[1],
    #                              source_trips = diff_trips[1])
    #
    # # Redistribution of walking trips to car on any distance
    # source_modes <- c('walking')
    # target_modes <- c('car')
    # # these trips shouldnt be the same as those chosen to be bicycle
    # rdr2 <- rdr[-match(w2bb_trips$id,rdr$id),]
    # w2c_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT,
    #                              source_trips = diff_trips[3])
    #
    # # Redistribution of walking trips to motorcycle on any distance
    # source_modes <- c('walking')
    # target_modes <- c('motorcycle')
    # remaining_w <- (diff_trips[6] + diff_trips[1] + diff_trips[3]) * -1
    # # these trips shouldnt be the same as those chosen to be bicycle
    # rdr3 <- rdr2[-match(w2c_trips$id,rdr2$id),]
    # w2m_trips <- create_scenario(rdr3, scen_name = 'Scenario 1',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT,
    #                              source_trips = remaining_w)
    #
    # # Redistribution of taxi trips to motorcycle on any distance
    # source_modes <- c('taxi')
    # target_modes <- c('motorcycle')
    # t2m_trips <- create_scenario(rdr, scen_name = 'Scenario 1',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT,
    #                              source_trips = diff_trips[5]*-1)
    #
    # # Redistribution of Bus trips to motorcycle on any distance
    # source_modes <- c('bus')
    # target_modes <- c('motorcycle')
    # b2m_trips <- create_scenario(rdr, scen_name = 'Scenario 1',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT,
    #                              source_trips = diff_trips[2]*-1)
    #
    # redistribute_trips <- rbind(w2bb_trips, w2c_trips, w2m_trips, t2m_trips,
    #                             b2m_trips)
    #
    # # Update selected rows for mode and duration
    # rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    # rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    # rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    # rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    # rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    #
    # rdr_full <- rbind(rdr_full[rdr_full$sex == "male",], rdr[,-13])
    # rdr_full$scenario <- "Scenario 1"
    # rd_list[[2]] <- rdr_full

    # ###############################################################
    # # Scenario 1: Ciudad expandida y dependiente del carro
    # rdr <- trip_set
    # # tt <- length(unique(rdr$trip_id))
    # tt <- nrow(trip_set) # Total number of trips
    # rd_list <- list()
    # rd_list[[1]] <- rdr # Baseline
    #
    # # New distribution of trips
    # # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    # new_trips <- round(tt * c(0.055513583594621,
    #                           0.27936788836516,
    #                           0.21113660337822,
    #                           0.056076012576362,
    #                           0.041060680786673,
    #                           0.356845231298965))
    # # Difference of trips between baseline and scenario 1
    # diff_trips <- new_trips - table(rdr$trip_mode)
    #
    # # Create new id to avoid duplicates at the end of the redistribution
    # rdr$id <- 1:nrow(rdr)
    #
    # # Redistribution of Bus trips to car on any distance
    # source_modes <- c('bus')
    # target_modes <- c('car')
    # b2c_trips <- create_scenario(rdr, scen_name = 'Scenario 1',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT,
    #                              source_trips = diff_trips[3])
    #
    # # Redistribution of bus trips to motorcycle on any distance
    # source_modes <- c('bus')
    # target_modes <- c('motorcycle')
    # # motorcycle trips shouldnt be the same as those chosen to be car
    # rdr2 <- rdr[-match(b2c_trips$id,rdr$id),]
    # b2m_trips <- create_scenario(rdr2, scen_name = 'Scenario 1',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT,
    #                              source_trips = diff_trips[4])
    #
    # # Redistribution of remaining bus trips to walk on short distances (0-5km)
    # source_modes <- c('bus')
    # target_modes <- c('walking')
    # remaining_b <- (diff_trips[2] + diff_trips[4] + diff_trips[3]) * -1
    # # Walking trips shouldnt be the same as those chosen to be motorcycle and bicycle
    # rdr3 <- rdr2[-match(b2m_trips$id,rdr2$id),]
    # b2w_trips <- create_scenario(rdr3, scen_name = 'Scenario 1',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[1],
    #                              source_trips = remaining_b)
    #
    # # Redistribution of Bicycle trips to walk on short distances (0-5km)
    # source_modes <- c('bicycle')
    # target_modes <- c('walking')
    # bb2w_trips <- create_scenario(rdr, scen_name = 'Scenario 1',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[1],
    #                              source_trips = diff_trips[1]*-1)
    #
    # # Redistribution of taxi trips to walk on short distances (0-5km)
    # source_modes <- c('taxi')
    # target_modes <- c('walking')
    # t2w_trips <- create_scenario(rdr, scen_name = 'Scenario 1',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[1],
    #                              source_trips = diff_trips[5]*-1)
    #
    # redistribute_trips <- rbind(b2c_trips, b2m_trips, b2w_trips, bb2w_trips, t2w_trips)
    #
    # # Update selected rows for mode and duration
    # rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    # rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    # rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    # rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    # rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    # rdr$scenario <- "Scenario 1"
    # rdr$id <- NULL
    # rd_list[[2]] <- rdr
    #
    # # ###############################################################
    # # Scenario 2: Ciudad densa y transporte publico
    # rdr <- trip_set
    # # tt <- length(unique(rdr$trip_id))
    # tt <- nrow(trip_set) # Total number of trips
    #
    # # New distribution of trips
    # # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    # new_trips <- round(tt * c(0.05941956695296,
    #                           0.332249391797288,
    #                           0.154803340050874,
    #                           0.037513480575174,
    #                           0.034061052767543,
    #                           0.381953167856161))
    # # Difference of trips between baseline and scenario 1
    # diff_trips <- new_trips - table(rdr$trip_mode)
    #
    # # Create new id to avoid duplicates at the end of the redistribution
    # rdr$id <- 1:nrow(rdr)
    #
    # # Redistribution of taxi trips to walk on short distances
    # source_modes <- c('taxi')
    # target_modes <- c('walking')
    # t2w_trips <- create_scenario(rdr, scen_name = 'Scenario 2',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[1],
    #                              source_trips = diff_trips[5]*-1)
    #
    # # Redistribution of motorcycle trips to walk on short to medium distances
    # source_modes <- c('motorcycle')
    # target_modes <- c('walking')
    # m2w_trips <- create_scenario(rdr, scen_name = 'Scenario 2',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[-3],
    #                              source_trips = diff_trips[4]*-1)
    #
    # # Redistribution of bus trips to car on medium and long distances
    # source_modes <- c('bus')
    # target_modes <- c('car')
    # b2c_trips <- create_scenario(rdr, scen_name = 'Scenario 2',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[-1],
    #                              source_trips = diff_trips[3])
    #
    # # Redistribution of Bus trips to walk on any distance
    # source_modes <- c('bus')
    # target_modes <- c('walking')
    # walktripsleft <- diff_trips[6] + diff_trips[4] + diff_trips[5]
    # # walking trips shouldnt be the same as those chosen to be car
    # rdr2 <- rdr[-match(b2c_trips$id,rdr$id),]
    # b2w_trips <- create_scenario(rdr2, scen_name = 'Scenario 2',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT,
    #                              source_trips = walktripsleft)
    #
    # # Redistribution of bus trips to bicycle on any distance
    # source_modes <- c('bus')
    # target_modes <- c('bicycle')
    # # Bicycle trips shouldnt be the same as those chosen to be walking
    # rdr3 <- rdr2[-match(b2w_trips$id,rdr2$id),]
    # b2bb_trips <- create_scenario(rdr3, scen_name = 'Scenario 2',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT,
    #                              source_trips = diff_trips[1])
    #
    # redistribute_trips <- rbind(t2w_trips, m2w_trips, b2c_trips, b2w_trips, b2bb_trips)
    #
    # # Update selected rows for mode and duration
    # rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    # rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    # rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    # rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    # rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    # rdr$scenario <- "Scenario 2"
    # rdr$id <- NULL
    # rd_list[[3]] <- rdr
    #
    # # ###############################################################
    # # Scenario 3: Ciudad compartida
    # rdr <- trip_set
    # # tt <- length(unique(rdr$trip_id))
    # tt <- nrow(trip_set) # Total number of trips
    #
    # # New distribution of trips
    # # This is the order of proportions: bicycle, bus, car, motorcycle, taxi, walk
    # new_trips <- round(tt * c(0.060407332566651,
    #                           0.322693432062763,
    #                           0.17348662039428,
    #                           0.042677218334197,
    #                           0.036701713924869,
    #                           0.36403368271724))
    #
    # # Difference of trips between baseline and scenario 1
    # diff_trips <- new_trips - table(rdr$trip_mode)
    #
    # # Create new id to avoid duplicates at the end of the redistribution
    # rdr$id <- 1:nrow(rdr)
    #
    # # Redistribution of bus trips to walking on short to medium distances
    # source_modes <- c('bus')
    # target_modes <- c('walking')
    # b2w_trips <- create_scenario(rdr, scen_name = 'Scenario 3',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[-3],
    #                              source_trips = diff_trips[6])
    #
    # # Redistribution of bus trips to car on any distance
    # source_modes <- c('bus')
    # target_modes <- c('car')
    # remaining_b <- (diff_trips[2] + diff_trips[6]) * -1
    # # Car trips shouldnt be the same as those chosen to be bicycle
    # rdr2 <- rdr[-match(b2w_trips$id,rdr$id),]
    # b2c_trips <- create_scenario(rdr2, scen_name = 'Scenario 3',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT,
    #                              source_trips = remaining_b)
    #
    # # Redistribution of taxi trips to bicycle on short distances
    # source_modes <- c('taxi')
    # target_modes <- c('bicycle')
    # t2b_trips <- create_scenario(rdr, scen_name = 'Scenario 3',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[1],
    #                              source_trips = diff_trips[1])
    #
    # # Redistribution of taxi trips to car on medium to long distances
    # source_modes <- c('taxi')
    # target_modes <- c('car')
    # cartripsleft <- diff_trips[3] - remaining_b
    # t2c_trips <- create_scenario(rdr, scen_name = 'Scenario 3',
    #                                source_modes = source_modes, combined_modes = T,
    #                                target_modes = target_modes,
    #                                source_distance_cats = DIST_CAT[-1],
    #                                source_trips = cartripsleft)
    #
    # # Redistribution of taxi trips to Motorcycle
    # source_modes <- c('taxi')
    # target_modes <- c('motorcycle')
    # # motorcycle trips shouldnt be the same as those chosen to be car
    # rdr2 <- rdr[-match(t2c_trips$id,rdr$id),]
    # t2m_trips <- create_scenario(rdr2, scen_name = 'Scenario 3',
    #                              source_modes = source_modes, combined_modes = T,
    #                              target_modes = target_modes,
    #                              source_distance_cats = DIST_CAT[-1],
    #                              source_trips = diff_trips[4])
    #
    # redistribute_trips <- rbind(b2w_trips, b2c_trips, t2b_trips, t2c_trips, t2m_trips)
    #
    # # Update selected rows for mode and duration
    # rdr$trip_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_mode
    # rdr$trip_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$trip_distance
    # rdr$stage_mode[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_mode
    # rdr$stage_distance[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_distance
    # rdr$stage_duration[match(redistribute_trips$id, rdr$id)] <- redistribute_trips$stage_duration
    # rdr$scenario <- "Scenario 3"
    # rdr$id <- NULL
    # rd_list[[4]] <- rdr
    #
    # return(rd_list)
  }
}
