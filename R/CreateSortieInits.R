
#' create sortie inits from Summit Lake data
#'
#' @param dbh_size_class
#' @param plot_area
#' @param raw_data
#' @param in_dir
#' @param out_dir
#' @author Alana Clason & Leah Walker
#'
#' @description
#' initial tree conditions created from 1992 Summit Lake data. This code also integrates the density
#' of planted trees surveyed in 2021, and backcasts that density to the smallest seedling class
#' size for the 1992 data. This is likely an underestimate assuming planted seedling mortality from
#' 1992 to 2021, but we also add natural regeneration data and small tree data (<4cm DBH) from
#' a nearby study in the SBS. In 1992 the smallest tagged diameter size was 4cm, so we use the
#' Summit Lake data down to that size class.
#'
#' For every initial condition produced, we draw randomly from a normal distribution of these seedling
#' densities to create some initial variability in seedling densities between plots. So this will
#' change with every run of the sl_sortie_inits function.
#'
#' @return
#' @export
#'
#' @examples
sl_sortie_inits <- function(dbh_size_class = 2,
                              plot_area = 0.05,
                              raw_data = "D:/Github/SummitLakeData/data-raw/SummitLakeData.csv",
                              in_dir,
                              out_dir) {


  #1. tree initial values
  #sbs_trees <- clean_trees(raw_data = "D:/Github/SummitLakeData/data-raw/SummitLakeData.csv")
  summit_sph <- plot_sph_size(dbh_size_class = 2,
                              plot_area = 0.05,
                              raw_data = raw_data)

  #get live trees from 1992 for all plots except 4 & 15 that were established in 1994
  summit_sph_u_o <- summit_sph[unit != 4 & unit != 15 & Year == 1992 & State == "Live"]
  summit_sph_u_4_15 <- summit_sph[unit == 4 & Year == 1994 & State == "Live"|
                                 unit == 15 & Year == 1994 & State == "Live"]
  summit_sph_u <- rbind(summit_sph_u_o, summit_sph_u_4_15)
  summit_sph_u[, vari := paste0("Init.Dens_", DBH_bin,".0")]


  #prior to 2009, limit was 4cm DBH, so go down to 4cm from summit lake data

  #2. add small trees from Bruce Rogers Sx trial:
  small_trees <- fread(file.path(in_dir, "SBS_nat_regen.csv"))
  # their height and diameter classes don't match SORTIE.
  # diameter cleasses include the lower bound, but not the upper
  small_trees[ , sortie_bins := ifelse(Height_m <= 1.3 & is.na(DBH_cm), "Init.Dens_1",
                                       ifelse(Height_m >= 1.3 & DBH_cm < 2,"Init.Dens_2.0",
                                              ifelse(Height_m >= 1.3 & DBH_cm >= 2 & DBH_cm < 4,
                                                     "Init.Dens_4.0",
                                                     ifelse(Height_m >= 1.3 & DBH_cm >= 4 & DBH_cm < 6,
                                                            "Init.Dens_6.0",
                                                            ifelse(Height_m >= 1.3 & DBH_cm >= 6 & DBH_cm < 7.5,
                                                                   "Init.Dens_8.0",
                                                                   NA)))))]
  small_trees[, Species := ifelse(Species == "SX", "Sx",
                                  Species)]
  #cleaning up some odd ones
  small_trees[DBH_cm == 6.1 & Height_m == 0.81, sortie_bins := "Init.Dens_8.0"]
  small_trees[is.na(DBH_cm) & is.na(sortie_bins)]
  small_trees[DBH_cm == 0.5 & Height_m == 1, sortie_bins := "Init.Dens_1"]
  small_trees[DBH_cm == 8.0 & Height_m == 0.49, sortie_bins := "Init.Dens_10.0"]
  small_trees[DBH_cm == 2.5 & Height_m == 1.17, sortie_bins := "Init.Dens_4.0"]
  small_trees[DBH_cm == 7.4 & is.na(Height_m), sortie_bins := "Init.Dens_8.0"]
  small_trees <- small_trees[!is.na(sortie_bins)]

  sm_trees_u4 <- small_trees[sortie_bins != "Init.Dens_8.0"][sortie_bins != "Init.Dens_6.0"]
  #assuming 5.64m plots
  small_trees_sph <- sm_trees_u4[,.N/0.01, by=.(Plot, Species, sortie_bins)]
  sph_nat <- dcast(small_trees_sph, Plot+sortie_bins ~ Species,
                   value.var = "V1")
  sph_nat[is.na(sph_nat)] <- 0

  sph_nat <- sph_nat[, .(Bl = mean(Bl), Sx = mean(Sx)), by = sortie_bins]
  #sph_nat[is.na()]

  #3. add planted seedlings (units 3 - 15 & 24)
  plantTrees <- fread(file.path(in_dir, "SBS_plantedTrees.csv"))
  #remove trees that are outside the plot
  plantTrees <- plantTrees[!grep("Outside|outside", COMMENTS_2021),]
  # counted in the PSP, so 0.05 ha in size
  sph_plant <- plantTrees[, .(SPH = (.N/0.05)), by = .(Plot)] #all spruce
  setnames(sph_plant, "SPH", "Sx")


  #4. add snags
  #get dead trees from 1992 for all plots except 4 & 15 that were established in 1994
  #psp_r_94 <- remeas_freq[`1994`== "R"]$PSP
  #summit_sph_d_u_o <- summit_sph[unit %in% psp_r_94 & Year == 1994 & State == "Dead"]
  #summit_sph_d_u_4_15 <- summit_sph[unit == 4 & Year == 1994 & State == "Dead"|
   #                                 unit == 15 & Year == 1994 & State == "Dead"]
  #summit_sph_d_u <- rbind(summit_sph_d_u_o, summit_sph_u_4_15)
  #summit_sph_d_u[, vari := paste0("Init.Dens_", DBH_bin,".0")]



  #5. write out the initial plots
  sl_out_dir <- file.path(out_dir, "02_summit_lake","ParameterValues")

  for(ii in 1:length(unique(summit_sph_u$unit))){
    dat.unit <- summit_sph_u[unit == unique(summit_sph_u$unit)[ii]]
    dat.unit$unit <- NULL
    dat.unit$DBH_bin <- NULL
    dat.unit <- dcast(dat.unit, vari ~ Species, value.var = "SPH")
    dat.unit <- as.data.table(dat.unit)
    dat.unit[, c("Pl", "At", "Ac") := 0]

    #random draw from naturals < 4cm
    sph_nat_ii <- sph_nat[,  .(Bl = abs(rnorm(1, mean = Bl, sd = 50)),
                               Sx = abs(rnorm(1, mean = Sx, sd = 50))),
                          by = sortie_bins]
    sph_nat_ii <- sph_nat_ii[sortie_bins == "Init.Dens_1" |
                               sortie_bins == "Init.Dens_2.0" |
                               sortie_bins == "Init.Dens_4.0"]
    setnames(sph_nat_ii, "sortie_bins", "vari")

    #add planted
    sph_plant_ii <- sph_plant[Plot == unique(summit_sph_u$unit)[ii]]
    sph_plant_ii[, Plot := NULL]
    sph_plant_ii[, sortie_bins := "Init.Dens.Seedling.Hgt.Class.1"]
    setnames(sph_plant_ii, "sortie_bins", "vari")

    #join them together
    dat.unit <- rbind(dat.unit, sph_nat_ii, fill = TRUE)
    dat.unit <- rbind(dat.unit, sph_plant_ii, fill = TRUE)

    #fill in any missing inits with 0
    full_list <- data.table(vari = paste0("Init.Dens_", seq(6,90, by = 2),".0"))
    dat.unit <- merge(dat.unit, full_list, by = "vari", all = TRUE)
    dat.unit[is.na(dat.unit)] <- 0

    #dat.unit <- rbindlist(list(dat.unit, TS92), use.names = TRUE, fill = TRUE)
    DT <- data.table(1)[, `:=`(c("vari", "Sx", "Pl", "Bl", "At", "Lw", "Fd", "Ac", "Ep"), NA)][,V1 := NULL]
    dat.unit <- rbindlist(list(DT, dat.unit[1:nrow(dat.unit)]), use.names = TRUE, fill = TRUE)
    setcolorder(dat.unit, c("vari", "Sx", "Pl", "Bl", "At", "Lw", "Fd", "Ac", "Ep"))
    setnames(dat.unit, c("vari", "Sx", "Pl", "Bl", "At", "Lw", "Fd", "Ac", "Ep"),
             c(" ", "Interior_Spruce", "Lodgepole_Pine", "Subalpine_Fir", "Trembling_Aspen",
               "Western_Larch", "Douglas_Fir", "Black_Cottonwood", "Paper_Birch"))

    write.csv(dat.unit, paste0(sl_out_dir,"/","summit_",
                               unique(summit_sph_u$unit)[ii],".csv"), row.names = FALSE)
  }
}


#' Plot tree summary - by species and size class
#'
#' @param dbh_size_class
#' @param minDBH
#' @param plot_area
#' @param raw_data
#'
#' @description
#' take the cleaned tree data and produce the stems/ha for each Summit Lake plot. This is largely
#' used to create sortie conditions, but could be used more broadly if needed (hence no export
#' needed on this function)
#'
#'
#' @return
#'
#' @examples
plot_sph_size <- function(dbh_size_class = 2,
                          plot_area = 0.05,
                          raw_data = "./data-raw/SummitLakeData.csv") {

  #sl_dat <- SummitLakeData::clean_tree_data(raw_data = raw_data)
  sl_dat <- SummitLakeData::clean_trees(raw_data = raw_data)
  #summary(lm(Height ~ Hgt_allom, data = sl_dat[!is.na(Height)]))

  minDBH <- round(min(sl_dat$DBH, na.rm = TRUE),0)
  maxDBH <- round(max(sl_dat$DBH, na.rm = TRUE),0)
  # Create a vector of DBH size classes, by 2 cm increments
  diam_classes <- seq(minDBH,(maxDBH + dbh_size_class),
                      by = dbh_size_class)

  # Replace old tree tag numbers with new, if applicable
  #raw_data$TreeID <- as.numeric(raw_data$TreeID)
  #raw_data$TreeID_new <- as.numeric(raw_data$TreeID_new)
  #raw_data <- raw_data %>%
  #  dplyr::mutate(TreeID = ifelse(is.na(TreeID_new), TreeID, TreeID_new))
  #raw_data$TreeID_new <- NULL

  # Create column for and fill with DBH bins
  for(j in 1:length(diam_classes)){
    sl_dat[DBH <= diam_classes[j] & DBH > diam_classes[j] - dbh_size_class,
           DBH_bin := diam_classes[j]]
  }

  #add all zeros:
  all_poss <- CJ(unique(sl_dat$unit), unique(sl_dat$Species), unique(sl_dat$Year),
                 unique(sl_dat$State), unique(sl_dat$DBH_bin))
  setnames(all_poss,c("V1","V2","V3","V4","V5"),
           c("unit","Species","Year","State","DBH_bin"))

  plot_ha <- 1/plot_area
  sl_dat[, SPH := 1 * plot_ha]

  #merge with sl_dat
  sl_poss <- merge(sl_dat, all_poss,
                   by = c("unit","Species","Year","State","DBH_bin"),
                   all = T)
  sl_poss <- sl_poss[, .(unit, Species, Year, State, DBH_bin, SPH)]
  #sl_poss <- sl_poss[!is.na(DBH_bin)]
  sl_poss[is.na(SPH), SPH := 0]
  sl_sum <- sl_poss[, .(SPH = sum(SPH)), by = .(unit,Species,Year,State,DBH_bin)]

  return(sl_sum)
  #sl_dat[, .(SPH = .N/plot_area), by = .(unit,Species,Year,State,DBH_bin)]

  # Calculate stems per hectare
  #dat.summit.m.s[, SPH := count/ PlotArea]

  # Remove unnecessary columns
  #dat.summit.m.s$TreeID <- NULL
  #dat.summit.m.s$DBH <- NULL
  #dat.summit.m.s$count <- NULL

  # Merge labels with data set including SPH
  #dat.summit.SPH <- merge(labels.summit.spD, dat.summit.m.s, all = T)
  #cols <- "SPH"
  # Now that the counts are done, fill in empty DBH bins with zero
  #dat.summit.SPH[,(cols) := lapply(.SD,nafill, fill = 0), .SDcols = cols]
  # Eliminate duplicates
  #dat.summit.SPH <- unique(dat.summit.SPH)
}


