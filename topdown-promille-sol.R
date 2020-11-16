


# plan: 

# check inputs 
# function 1: Calculate mass of alcohol from resported drinks (kind and number)
# function 2: Calculate "Gesamtkörperwasser", total body water
#             from personal data (age, sex, heigt, weight) 
# function 3: Calculate total promille
# function 4: Calculate promille at end of drinking time


# collect formulas from promille-website and course site

# “massn”, “hoibe”, “wein” und “schnaps”
# Für Bier nehmen Sie bitte oktoberfestmäßige 6% an, für Wein 11% und 
# 0.2l pro Portion und für Schnaps 40% und 4cl pro Portion.

# list("massn" = 2, "schnaps" = 3)
# c("wein" = 4, "hoibe" = 1)
# 
# 
# rho <- 0.8 # Alkoholdichte
# rho_blut <- 1.055 # g/cm^3
# h <- height
# m <- weight
# t <- age
# V <- drinks$menge # Volumen
# e <- drinks$umdrehungen # Alkgehalt
# 
# A <- V * e * rho  # Masse Alkohol berechnen
# 
# gkw_mann <- 2.447 - 0.09516 * t + 0.1074 * h + 0.3362 * m
# gkw_frau <-  0.203 - 0.07 * t + 0.1069 * h + 0.2466 * m
# 
# c <- 0.8 * A / (rho_blut * gkw)  # Blutalkohol berechnen
# 
# c_final <- c - ((drinking_time - 1) * 0.15)



# translate formulas into functions 

# function 1: Calculate mass of alcohol from resported drinks (kind and number)
calc_mass_alcohol <- function (drinks) {
  drinks <- as.data.frame(drinks)
  density_alc <- 0.8 # density of alcohol
  # Calculate mass of alcohol per sort of drink
  try (drinks$wein <- drinks$wein * 0.2 * 0.11, silent = T)
  try (drinks$massn <- drinks$massn * 1 * 0.06, silent = T)
  try (drinks$hoibe <- drinks$hoibe * 0.5 * 0.06, silent = T)
  try (drinks$schnaps <- drinks$schnaps * 0.04 * 0.40, silent = T)
  # Add them up
  total_mass_alc <- sum (drinks)
  return (total_mass_alc)
}



# function 2: Calculate "Gesamtkörperwasser", total body water
#             from personal data (age, sex, heigt, weight) 

calc_total_body_water <- function (age, sex, height, weight) {
  if (sex == "male") {
    total_body_water <- 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight
  }
  # else: for women: 
  total_body_water <-  0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
  return (total_body_water)
}


# function 3: Calculate total promille
calc_blood_alc <- function (total_mass_alc, total_body_water) {
  blood_alc <-  (0.8 *  total_mass_alc / (1.055 * total_body_water))*1000
  return (blood_alc)
}


# function 4: Calculate promille at end of drinking time
calc_blood_alc_stop_time <- function (blood_alc, drinking_time) {
  drinking_time_diff <- as.numeric (abs(drinking_time [2] - drinking_time [1]))
  # absolute, if order mistaken, as.numeric to get rid of date format
  blood_alc_stop_theory <- blood_alc - ((drinking_time_diff - 1) * 0.15)
  # maximum, because there is no negative promille values
  blood_alc_stop <- max (blood_alc_stop_theory, 0)
  return (blood_alc_stop)
}




tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight, 
                              drinking_time, drinks) {

  # define possible drinks
  possible_drinks <- c("massn", "hoibe", "wein" ,"schnaps")
  
  # change drinks-input into list
  drinks <- as.list (drinks)
  
  # check inputs
  
  # age must be a number in years
  assert(check_integer(age), check_count(age), age > 0) 
  if (is.na (age) || age < 14) stop (
  "You must indicate your age and be older than 14 to consume alcohol.")
  # heigth must be numeric in cm
  assert (check_numeric(height), height > 0, height < Inf, combine = "and")
  # weight must be numeric in kg
  assert (check_numeric(weight))
  # drinking number must be a date in POSIXt or POSIXct-format
  assert_true (class(drinking_time) == "POSIXct" || class(drinking_time) == "POSIXt" )
  # sex must be "female" or "male"
  if (sex == "m") sex <- "male"
  if (sex == "f") sex <- "female"
  if (sex != "female" & sex != "male") stop ("sex must be 'male' or 'female'")
  # check if drinks have names
  if (is.null (names (drinks))) stop ("you must report WHAT you drink, not just
                                    how much you drink!")
  
  # you cannot name drinks twice in the list
  if (sum(duplicated (names(drinks))) > 0) stop ("please list drinks only once!") 
  # all drinks must be chosen from the predefined list of drinks, this is also
  # important to make referencing clear in function 1: calc_mass_alcohol ()
  if (is.element (FALSE, is.element(names (drinks),possible_drinks))) stop (
    "unknown drink indicated, possible choices are: 'massn', 'hoibe', 'wein', 
    'schnaps'"
  )
  # the number of drinks must be numeric
  assert (check_numeric (unlist(drinks)))
  
  # change time into POSITct 
  drinking_time <- as.POSIXct (drinking_time)
  
  # total mass of alcohol consumed
  total_mass_alc <- calc_mass_alcohol(drinks)
  
  # total body water of consumer
  total_body_water <- calc_total_body_water (age, sex, height, weight)
  
  # blood alc right after drinking
  blood_alc <- calc_blood_alc (total_mass_alc, total_body_water)
  
  # blood alc after drinking time indicated by user
  blood_alc_stop <- calc_blood_alc_stop_time (blood_alc, drinking_time)
  
  return (paste ("Ihr Blutalkohol beträgt", round (blood_alc_stop, digits = 2),
                 "Promille"))

}














