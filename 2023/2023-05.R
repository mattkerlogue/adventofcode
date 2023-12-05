# advent of code 2023
# day 5

input <- readLines("2023/input/2023-05")


# part 1 ------------------------------------------------------------------

# function factory
garden_shifter <- function(garden_map) {

  shft <- sapply(garden_map, function(x){x[[1]] - x[[2]]})
  srts <- sapply(garden_map, function(x){x[[2]]})
  ends <- sapply(garden_map, function(x){x[[2]] + x[[3]] - 1})

  function (y) {

    map_loc <- which(y >= srts & ends >= y)

    if (length(map_loc) == 0) {
      return(y)
    } else {
      return(y + shft[map_loc])
    }

  }

}

seeds <- as.numeric(unlist(strsplit(gsub("^seeds: ", "", input[1]), split = "\\s")))

seed_soil <- lapply(
  strsplit(
    input[(which(grepl("^seed-", input))+1):(which(grepl("^soil",input))-2)],
    split = "\\s"
  ),
  as.numeric
)

soil_fert <- lapply(
  strsplit(
    input[(which(grepl("^soil", input))+1):(which(grepl("^fert",input))-2)],
    split = "\\s"
  ),
  as.numeric
)

fert_water <- lapply(
  strsplit(
    input[(which(grepl("^fert", input))+1):(which(grepl("^water",input))-2)],
    split = "\\s"
  ),
  as.numeric
)

water_light <- lapply(
  strsplit(
    input[(which(grepl("^water", input))+1):(which(grepl("^light",input))-2)],
    split = "\\s"
  ),
  as.numeric
)

light_temp <- lapply(
  strsplit(
    input[(which(grepl("^light", input))+1):(which(grepl("^temp",input))-2)],
    split = "\\s"
  ),
  as.numeric
)

temp_humid <- lapply(
  strsplit(
    input[(which(grepl("^temp", input))+1):(which(grepl("^humid",input))-2)],
    split = "\\s"
  ),
  as.numeric
)

humid_loc <- lapply(
  strsplit(
    input[(which(grepl("^humid", input))+1):length(input)],
    split = "\\s"
  ),
  as.numeric
)


seed_soil_shift   <- garden_shifter(seed_soil)
soil_fert_shift   <- garden_shifter(soil_fert)
fert_water_shift  <- garden_shifter(fert_water)
water_light_shift <- garden_shifter(water_light)
light_temp_shift  <- garden_shifter(light_temp)
temp_humid_shift  <- garden_shifter(temp_humid)
humid_loc_shift   <- garden_shifter(humid_loc)

seed_to_loc <- function(x) {
  x |>
    seed_soil_shift() |>
    soil_fert_shift() |>
    fert_water_shift() |>
    water_light_shift() |>
    light_temp_shift() |>
    temp_humid_shift() |>
    humid_loc_shift()
}

locations <- sapply(seeds, seed_to_loc)

min(locations)
