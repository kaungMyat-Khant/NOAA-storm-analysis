### read the data
data <- read.csv(bzfile("storm-dataset.csv.bz2"))

# libraries
library(dplyr)
library(stringr)

# subset data
df <- data %>% 
        select(c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")) %>% 
        filter(EVTYPE != "?" & (FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0))
head(df)
unique(df$PROPDMGEXP)
unique(df$CROPDMGEXP)

# string treat
df <- df %>% 
        mutate(
                ## To Upper Case
                EVTYPE = str_to_upper(EVTYPE),
                ## Remove white spaces
                EVTYPE = str_squish(EVTYPE))

# unique event type
event <- with(df, sort(unique(EVTYPE)))
event
writeLines(text = event, con = "event.txt")
file.edit("event.txt")


# regroup events
df <- df %>% 
        mutate(
                ## AVALANCHE
                EVTYPE = case_when(EVTYPE %in% c("AVALANCE","AVALANCHE") ~ "AVALANCHE"),
                ## BLIZZARD
                EVTYPE = case_when(EVTYPE %in% c("BLIZZARD","BLIZZARD/WINTER STORM",
                                                 "GROUND BLIZZARD") ~ "BLIZZARD"),
                ## COASTAL FLOODING/ EROSION
                EVTYPE = case_when(EVTYPE %in% c("COASTAL EROSION","COASTAL FLOOD",
                                                 "COASTAL FLOODING", 
                                                 "COASTAL FLOODING/EROSION",
                                                 "EROSION/CSTL FLOOD"
                ) ~ "COASTAL FLOODING/EROSION"),
                
                ## COLD/ WINDCHILL (No tornado)
                EVTYPE = case_when(EVTYPE %in% c("COLD",
                                                 "COLD AND SNOW",
                                                 "COLD AND WET CONDITIONS",
                                                 "COLD TEMPERATURE",
                                                 "COLD WAVE",
                                                 "COLD WEATHER",
                                                 "COLD/WIND CHILL",
                                                 "COLD/WINDS",
                                                 "COOL AND WET") ~ "COLD/WIND CHILL"),
                
                ## EXTREME HEAT/DROUGHT
                EVTYPE = case_when(EVTYPE %in% c("DROUGHT",
                                                 "DROUGHT/EXCESSIVE HEAT",
                                                 "HEAT WAVE DROUGHT",
                                                 "EXCESSIVE HEAT",
                                                 "EXTREME HEAT",
                                                 "HEAT",
                                                 "HEAT WAVE",
                                                 "HEAT WAVES",
                                                 "HYPERTHERMIA/EXPOSURE") ~ "EXTREME HEAT/DROUGHT"),

                ## EXTREME COLD/WINDCHILL
                EVTYPE = case_when(EVTYPE %in% c("EXTENDED COLD",
                                                 "EXTREME COLD",
                                                 "EXTREME COLD/WIND CHILL",
                                                 "EXTREME WIND CHILL",
                                                 "EXTREME WINDCHILL") ~ "EXTREME COLD/WIND CHILL"),
                ## FLASH FLOOD
                EVTYPE = case_when(EVTYPE %in% c("FLASH FLOOD",
                                                 "FLASH FLOOD - HEAVY RAIN",
                                                 "FLASH FLOOD FROM ICE JAMS",
                                                 "FLASH FLOOD LANDSLIDES",
                                                 "FLASH FLOOD WINDS",
                                                 "FLASH FLOOD/",
                                                 "FLASH FLOOD/ STREET",
                                                 "FLASH FLOOD/FLOOD",
                                                 "FLASH FLOOD/LANDSLIDE",
                                                 "FLASH FLOODING",
                                                 "FLASH FLOODING/FLOOD",
                                                 "FLASH FLOODING/THUNDERSTORM WI",
                                                 "FLASH FLOODS",
                                                 "FLOOD FLASH",
                                                 "FLOOD/FLASH",
                                                 "FLOOD/FLASH FLOOD",
                                                 "FLOOD/FLASH/FLOOD",
                                                 "FLOOD/FLASHFLOOD",
                                                 "MAJOR FLOOD") ~ "FLASH FLOOD"),
                ## FLOOD
                EVTYPE = case_when(EVTYPE %in% c("FLOOD",
                                                 "FLOOD/RAIN/WINDS",
                                                 "FLOOD/RIVER FLOOD",
                                                 "FLOODING",
                                                 "FLOODING/HEAVY RAIN",
                                                 "FLOODS",
                                                 "MINOR FLOODING") ~ "FLOOD"),
                ## FREEZING FOG
                EVTYPE = case_when(EVTYPE %in% c("FOG AND COLD TEMPERATURES",
                                                 "FREEZING FOG",
                                                 "FREEZING DRIZZLE",
                                                 "Freezing drizzle",
                                                 "Freezing Drizzle",
                                                 "Freezing Rain",
                                                 "FREEZING RAIN",
                                                 "FREEZING RAIN/SLEET",
                                                 "FREEZING RAIN/SNOW",
                                                 "FREEZING SPRAY",
                                                 "LIGHT FREEZING RAIN")  ~ "FREEZING FOG"),
                ## DENSE FOG
                EVTYPE = case_when(EVTYPE %in% c("FOG", "DENSE FOG") ~ "DENSE FOG"),
                
                ## FROST/FREEZE
                EVTYPE = case_when(EVTYPE %in% c("FROST","FROST/FREEZE",
                                                 "FROST\FREEZE",
                                                 "HARD FREEZE") ~ "FROST/FREEZE"),
                
                ## ICE STORM
                EVTYPE = case_when(EVTYPE %in% c("GLAZE",
                                                 "GLAZE ICE",
                                                 "GLAZE/ICE STORM",
                                                 "ICE",
                                                 "ICE AND SNOW",
                                                 "ICE FLOES",
                                                 "ICE JAM",
                                                 "Ice JAM FLOOD (MINOR",
                                                 "ICE JAM FLOODING",
                                                 "ICE ON ROAD",
                                                 "ICE ROADS",
                                                 "ICE STORM",
                                                 "ICE STORM/FLASH FLOOD",
                                                 "ICE/STRONG WINDS",
                                                 "ICY ROADS",
                                                 "HYPOTHERMIA",
                                                 "HYPOTHERMIA/EXPOSURE",
                                                 "LOW TEMPERATURE") ~ "ICE STORM"),
                ## HIGH WIND
                EVTYPE = case_when(EVTYPE %in% c("HIGH WIND",
                                                 "HIGH WIND (G40)",
                                                 "HIGH WIND 48",
                                                 "HIGH WIND AND SEAS"
                                                 "HIGH WIND DAMAGE",
                                                 "HIGH WIND/BLIZZARD",
                                                 "HIGH WIND/HEAVY SNOW",
                                                 "HIGH WIND/SEAS",
                                                 "HIGH WINDS",
                                                 "HIGH WINDS HEAVY RAINS",
                                                 "HIGH WINDS/",
                                                 "HIGH WINDS/COASTAL FLOOD",
                                                 "HIGH WINDS/COLD",
                                                 "HIGH WINDS/HEAVY RAIN",
                                                 "HIGH WINDS/SNOW") ~ "HIGH WIND"),
                ## STRONG WIND
                EVTYPE = case_when(EVTYPE %in% c("GUSTY WIND",
                                                 "GUSTY WIND/HAIL",
                                                 "GUSTY WIND/HVY RAIN",
                                                 "GUSTY WIND/RAIN",
                                                 "GUSTY WINDS",
                                                 "STORM FORCE WINDS",
                                                 "STRONG WIND",
                                                 "STRONG WINDS") ~ "STRONG WIND"),
                ## HAIL
                EVTYPE = case_when(EVTYPE %in% c("HAIL",
                                                 "HAIL 0.75",
                                                 "HAIL 075",
                                                 "HAIL 100",
                                                 "HAIL 125",
                                                 "HAIL 150",
                                                 "HAIL 175",
                                                 "HAIL 200",
                                                 "HAIL 275",
                                                 "HAIL 450",
                                                 "HAIL 75",
                                                 "HAIL DAMAGE",
                                                 "HAIL/WIND",
                                                 "HAIL/WINDS",
                                                 "HAILSTORM") ~ "HAIL"),
                ## HIGH SURF
                EVTYPE = case_when(EVTYPE %in% c("HAZARDOUS SURF",
                                                 "HEAVY SEAS",
                                                 "HEAVY SURF",
                                                 "HEAVY SURF AND WIND",
                                                 "HEAVY SURF COASTAL FLOODING",
                                                 "HEAVY SURF/HIGH SURF",
                                                 "HEAVY SWELLS",
                                                 "HIGH",
                                                 "HIGH SEAS",
                                                 "HIGH SURF",
                                                 "HIGH SURF ADVISORY",
                                                 "HIGH SWELLS",
                                                 "HIGH TIDES",
                                                 "HIGH WATER",
                                                 "HIGH WAVES") ~ "HIGH SURF"),
                ## HEAVY RAIN
                EVTYPE = case_when(EVTYPE %in% c("HEAVY MIX",
                                                 "HEAVY PRECIPITATION",
                                                 "HEAVY RAIN",
                                                 "HEAVY RAIN AND FLOOD",
                                                 "HEAVY RAIN/HIGH SURF",
                                                 "HEAVY RAIN/LIGHTNING",
                                                 "HEAVY RAIN/SEVERE WEATHER",
                                                 "HEAVY RAIN/SMALL STREAM URBAN",
                                                 "HEAVY RAIN/SNOW",
                                                 "HEAVY RAINS",
                                                 "HEAVY RAINS/FLOODING",
                                                 "HEAVY SHOWER",
                                                 "HVY RAIN") ~ "HEAVY RAIN"),
                ## HEAVY SNOW
                EVTYPE = case_when(EVTYPE %in% c("HEAVY SNOW",
                                                 "HEAVY SNOW-SQUALLS",
                                                 "HEAVY SNOW AND HIGH WINDS",
                                                 "HEAVY SNOW AND STRONG WINDS",
                                                 "HEAVY SNOW SHOWER",
                                                 "HEAVY SNOW SQUALLS",
                                                 "HEAVY SNOW/BLIZZARD",
                                                 "HEAVY SNOW/BLIZZARD/AVALANCHE",
                                                 "HEAVY SNOW/FREEZING RAIN",
                                                 "HEAVY SNOW/HIGH WINDS & FLOOD",
                                                 "HEAVY SNOW/ICE",
                                                 "HEAVY SNOW/SQUALLS",
                                                 "HEAVY SNOW/WIND",
                                                 "HEAVY SNOW/WINTER STORM",
                                                 "HEAVY SNOWPACK",
                                                 "LATE SEASON SNOW") ~ "HEAVY SNOW"),
                ## HURRICANE
                EVTYPE = case_when(EVTYPE %in% c("HURRICANE",
                                                 "HURRICANE-GENERATED SWELLS",
                                                 "HURRICANE EDOUARD",
                                                 "HURRICANE EMILY",
                                                 "HURRICANE ERIN",
                                                 "HURRICANE FELIX",
                                                 "HURRICANE GORDON",
                                                 "HURRICANE OPAL",
                                                 "HURRICANE OPAL/HIGH WINDS",
                                                 "HURRICANE/TYPHOON") ~ "HURRICANE"),
                ## LAKE-EFFECT SNOW
                EVTYPE = case_when(EVTYPE %in% c("LAKE-EFFECT SNOW",
                                                 "LAKE EFFECT SNOW") ~ "LAKE-EFFECT SNOW"),
                ## LAKESHORE FLOOD
                EVTYPE = case_when(EVTYPE %in% c("LAKE FLOOD",
                                                 "LAKESHORE FLOOD") ~ "LAKESHORE FLOOD"),
                ## LANDSLIDES
                EVTYPE = case_when(EVTYPE %in% c("LANDSLIDE",
                                                 "LANDSLIDES",
                                                 "LANDSLUMP") ~ "LANDSLIDES"),
                ## LIGHT SNOWFALL
                EVTYPE = case_when(EVTYPE %in% c("LIGHT SNOW"
                                                 "LIGHT SNOWFALL") ~ "LIGHT SNOWFALL"),
                ## LIGHTNING
                EVTYPE = case_when(EVTYPE %in% c("LIGHTING",
                                                 "LIGHTNING",
                                                 "LIGHTNING AND HEAVY RAIN",
                                                 "LIGHTNING AND THUNDERSTORM WIN",
                                                 "LIGHTNING FIRE",
                                                 "LIGHTNING INJURY",
                                                 "LIGHTNING THUNDERSTORM WINDS",
                                                 "LIGHTNING WAUSEON",
                                                 "LIGHTNING.",
                                                 "LIGHTNING/HEAVY RAIN",
                                                 "LIGNTNING") ~ "LIGHTNING"),
                ## MARINE THUNDERSTORM WIND
                EVTYPE = case_when(EVTYPE %in% c("MARINE THUNDERSTORM WIND",
                                                 "MARINE TSTM WIND") ~ "MARINE THUNDERSTORM WIND"),
                
                
                ## THUNDERSTORM WIND
                EVTYPE = case_when(EVTYPE %in% c("MICROBURST",
                                                 "MICROBURST WINDS","GRADIENT WIND",
                                                 "GUSTNADO",
                                                 "DOWNBURST") ~ "THUNDERSTORM WIND"),
                
                ## TORNADO
                EVTYPE = case_when(EVTYPE %in% c("COLD AIR TORNADO",
                                                 "LANDSPOUT",))
                
        )