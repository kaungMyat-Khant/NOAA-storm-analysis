### read the data
data <- read.csv(bzfile("storm-dataset.csv.bz2"))

# libraries
library(tidyverse)


# subset data
df <- data %>% 
        select(c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")) %>% 
        filter(EVTYPE != "?" & (FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0))
head(df)

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
                EVTYPE = case_when(EVTYPE %in% c("AVALANCE","AVALANCHE") ~ "AVALANCHE",
                                   ## BLIZZARD
                                   EVTYPE %in% c("BLIZZARD","BLIZZARD/WINTER STORM",
                                                 "GROUND BLIZZARD") ~ "BLIZZARD",
                                   ## COASTAL FLOODING/ EROSION
                                   EVTYPE %in% c("COASTAL EROSION","COASTAL FLOOD",
                                                 "COASTAL FLOODING", 
                                                 "COASTAL FLOODING/EROSION",
                                                 "EROSION/CSTL FLOOD") ~ "COASTAL FLOODING/EROSION",
                                   ## COLD/ WINDCHILL (No tornado)
                                   EVTYPE %in% c("COLD",
                                                 "COLD AND SNOW",
                                                 "COLD AND WET CONDITIONS",
                                                 "COLD TEMPERATURE",
                                                 "COLD WAVE",
                                                 "COLD WEATHER",
                                                 "COLD/WIND CHILL",
                                                 "COLD/WINDS",
                                                 "COOL AND WET",
                                                 "RECORD COLD",
                                                 "UNSEASONABLE COLD",
                                                 "UNSEASONABLY COLD") ~ "COLD/WIND CHILL",
                                   ## EXTREME HEAT/DROUGHT
                                   EVTYPE %in% c("DROUGHT",
                                                 "DROUGHT/EXCESSIVE HEAT",
                                                 "HEAT WAVE DROUGHT",
                                                 "EXCESSIVE HEAT",
                                                 "EXTREME HEAT",
                                                 "HEAT",
                                                 "HEAT WAVE",
                                                 "HEAT WAVES",
                                                 "HYPERTHERMIA/EXPOSURE",
                                                 "RECORD/EXCESSIVE HEAT",
                                                 "UNSEASONABLY WARM",
                                                 "UNSEASONABLY WARM AND DRY",
                                                 "WARM WEATHER") ~ "EXTREME HEAT/DROUGHT",
                                   ## EXTREME COLD/WINDCHILL
                                   EVTYPE %in% c("EXTENDED COLD",
                                                 "EXTREME COLD",
                                                 "EXTREME COLD/WIND CHILL",
                                                 "EXTREME WIND CHILL",
                                                 "EXTREME WINDCHILL") ~ "EXTREME COLD/WIND CHILL",
                                   ## FLASH FLOOD
                                   EVTYPE %in% c("FLASH FLOOD",
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
                                                 "MAJOR FLOOD") ~ "FLASH FLOOD",
                                   ## FLOOD
                                   EVTYPE %in% c("FLOOD",
                                                 "FLOOD/RAIN/WINDS",
                                                 "FLOOD/RIVER FLOOD",
                                                 "FLOODING",
                                                 "FLOODING/HEAVY RAIN",
                                                 "FLOODS",
                                                 "MINOR FLOODING",
                                                 "RIVER AND STREAM FLOOD",
                                                 "RIVER FLOOD",
                                                 "RIVER FLOODING",
                                                 "RURAL FLOOD",
                                                 "SMALL STREAM FLOOD",
                                                 "SNOWMELT FLOODING",
                                                 "URBAN AND SMALL",
                                                 "URBAN AND SMALL STREAM FLOODIN",
                                                 "URBAN FLOOD",
                                                 "URBAN FLOODING",
                                                 "URBAN FLOODS",
                                                 "URBAN SMALL",
                                                 "URBAN/SMALL STREAM",
                                                 "URBAN/SMALL STREAM FLOOD",
                                                 "URBAN/SML STREAM FLD") ~ "FLOOD",
                                   ## FREEZING FOG
                                   EVTYPE %in% c("FOG AND COLD TEMPERATURES",
                                                 "FREEZING FOG",
                                                 "FREEZING DRIZZLE",
                                                 "FREEZING RAIN",
                                                 "FREEZING RAIN/SLEET",
                                                 "FREEZING RAIN/SNOW",
                                                 "FREEZING SPRAY",
                                                 "LIGHT FREEZING RAIN")  ~ "FREEZING FOG",
                                   ## DENSE FOG
                                   EVTYPE %in% c("FOG", "DENSE FOG") ~ "DENSE FOG",
                                   ## FROST/FREEZE
                                   EVTYPE %in% c("FROST","FROST/FREEZE",
                                                 "FROST\\FREEZE",
                                                 "HARD FREEZE",
                                                 "AGRICULTURAL FREEZE") ~ "FROST/FREEZE",
                                   ## ICE STORM
                                   EVTYPE %in% c("GLAZE",
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
                                                 "LOW TEMPERATURE") ~ "ICE STORM",
                                   ## HIGH WIND
                                   EVTYPE %in% c("HIGH WIND",
                                                 "HIGH WIND (G40)",
                                                 "HIGH WIND 48",
                                                 "HIGH WIND AND SEAS",
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
                                                 "HIGH WINDS/SNOW",
                                                 "NON-TSTM WIND",
                                                 "NON TSTM WIND",
                                                 "NON-SEVERE WIND DAMAGE",
                                                 "WINDS",
                                                 "WIND") ~ "HIGH WIND",
                                   ## STRONG WIND
                                   EVTYPE %in% c("GUSTY WIND",
                                                 "GUSTY WIND/HAIL",
                                                 "GUSTY WIND/HVY RAIN",
                                                 "GUSTY WIND/RAIN",
                                                 "GUSTY WINDS",
                                                 "STORM FORCE WINDS",
                                                 "STRONG WIND",
                                                 "STRONG WINDS",
                                                 "WIND AND WAVE",
                                                 "WIND DAMAGE",
                                                 "WIND STORM",
                                                 "WIND/HAIL") ~ "STRONG WIND",
                                   ## HAIL
                                   EVTYPE %in% c("HAIL",
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
                                                 "HAILSTORM",
                                                 "SMALL HAIL") ~ "HAIL",
                                   ## HIGH SURF
                                   EVTYPE %in% c("HAZARDOUS SURF",
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
                                                 "HIGH WAVES",
                                                 "RAPIDLY RISING WATER",
                                                 "ROGUE WAVE",
                                                 "ROUGH SEAS",
                                                 "ROUGH SURF",
                                                 "STORM SURGE",
                                                 "STORM SURGE/TIDE") ~ "HIGH SURF",
                                   ## HEAVY RAIN
                                   EVTYPE %in% c("HEAVY MIX",
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
                                                 "HVY RAIN",
                                                 "RECORD RAINFALL",
                                                 "RAIN",
                                                 "RAIN/SNOW",
                                                 "RAIN/WIND",
                                                 "RAINSTORM",
                                                 "TORRENTIAL RAINFALL") ~ "HEAVY RAIN",
                                   ## HEAVY SNOW
                                   EVTYPE %in% c("HEAVY SNOW",
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
                                                 "LATE SEASON SNOW",
                                                 "RECORD SNOW",
                                                 "SNOW",
                                                 "SNOW ACCUMULATION",
                                                 "SNOW AND HEAVY SNOW",
                                                 "SNOW AND ICE",
                                                 "SNOW AND ICE STORM",
                                                 "SNOW FREEZING RAIN",
                                                 "SNOW SQUALL",
                                                 "SNOW SQUALLS",
                                                 "SNOW/ BITTER COLD",
                                                 "SNOW/ ICE",
                                                 "SNOW/BLOWING SNOW",
                                                 "SNOW/COLD",
                                                 "SNOW/FREEZING RAIN",
                                                 "SNOW/HEAVY SNOW",
                                                 "SNOW/HIGH WINDS",
                                                 "SNOW/ICE",
                                                 "SNOW/ICE STORM","SNOW/SLEET",
                                                 "SNOW/SLEET/FREEZING RAIN") ~ "HEAVY SNOW",
                                   ## HURRICANE
                                   EVTYPE %in% c("HURRICANE",
                                                 "HURRICANE-GENERATED SWELLS",
                                                 "HURRICANE EDOUARD",
                                                 "HURRICANE EMILY",
                                                 "HURRICANE ERIN",
                                                 "HURRICANE FELIX",
                                                 "HURRICANE GORDON",
                                                 "HURRICANE OPAL",
                                                 "HURRICANE OPAL/HIGH WINDS",
                                                 "HURRICANE/TYPHOON",
                                                 "TYPHOON") ~ "HURRICANE",
                                   ## LAKE-EFFECT SNOW
                                   EVTYPE %in% c("LAKE-EFFECT SNOW",
                                                 "LAKE EFFECT SNOW") ~ "LAKE-EFFECT SNOW",
                                   ## LAKESHORE FLOOD
                                   EVTYPE %in% c("LAKE FLOOD",
                                                 "LAKESHORE FLOOD") ~ "LAKESHORE FLOOD",
                                   ## LANDSLIDES
                                   EVTYPE %in% c("LANDSLIDE",
                                                 "LANDSLIDES",
                                                 "LANDSLUMP",
                                                 "MUD SLIDE",
                                                 "MUD SLIDES",
                                                 "MUD SLIDES URBAN FLOODING",
                                                 "MUDSLIDE",
                                                 "MUDSLIDES",
                                                 "ROCK SLIDE") ~ "LANDSLIDES",
                                   ## LIGHT SNOWFALL
                                   EVTYPE %in% c("LIGHT SNOW",
                                                 "LIGHT SNOWFALL") ~ "LIGHT SNOWFALL",
                                   ## LIGHTNING
                                   EVTYPE %in% c("LIGHTING",
                                                 "LIGHTNING",
                                                 "LIGHTNING AND HEAVY RAIN",
                                                 "LIGHTNING AND THUNDERSTORM WIN",
                                                 "LIGHTNING FIRE",
                                                 "LIGHTNING INJURY",
                                                 "LIGHTNING THUNDERSTORM WINDS",
                                                 "LIGHTNING WAUSEON",
                                                 "LIGHTNING.",
                                                 "LIGHTNING/HEAVY RAIN",
                                                 "LIGNTNING") ~ "LIGHTNING",
                                   ## MARINE THUNDERSTORM WIND
                                   EVTYPE %in% c("MARINE THUNDERSTORM WIND",
                                                 "MARINE TSTM WIND") ~ "MARINE THUNDERSTORM WIND",
                                   ## THUNDERSTORM WIND
                                   EVTYPE %in% c("MICROBURST",
                                                 "MICROBURST WINDS",
                                                 "GRADIENT WIND",
                                                 "GUSTNADO",
                                                 "DOWNBURST",
                                                 "SEVERE THUNDERSTORM",
                                                 "SEVERE THUNDERSTORM WINDS",
                                                 "SEVERE THUNDERSTORMS",
                                                 "SEVERE TURBULENCE",
                                                 "THUDERSTORM WINDS",
                                                 "THUNDEERSTORM WINDS",
                                                 "THUNDERESTORM WINDS",
                                                 "THUNDERSNOW",
                                                 "THUNDERSTORM",
                                                 "THUNDERSTORM DAMAGE TO",
                                                 "THUNDERSTORM HAIL",
                                                 "THUNDERSTORM WIND",
                                                 "THUNDERSTORM WIND (G40)",
                                                 "THUNDERSTORM WIND 60 MPH",
                                                 "THUNDERSTORM WIND 65 MPH",
                                                 "THUNDERSTORM WIND 65MPH",
                                                 "THUNDERSTORM WIND 98 MPH",
                                                 "THUNDERSTORM WIND G50",
                                                 "THUNDERSTORM WIND G52",
                                                 "THUNDERSTORM WIND G55",
                                                 "THUNDERSTORM WIND G60",
                                                 "THUNDERSTORM WIND TREES",
                                                 "THUNDERSTORM WIND.",
                                                 "THUNDERSTORM WIND/ TREE",
                                                 "THUNDERSTORM WIND/ TREES",
                                                 "THUNDERSTORM WIND/AWNING",
                                                 "THUNDERSTORM WIND/HAIL",
                                                 "THUNDERSTORM WIND/LIGHTNING",
                                                 "THUNDERSTORM WINDS",
                                                 "THUNDERSTORM WINDS 13",
                                                 "THUNDERSTORM WINDS 63 MPH",
                                                 "THUNDERSTORM WINDS AND",
                                                 "THUNDERSTORM WINDS G60",
                                                 "THUNDERSTORM WINDS HAIL",
                                                 "THUNDERSTORM WINDS LIGHTNING",
                                                 "THUNDERSTORM WINDS.",
                                                 "THUNDERSTORM WINDS/ FLOOD",
                                                 "THUNDERSTORM WINDS/FLOODING",
                                                 "THUNDERSTORM WINDS/FUNNEL CLOU",
                                                 "THUNDERSTORM WINDS/HAIL",
                                                 "THUNDERSTORM WINDS53",
                                                 "THUNDERSTORM WINDSHAIL",
                                                 "THUNDERSTORM WINDSS",
                                                 "THUNDERSTORM WINS",
                                                 "THUNDERSTORMS",
                                                 "THUNDERSTORMS WIND",
                                                 "THUNDERSTORMS WINDS",
                                                 "THUNDERSTORMW",
                                                 "THUNDERSTORMWINDS",
                                                 "THUNDERSTROM WIND",
                                                 "THUNDERTORM WINDS",
                                                 "THUNERSTORM WINDS",
                                                 "TSTM WIND",
                                                 "TSTM WIND (41)",
                                                 "TSTM WIND (G35)",
                                                 "TSTM WIND (G40)",
                                                 "TSTM WIND (G45)",
                                                 "TSTM WIND 40",
                                                 "TSTM WIND 45",
                                                 "TSTM WIND 55",
                                                 "TSTM WIND 65)",
                                                 "TSTM WIND AND LIGHTNING",
                                                 "TSTM WIND DAMAGE",
                                                 "TSTM WIND G45",
                                                 "TSTM WIND G58",
                                                 "TSTM WIND/HAIL",
                                                 "TSTM WINDS",
                                                 "TSTMW",
                                                 "TUNDERSTORM WIND",
                                                 "WET MICROBURST") ~ "THUNDERSTORM WIND",
                                   ## MIXED PRECIPITATION
                                   EVTYPE %in% c("MIXED PRECIP",
                                                 "MIXED PRECIPITATION") ~ "MIXED PRECIPITATION",
                                   ## RAIN
                                   EVTYPE %in% c("TROPICAL STORM",
                                                 "TROPICAL STORM ALBERTO",
                                                 "TROPICAL STORM DEAN",
                                                 "TROPICAL STORM GORDON",
                                                 "TROPICAL STORM JERRY") ~ "TROPICAL STORM",
                                   # TORNADO
                                   EVTYPE %in% c("COLD AIR TORNADO",
                                                 "LANDSPOUT",
                                                 "TORNADO",
                                                 "TORNADO F0",
                                                 "TORNADO F1",
                                                 "TORNADO F2",
                                                 "TORNADO F3",
                                                 "TORNADOES",
                                                 "TORNADOES, TSTM WIND, HAIL",
                                                 "TORNDAO",
                                                 "WATERSPOUT-TORNADO",
                                                 "WATERSPOUT TORNADO",
                                                 "WATERSPOUT/ TORNADO",
                                                 "WATERSPOUT/TORNADO",
                                                 "WHIRLWIND") ~ "TORNADO",
                                   ## WATER SPROUT
                                   EVTYPE %in% c("WATERSPOUT",
                                                 "WATERSPOUT-") ~"WATERSPOUT",
                                   ## WILD FIRES
                                   EVTYPE %in% c("WILD FIRES",
                                                 "WILD/FOREST FIRE",
                                                 "WILD/FOREST FIRES",
                                                 "WILDFIRE",
                                                 "WILDFIRES",
                                                 "GRASS FIRES") ~ "WILD FIRES",
                                   ## WINTER STORM
                                   EVTYPE %in% c("WINTER STORM",
                                                 "WINTER STORM HIGH WINDS",
                                                 "WINTER STORMS") ~ "WINTER STORM",
                                   ## WINTER WEATHER
                                   EVTYPE %in% c("WINTER WEATHER",
                                                 "WINTER WEATHER MIX",
                                                 "WINTER WEATHER/MIX",
                                                 "WINTRY MIX") ~ "WINTER WEATHER",
                                   ## OTHERS
                                   TRUE ~ EVTYPE)
                
        )
df$EVTYPE <-  str_to_sentence(df$EVTYPE)
unique(df$EVTYPE)

unique(df$PROPDMGEXP)
unique(df$CROPDMGEXP)


df <- df %>% 
    mutate( ## Re-code Property damage exponents
        PROPDMGEXP = case_when(
            PROPDMGEXP %in% c("+", "-", "0", "?", "") ~ 1,
            PROPDMGEXP %in% c("H", "h", "2") ~ 100,
            PROPDMGEXP %in% c("K", "k", "3") ~ 1000,
            PROPDMGEXP == "4" ~ 10^4,
            PROPDMGEXP == "5" ~ 10^5,
            PROPDMGEXP %in% c("M", "m", "6") ~ 10^6,
            PROPDMGEXP == "7" ~ 10^7,
            PROPDMGEXP == "B" ~ 10^9,
            TRUE ~ NA
        ),
        ## Re-code Crop damage exponent
        CROPDMGEXP = case_when(
            CROPDMGEXP %in% c("+", "-", "0", "?", "") ~ 1,
            CROPDMGEXP %in% c("H", "h", "2") ~ 100,
            CROPDMGEXP %in% c("K", "k", "3") ~ 1000,
            CROPDMGEXP == "4" ~ 10^4,
            CROPDMGEXP == "5" ~ 10^5,
            CROPDMGEXP %in% c("M", "m", "6") ~ 10^6,
            CROPDMGEXP == "7" ~ 10^7,
            CROPDMGEXP == "B" ~ 10^9,
            TRUE ~ NA
        ),
        ## create economic loss columns for property and crops
        economyLoss_property = PROPDMG*PROPDMGEXP,
        economyLoss_crop = CROPDMG * CROPDMGEXP,
        economyLoss_total = economyLoss_property + economyLoss_crop
    )

# export a csv file 
write.csv(df, "stormData.csv")
summary(df)

library(tidyr)
library(ggplot2)


# order buy total
df %>%
    group_by(EVTYPE) %>% 
    summarise(Fatalities = sum(FATALITIES, na.rm = T),
              Injuries = sum(INJURIES, na.rm = T), .groups = "drop") %>% 
    mutate(Total = Fatalities + Injuries) %>% 
    pivot_longer(cols = c(Fatalities, Injuries, Total),
                 names_to = "type",
                 values_to = "damage") %>% 
    group_by(type) %>% 
    slice_max(n = 5, order_by = damage) %>% 
    ggplot(aes(x = damage, y =tidytext::reorder_within(EVTYPE, damage, type),fill = type))+
    geom_col()+
    facet_wrap(~ type, nrow = 3, scales = "free_y")+
    tidytext::scale_y_reordered()+
    geom_text(aes(label = scales::comma(damage)), size = 3.5, hjust = -0.05)+
    scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Accent"))+
    labs(y = "Types of events", x = "Number of casualities")+
    theme_bw()+
    theme(legend.position = "")
    



# order by median
df %>%
    mutate(DmgTotal = rowSums(df[,c("FATALITIES", "INJURIES")],na.rm = T), .after = INJURIES) %>% 
    group_by(EVTYPE) %>% 
    summarise(Fatalities = median(FATALITIES, na.rm = T),
              Injuries = median(INJURIES, na.rm = T),
              Total = median(DmgTotal), .groups = "drop") %>%
    pivot_longer(cols = c(Fatalities, Injuries, Total),
                 names_to = "type",
                 values_to = "damage")  %>% 
    group_by(type) %>% 
    slice_max(n = 5, order_by = damage, with_ties = F) %>%  
    ggplot(aes(x = damage, y =tidytext::reorder_within(EVTYPE, damage, type),fill = type))+
    geom_col()+
    facet_wrap(~ type, nrow = 3, scales = "free_y")+
    tidytext::scale_y_reordered()+
    geom_text(aes(label = round(damage,2)), size = 3.5, hjust = -0.05)+
    scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Accent"))+
    labs(y = "Types of events", x = "Number of casualities")+
    theme_bw()+
    theme(legend.position = "")
        

df %>% 
    group_by(EVTYPE) %>% 
    summarise(Property = sum(economyLoss_property, na.rm = T),
              Crop = sum(economyLoss_crop, na.rm = T), .groups = "drop") %>% 
    mutate(Total = Property+Crop) %>%  
    pivot_longer(cols = c("Property","Crop","Total"),
                 names_to = "type",
                 values_to = "USD") %>% 
    group_by(type) %>% 
    slice_max(order_by = USD, n = 5) %>% 
    ungroup() %>% 
    ggplot(aes(y = USD, x = tidytext::reorder_within(EVTYPE,-USD,type), fill = type))+
    geom_col()+
    scale_y_continuous(limits = c(0,1.8e+11))+
    facet_wrap(~ type, nrow = 3, scales = "free_x")+
    tidytext::scale_x_reordered()+
    geom_text(aes(label = scales::label_dollar()(USD)), vjust = -0.5, size = 3.5)+
    scale_fill_manual(values = RColorBrewer::brewer.pal(3,"Accent"))+
    labs(y = "US Dollar", x = "Types of event")+
    theme_bw()+
    theme(legend.position = "",
          axis.text.y = element_blank(),
          axis.ticks = element_blank())

df %>% 
    group_by(EVTYPE) %>% 
    summarise(Property = sum(economyLoss_property, na.rm = T),
              Crop = sum(economyLoss_crop, na.rm = T), .groups = "drop") %>% 
    mutate(Total = Property+Crop) %>%  
    pivot_longer(cols = c("Property","Crop","Total"),
                 names_to = "type",
                 values_to = "USD") %>% 
    group_by(type) %>% 
    slice_max(order_by = USD, n = 10) %>%
    ungroup() %>% 
    mutate(USD = USD/10^9) %>% 
    pivot_wider(names_from = type, values_from = USD, values_fill = 0)
