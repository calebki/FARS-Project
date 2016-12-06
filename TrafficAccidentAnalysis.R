data = read.csv("accident.csv")
library(dplyr)
class(data$P.DOA)
View(data)


small <- data %>% dplyr::select(V_V_CONFIG, V_TRAV_SP, V_DEFORMED, V_DR_DRINK, V_PREV_ACC, 
                                V_PREV_SUS, V_PREV_DWI, V_PREV_SPD, V_VALIGN, V_VPAVETYP, V_VSURCOND,
                                A_LGT_COND, A_FATALS, A_DRUNK_DR, A_WEATHER, A_WEATHER1, A_WEATHER2, A_WRK_ZONE, A_MAN_COLL,
                                P_AGE, P_SEX, P_INJ_SEV + P_SEAT_POS, P_AIR_BAG, P_EJECTION, P_EJ_PATH, P_DRINKING, P_DRUGS, P_LAG_HRS)

small %>% mutate(TravSpeed = readr::parse_number(V_TRAV_SP),
                 Age = readr::parse_number(P_AGE),
                 PDWI = readr::parse_number(V_PREV_DWI),
                 PSuspension = readr::parse_number(V_PREV_SUS),
                 PCrash = readr::parse_number(V_PREV_ACC),
                 PSpeed = readr:parse_number(V_PREV_SPD))




data2$dat <- readr::parse_number(data$V_PREV_SPD)
