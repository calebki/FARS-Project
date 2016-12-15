#PER_TYPE variable == 01: indicator if the person is a driver of the motor vehicle involved 
#V_DR_DRINK: variable if the driver was drinking or not

data8$P_DOA <- data8$P_DOA[data8$P_DOA!="DOA"]
data8$P_DOA <- factor(data8$P_DOA)
