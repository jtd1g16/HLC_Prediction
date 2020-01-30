# load "ref1146-50-32-8.gnu"
# chem = "benzo[a]pyrene"

set terminal postscript eps color
set title "ref = 1146; chem = benzo[a]pyrene; casrn = 50-32-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    21.60820     * exp(  -4722.586    *(1/   298.    -1/T))

set label "" at    283.1500    ,    45.45455     point
set label "" at    293.1500    ,    29.41176     point
set label "" at    308.1500    ,    13.51351     point
set label "" at    313.2500    ,    10.86957     point
set label "" at    318.1500    ,    9.090909     point
set label "" at    328.1500    ,    4.184100     point
set label "" at    298.1500    ,    21.60820     point ps 2 pt 6

plot [280:330] H(T)
