# load "ref1146-191-24-2.gnu"
# chem = "benzo[ghi]perylene"

set terminal postscript eps color
set title "ref = 1146; chem = benzo[ghi]perylene; casrn = 191-24-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    29.82996     * exp(  -3186.737    *(1/   298.    -1/T))

set label "" at    283.1500    ,    52.63158     point
set label "" at    293.1500    ,    37.03704     point
set label "" at    308.1500    ,    19.23077     point
set label "" at    313.2500    ,    18.51852     point
set label "" at    318.1500    ,    15.15152     point
set label "" at    328.1500    ,    11.49425     point
set label "" at    298.1500    ,    29.82996     point ps 2 pt 6

plot [280:330] H(T)
