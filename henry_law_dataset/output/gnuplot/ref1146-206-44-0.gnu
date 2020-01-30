# load "ref1146-206-44-0.gnu"
# chem = "fluoranthene"

set terminal postscript eps color
set title "ref = 1146; chem = fluoranthene; casrn = 206-44-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.112503     * exp(  -6863.205    *(1/   298.    -1/T))

set label "" at    283.1500    ,    3.846154     point
set label "" at    293.1500    ,    1.562500     point
set label "" at    308.1500    ,   0.6134969     point
set label "" at    313.2500    ,   0.4201681     point
set label "" at    318.1500    ,   0.1712329     point
set label "" at    328.1500    ,   0.1605136     point
set label "" at    298.1500    ,    1.112503     point ps 2 pt 6

plot [280:330] H(T)
