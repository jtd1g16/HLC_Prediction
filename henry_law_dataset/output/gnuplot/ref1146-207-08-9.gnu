# load "ref1146-207-08-9.gnu"
# chem = "benzo[k]fluoranthene"

set terminal postscript eps color
set title "ref = 1146; chem = benzo[k]fluoranthene; casrn = 207-08-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    16.84313     * exp(  -5851.747    *(1/   298.    -1/T))

set label "" at    283.1500    ,    45.45455     point
set label "" at    293.1500    ,    23.25581     point
set label "" at    308.1500    ,    9.345794     point
set label "" at    313.2500    ,    7.246377     point
set label "" at    318.1500    ,    5.050505     point
set label "" at    328.1500    ,    2.481390     point
set label "" at    298.1500    ,    16.84313     point ps 2 pt 6

plot [280:330] H(T)
