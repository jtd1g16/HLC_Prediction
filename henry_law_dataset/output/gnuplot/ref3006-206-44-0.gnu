# load "ref3006-206-44-0.gnu"
# chem = "fluoranthene"

set terminal postscript eps color
set title "ref = 3006; chem = fluoranthene; casrn = 206-44-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.917591     * exp(  -8737.244    *(1/   298.    -1/T))

set label "" at    298.1500    ,    1.850900     point
set label "" at    308.1500    ,   0.8102769     point
set label "" at    318.1500    ,   0.2867456     point
set label "" at    328.1500    ,   0.1302163     point
set label "" at    338.1500    ,   0.6074853E-01 point
set label "" at    298.1500    ,    1.917591     point ps 2 pt 6

plot [290:340] H(T)
