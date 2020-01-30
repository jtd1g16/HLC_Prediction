# load "ref0824-67-68-5.gnu"
# chem = "dimethylsulfoxide"

set terminal postscript eps color
set title "ref = 824; chem = dimethylsulfoxide; casrn = 67-68-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    941.9238     * exp(  -1328.864    *(1/   298.    -1/T))

set label "" at    280.1500    ,    1229.953     point
set label "" at    288.1500    ,    1151.236     point
set label "" at    294.1500    ,    975.1068     point
set label "" at    308.1500    ,    814.7209     point
set label "" at    298.1500    ,    941.9238     point ps 2 pt 6

plot [280:310] H(T)
