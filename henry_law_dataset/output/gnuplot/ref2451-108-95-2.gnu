# load "ref2451-108-95-2.gnu"
# chem = "hydroxybenzene"

set terminal postscript eps color
set title "ref = 2451; chem = hydroxybenzene; casrn = 108-95-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    80.92093     * exp(  -7397.452    *(1/   298.    -1/T))

set label "" at    313.2400    ,    29.12732     point
set label "" at    323.1500    ,    11.06838     point
set label "" at    333.1500    ,    4.941242     point
set label "" at    343.1800    ,    2.809234     point
set label "" at    353.1200    ,    1.969463     point
set label "" at    363.1400    ,   0.9971515     point
set label "" at    298.1500    ,    80.92093     point ps 2 pt 6

plot [310:370] H(T)
