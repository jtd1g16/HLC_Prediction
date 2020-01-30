# load "ref3112-64-17-5.gnu"
# chem = "ethanol"

set terminal postscript eps color
set title "ref = 3112; chem = ethanol; casrn = 64-17-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.869153     * exp(  -5847.089    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.8138516     point
set label "" at    323.0000    ,   0.3843188     point
set label "" at    333.0000    ,   0.2204857     point
set label "" at    343.0000    ,   0.1448741     point
set label "" at    353.0000    ,   0.9028044E-01 point
set label "" at    363.0000    ,   0.5825464E-01 point
set label "" at    298.1500    ,    1.869153     point ps 2 pt 6

plot [310:370] H(T)
