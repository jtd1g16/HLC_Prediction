# load "ref3112-71-41-0.gnu"
# chem = "1-pentanol"

set terminal postscript eps color
set title "ref = 3112; chem = 1-pentanol; casrn = 71-41-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9359014     * exp(  -6807.814    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.3458869     point
set label "" at    323.0000    ,   0.1499781     point
set label "" at    333.0000    ,   0.8102769E-01 point
set label "" at    343.0000    ,   0.4689992E-01 point
set label "" at    353.0000    ,   0.2767095E-01 point
set label "" at    363.0000    ,   0.1622930E-01 point
set label "" at    298.1500    ,   0.9359014     point ps 2 pt 6

plot [310:370] H(T)
