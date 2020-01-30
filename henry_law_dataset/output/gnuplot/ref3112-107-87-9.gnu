# load "ref3112-107-87-9.gnu"
# chem = "2-pentanone"

set terminal postscript eps color
set title "ref = 3112; chem = 2-pentanone; casrn = 107-87-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1000661     * exp(  -4636.501    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.4816528E-01 point
set label "" at    323.0000    ,   0.3042436E-01 point
set label "" at    333.0000    ,   0.1958312E-01 point
set label "" at    343.0000    ,   0.1287021E-01 point
set label "" at    353.0000    ,   0.8798396E-02 point
set label "" at    363.0000    ,   0.6368459E-02 point
set label "" at    298.1500    ,   0.1000661     point ps 2 pt 6

plot [310:370] H(T)
