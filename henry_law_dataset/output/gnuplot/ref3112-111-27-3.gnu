# load "ref3112-111-27-3.gnu"
# chem = "1-hexanol"

set terminal postscript eps color
set title "ref = 3112; chem = 1-hexanol; casrn = 111-27-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3905202     * exp(  -5849.640    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.1537275     point
set label "" at    323.0000    ,   0.8926114E-01 point
set label "" at    333.0000    ,   0.4867362E-01 point
set label "" at    343.0000    ,   0.3007712E-01 point
set label "" at    353.0000    ,   0.1762481E-01 point
set label "" at    363.0000    ,   0.1227093E-01 point
set label "" at    298.1500    ,   0.3905202     point ps 2 pt 6

plot [310:370] H(T)
