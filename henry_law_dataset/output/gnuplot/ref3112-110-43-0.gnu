# load "ref3112-110-43-0.gnu"
# chem = "2-heptanone"

set terminal postscript eps color
set title "ref = 3112; chem = 2-heptanone; casrn = 110-43-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5949847E-01 * exp(  -5307.536    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.2492879E-01 point
set label "" at    323.0000    ,   0.1524571E-01 point
set label "" at    333.0000    ,   0.9743294E-02 point
set label "" at    343.0000    ,   0.5819339E-02 point
set label "" at    353.0000    ,   0.3496015E-02 point
set label "" at    363.0000    ,   0.2546798E-02 point
set label "" at    298.1500    ,   0.5949847E-01 point ps 2 pt 6

plot [310:370] H(T)
