# load "ref2825-591-78-6.gnu"
# chem = "2-hexanone"

set terminal postscript eps color
set title "ref = 2825; chem = 2-hexanone; casrn = 591-78-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8628413E-01 * exp(  -5135.553    *(1/   298.    -1/T))

set label "" at    323.0000    ,   0.2256729E-01 point
set label "" at    333.0000    ,   0.1427582E-01 point
set label "" at    343.0000    ,   0.9476981E-02 point
set label "" at    353.0000    ,   0.5755319E-02 point
set label "" at    298.1500    ,   0.8628413E-01 point ps 2 pt 6

plot [320:360] H(T)
