# load "ref2551-107-00-6.gnu"
# chem = "1-butyne"

set terminal postscript eps color
set title "ref = 2551; chem = 1-butyne; casrn = 107-00-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7244346E-03 * exp(  -1925.884    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1263707E-02 point
set label "" at    303.1500    ,   0.7485375E-03 point
set label "" at    303.1500    ,   0.6120395E-03 point
set label "" at    333.1500    ,   0.3522529E-03 point
set label "" at    298.1500    ,   0.7244346E-03 point ps 2 pt 6

plot [270:340] H(T)
