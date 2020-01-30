# load "ref2939-2837-89-0.gnu"
# chem = "1-chloro-1,2,2,2-tetrafluoroethane"

set terminal postscript eps color
set title "ref = 2939; chem = 1-chloro-1,2,2,2-tetrafluoroethane; casrn = 2837-89-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1080571E-03 * exp(  -2758.730    *(1/   298.    -1/T))

set label "" at    298.0000    ,   0.1085616E-03 point
set label "" at    353.0000    ,   0.2566000E-04 point
set label "" at    298.1500    ,   0.1080571E-03 point ps 2 pt 6

plot [290:360] H(T)
