# load "ref2121-811-97-2.gnu"
# chem = "1,1,1,2-tetrafluoroethane"

set terminal postscript eps color
set title "ref = 2121; chem = 1,1,1,2-tetrafluoroethane; casrn = 811-97-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1953597E-03 * exp(  -2497.207    *(1/   298.    -1/T))

set label "" at    279.1500    ,   0.3628394E-03 point
set label "" at    285.1500    ,   0.2764491E-03 point
set label "" at    295.1500    ,   0.1973847E-03 point
set label "" at    303.1500    ,   0.1807552E-03 point
set label "" at    298.1500    ,   0.1953597E-03 point ps 2 pt 6

plot [270:310] H(T)
