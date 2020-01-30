# load "ref2445-406-90-6.gnu"
# chem = "(2,2,2-trifluoroethoxy)-ethene"

set terminal postscript eps color
set title "ref = 2445; chem = (2,2,2-trifluoroethoxy)-ethene; casrn = 406-90-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5459921E-03 * exp(  -4296.842    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.5459921E-03 point
set label "" at    310.1500    ,   0.3126245E-03 point
set label "" at    298.1500    ,   0.5459921E-03 point ps 2 pt 6

plot [290:320] H(T)
