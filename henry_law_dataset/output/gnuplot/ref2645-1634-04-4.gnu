# load "ref2645-1634-04-4.gnu"
# chem = "methyl {tert}-butyl ether"

set terminal postscript eps color
set title "ref = 2645; chem = methyl {tert}-butyl ether; casrn = 1634-04-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1124163E-01 * exp(  -4842.440    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.3603342E-01 point
set label "" at    288.1500    ,   0.1987595E-01 point
set label "" at    298.1500    ,   0.1120543E-01 point
set label "" at    298.1500    ,   0.1124163E-01 point ps 2 pt 6

plot [270:300] H(T)
