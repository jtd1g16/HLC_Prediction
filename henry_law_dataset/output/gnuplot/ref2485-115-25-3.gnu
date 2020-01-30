# load "ref2485-115-25-3.gnu"
# chem = "octafluorocyclobutane"

set terminal postscript eps color
set title "ref = 2485; chem = octafluorocyclobutane; casrn = 115-25-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1324860E-05 * exp(  -2928.703    *(1/   298.    -1/T))

set label "" at    288.2200    ,   0.1950013E-05 point
set label "" at    298.1600    ,   0.1258148E-05 point
set label "" at    308.1300    ,   0.9192270E-06 point
set label "" at    318.2300    ,   0.7498362E-06 point
set label "" at    298.1500    ,   0.1324860E-05 point ps 2 pt 6

plot [280:320] H(T)
