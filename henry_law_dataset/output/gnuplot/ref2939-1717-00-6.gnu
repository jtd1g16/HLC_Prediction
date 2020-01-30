# load "ref2939-1717-00-6.gnu"
# chem = "1,1-dichloro-1-fluoroethane"

set terminal postscript eps color
set title "ref = 2939; chem = 1,1-dichloro-1-fluoroethane; casrn = 1717-00-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2946889E-03 * exp(  -2783.409    *(1/   298.    -1/T))

set label "" at    298.0000    ,   0.2960770E-03 point
set label "" at    353.0000    ,   0.6908463E-04 point
set label "" at    298.1500    ,   0.2946889E-03 point ps 2 pt 6

plot [290:360] H(T)
