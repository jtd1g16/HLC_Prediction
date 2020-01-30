# load "ref2485-75-71-8.gnu"
# chem = "dichlorodifluoromethane"

set terminal postscript eps color
set title "ref = 2485; chem = dichlorodifluoromethane; casrn = 75-71-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2941336E-04 * exp(  -2731.761    *(1/   298.    -1/T))

set label "" at    288.1900    ,   0.4101588E-04 point
set label "" at    298.1400    ,   0.2868238E-04 point
set label "" at    308.1200    ,   0.2190993E-04 point
set label "" at    318.2500    ,   0.1661705E-04 point
set label "" at    298.1500    ,   0.2941336E-04 point ps 2 pt 6

plot [280:320] H(T)
