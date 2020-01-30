# load "ref2121-75-45-6.gnu"
# chem = "chlorodifluoromethane"

set terminal postscript eps color
set title "ref = 2121; chem = chlorodifluoromethane; casrn = 75-45-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2118696E-03 * exp(  -4396.278    *(1/   298.    -1/T))

set label "" at    279.1500    ,   0.6950164E-03 point
set label "" at    285.1500    ,   0.3311823E-03 point
set label "" at    295.1500    ,   0.2430845E-03 point
set label "" at    303.1500    ,   0.1684169E-03 point
set label "" at    313.1500    ,   0.1088118E-03 point
set label "" at    298.1500    ,   0.2118696E-03 point ps 2 pt 6

plot [270:320] H(T)
