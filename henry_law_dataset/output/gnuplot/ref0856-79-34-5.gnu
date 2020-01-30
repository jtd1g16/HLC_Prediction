# load "ref0856-79-34-5.gnu"
# chem = "1,1,2,2-tetrachloroethane"

set terminal postscript eps color
set title "ref = 856; chem = 1,1,2,2-tetrachloroethane; casrn = 79-34-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2556675E-01 * exp(  -4835.957    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.3289744E-01 point
set label "" at    303.1500    ,   0.1973847E-01 point
set label "" at    308.1500    ,   0.1644872E-01 point
set label "" at    313.1500    ,   0.1096581E-01 point
set label "" at    298.1500    ,   0.2556675E-01 point ps 2 pt 6

plot [290:320] H(T)
