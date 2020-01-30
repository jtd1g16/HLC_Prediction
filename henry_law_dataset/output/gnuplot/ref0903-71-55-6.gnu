# load "ref0903-71-55-6.gnu"
# chem = "1,1,1-trichloroethane"

set terminal postscript eps color
set title "ref = 903; chem = 1,1,1-trichloroethane; casrn = 71-55-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5896262E-03 * exp(  -3117.247    *(1/   298.    -1/T))

set label "" at    299.4500    ,   0.5672150E-03 point
set label "" at    308.1500    ,   0.4145937E-03 point
set label "" at    317.9500    ,   0.3094059E-03 point
set label "" at    298.1500    ,   0.5896262E-03 point ps 2 pt 6

plot [290:320] H(T)
