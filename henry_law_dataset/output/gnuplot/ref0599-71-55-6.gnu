# load "ref0599-71-55-6.gnu"
# chem = "1,1,1-trichloroethane"

set terminal postscript eps color
set title "ref = 599; chem = 1,1,1-trichloroethane; casrn = 71-55-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5614239E-03 * exp(  -3169.465    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.5607519E-03 point
set label "" at    303.1500    ,   0.4527171E-03 point
set label "" at    313.1500    ,   0.3738346E-03 point
set label "" at    318.1500    ,   0.2780066E-03 point
set label "" at    323.1500    ,   0.2401273E-03 point
set label "" at    298.1500    ,   0.5614239E-03 point ps 2 pt 6

plot [290:330] H(T)
