# load "ref0862-74-86-2.gnu"
# chem = "ethyne"

set terminal postscript eps color
set title "ref = 862; chem = ethyne; casrn = 74-86-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4086924E-03 * exp(  -1996.687    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.7617470E-03 point
set label "" at    283.1500    ,   0.5768142E-03 point
set label "" at    293.1500    ,   0.4535257E-03 point
set label "" at    303.1500    ,   0.3698656E-03 point
set label "" at    298.1500    ,   0.4086924E-03 point ps 2 pt 6

plot [270:310] H(T)
