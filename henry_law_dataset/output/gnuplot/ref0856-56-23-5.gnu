# load "ref0856-56-23-5.gnu"
# chem = "tetrachloromethane"

set terminal postscript eps color
set title "ref = 856; chem = tetrachloromethane; casrn = 56-23-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3815689E-03 * exp(  -3644.867    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.4837859E-03 point
set label "" at    303.1500    ,   0.2928556E-03 point
set label "" at    308.1500    ,   0.2583569E-03 point
set label "" at    313.1500    ,   0.2183459E-03 point
set label "" at    298.1500    ,   0.3815689E-03 point ps 2 pt 6

plot [290:320] H(T)
