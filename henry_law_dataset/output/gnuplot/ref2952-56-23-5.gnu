# load "ref2952-56-23-5.gnu"
# chem = "tetrachloromethane"

set terminal postscript eps color
set title "ref = 2952; chem = tetrachloromethane; casrn = 56-23-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3151992E-03 * exp(  -3334.573    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.5663541E-03 point
set label "" at    288.1500    ,   0.4637722E-03 point
set label "" at    293.1500    ,   0.3907389E-03 point
set label "" at    298.1500    ,   0.3103042E-03 point
set label "" at    298.1500    ,   0.3151992E-03 point ps 2 pt 6

plot [280:300] H(T)
