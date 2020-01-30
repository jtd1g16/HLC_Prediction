# load "ref0863-56-23-5.gnu"
# chem = "tetrachloromethane"

set terminal postscript eps color
set title "ref = 863; chem = tetrachloromethane; casrn = 56-23-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3522329E-03 * exp(  -4370.319    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1424345E-02 point
set label "" at    283.1500    ,   0.7250043E-03 point
set label "" at    293.1500    ,   0.4234538E-03 point
set label "" at    303.1500    ,   0.2951345E-03 point
set label "" at    298.1500    ,   0.3522329E-03 point ps 2 pt 6

plot [270:310] H(T)
