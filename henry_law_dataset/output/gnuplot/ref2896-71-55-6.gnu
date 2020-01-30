# load "ref2896-71-55-6.gnu"
# chem = "1,1,1-trichloroethane"

set terminal postscript eps color
set title "ref = 2896; chem = 1,1,1-trichloroethane; casrn = 71-55-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6188693E-03 * exp(  -3465.274    *(1/   298.    -1/T))

set label "" at    303.1500    ,   0.5289894E-03 point
set label "" at    313.1500    ,   0.3348498E-03 point
set label "" at    323.1500    ,   0.2537065E-03 point
set label "" at    333.1500    ,   0.1853263E-03 point
set label "" at    298.1500    ,   0.6188693E-03 point ps 2 pt 6

plot [300:340] H(T)
