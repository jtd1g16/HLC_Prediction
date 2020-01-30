# load "ref3116-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 3116; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1308287E-02 * exp(  -2137.463    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.9410289E-03 point
set label "" at    323.0000    ,   0.7438429E-03 point
set label "" at    333.0000    ,   0.6135467E-03 point
set label "" at    343.0000    ,   0.5172141E-03 point
set label "" at    298.1500    ,   0.1308287E-02 point ps 2 pt 6

plot [310:350] H(T)
