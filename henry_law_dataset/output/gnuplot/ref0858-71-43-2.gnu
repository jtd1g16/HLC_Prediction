# load "ref0858-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 858; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1758954E-02 * exp(  -2260.074    *(1/   298.    -1/T))

set label "" at    318.1600    ,   0.1096272E-02 point
set label "" at    333.1600    ,   0.8194809E-03 point
set label "" at    343.1600    ,   0.5993290E-03 point
set label "" at    353.1600    ,   0.5653305E-03 point
set label "" at    298.1500    ,   0.1758954E-02 point ps 2 pt 6

plot [310:360] H(T)
