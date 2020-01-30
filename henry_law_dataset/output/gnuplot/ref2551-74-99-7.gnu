# load "ref2551-74-99-7.gnu"
# chem = "propyne"

set terminal postscript eps color
set title "ref = 2551; chem = propyne; casrn = 74-99-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7675647E-03 * exp(  -2463.150    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1602751E-02 point
set label "" at    273.1500    ,   0.1717233E-02 point
set label "" at    303.1500    ,   0.6252490E-03 point
set label "" at    303.1500    ,   0.6736837E-03 point
set label "" at    318.1500    ,   0.4535257E-03 point
set label "" at    318.1500    ,   0.4579288E-03 point
set label "" at    333.1500    ,   0.3346403E-03 point
set label "" at    298.1500    ,   0.7675647E-03 point ps 2 pt 6

plot [270:340] H(T)
