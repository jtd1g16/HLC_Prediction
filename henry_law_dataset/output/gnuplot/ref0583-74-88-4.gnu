# load "ref0583-74-88-4.gnu"
# chem = "iodomethane"

set terminal postscript eps color
set title "ref = 583; chem = iodomethane; casrn = 74-88-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1394844E-02 * exp(  -4605.546    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.5770854E-02 point
set label "" at    283.1500    ,   0.3120981E-02 point
set label "" at    293.1500    ,   0.1827509E-02 point
set label "" at    298.1500    ,   0.1394844E-02 point ps 2 pt 6

plot [270:300] H(T)
