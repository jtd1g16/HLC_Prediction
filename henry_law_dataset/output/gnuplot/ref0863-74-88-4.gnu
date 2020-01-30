# load "ref0863-74-88-4.gnu"
# chem = "iodomethane"

set terminal postscript eps color
set title "ref = 863; chem = iodomethane; casrn = 74-88-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1865662E-02 * exp(  -3655.034    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.5861493E-02 point
set label "" at    283.1500    ,   0.3476568E-02 point
set label "" at    293.1500    ,   0.2259769E-02 point
set label "" at    303.1500    ,   0.1557502E-02 point
set label "" at    298.1500    ,   0.1865662E-02 point ps 2 pt 6

plot [270:310] H(T)
