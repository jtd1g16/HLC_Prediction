# load "ref0863-67-66-3.gnu"
# chem = "trichloromethane"

set terminal postscript eps color
set title "ref = 863; chem = trichloromethane; casrn = 67-66-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2566630E-02 * exp(  -4645.054    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1095406E-01 point
set label "" at    283.1500    ,   0.5687845E-02 point
set label "" at    293.1500    ,   0.3282085E-02 point
set label "" at    303.1500    ,   0.2033735E-02 point
set label "" at    298.1500    ,   0.2566630E-02 point ps 2 pt 6

plot [270:310] H(T)
