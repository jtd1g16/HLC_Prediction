# load "ref0863-74-96-4.gnu"
# chem = "bromoethane"

set terminal postscript eps color
set title "ref = 863; chem = bromoethane; casrn = 74-96-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1324521E-02 * exp(  -3888.444    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.4447081E-02 point
set label "" at    283.1500    ,   0.2590357E-02 point
set label "" at    293.1500    ,   0.1630295E-02 point
set label "" at    303.1500    ,   0.1086863E-02 point
set label "" at    298.1500    ,   0.1324521E-02 point ps 2 pt 6

plot [270:310] H(T)
