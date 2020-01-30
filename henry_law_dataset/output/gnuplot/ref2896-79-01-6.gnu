# load "ref2896-79-01-6.gnu"
# chem = "trichloroethene"

set terminal postscript eps color
set title "ref = 2896; chem = trichloroethene; casrn = 79-01-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1109932E-02 * exp(  -3923.848    *(1/   298.    -1/T))

set label "" at    303.1500    ,   0.9291384E-03 point
set label "" at    313.1500    ,   0.5542174E-03 point
set label "" at    323.1500    ,   0.4036740E-03 point
set label "" at    333.1500    ,   0.2835944E-03 point
set label "" at    298.1500    ,   0.1109932E-02 point ps 2 pt 6

plot [300:340] H(T)
