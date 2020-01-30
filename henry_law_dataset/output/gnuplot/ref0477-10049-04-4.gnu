# load "ref0477-10049-04-4.gnu"
# chem = "chlorine dioxide"

set terminal postscript eps color
set title "ref = 477; chem = chlorine dioxide; casrn = 10049-04-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9920185E-02 * exp(  -3334.760    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1458306E-01 point
set label "" at    293.1500    ,   0.1201601E-01 point
set label "" at    298.1500    ,   0.9956901E-02 point
set label "" at    303.1500    ,   0.8263736E-02 point
set label "" at    308.1500    ,   0.6876434E-02 point
set label "" at    298.1500    ,   0.9920185E-02 point ps 2 pt 6

plot [280:310] H(T)
