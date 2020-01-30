# load "ref0856-74-95-3.gnu"
# chem = "dibromomethane"

set terminal postscript eps color
set title "ref = 856; chem = dibromomethane; casrn = 74-95-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1116150E-01 * exp(  -4102.642    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1409890E-01 point
set label "" at    303.1500    ,   0.8972030E-02 point
set label "" at    308.1500    ,   0.7049452E-02 point
set label "" at    313.1500    ,   0.5805431E-02 point
set label "" at    298.1500    ,   0.1116150E-01 point ps 2 pt 6

plot [290:320] H(T)
