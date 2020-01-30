# load "ref0856-75-27-4.gnu"
# chem = "bromodichloromethane"

set terminal postscript eps color
set title "ref = 856; chem = bromodichloromethane; casrn = 75-27-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4829792E-02 * exp(  -4207.007    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.6168270E-02 point
set label "" at    303.1500    ,   0.3795859E-02 point
set label "" at    313.1500    ,   0.2467308E-02 point
set label "" at    298.1500    ,   0.4829792E-02 point ps 2 pt 6

plot [290:320] H(T)
