# load "ref0856-78-87-5.gnu"
# chem = "1,2-dichloropropane"

set terminal postscript eps color
set title "ref = 856; chem = 1,2-dichloropropane; casrn = 78-87-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3789776E-02 * exp(  -3793.870    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.4699635E-02 point
set label "" at    303.1500    ,   0.3084135E-02 point
set label "" at    313.1500    ,   0.2056090E-02 point
set label "" at    298.1500    ,   0.3789776E-02 point ps 2 pt 6

plot [290:320] H(T)
