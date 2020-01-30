# load "ref0855-542-75-6.gnu"
# chem = "1,3-dichloropropene"

set terminal postscript eps color
set title "ref = 855; chem = 1,3-dichloropropene; casrn = 542-75-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6410888E-02 * exp(  -4180.214    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.8186673E-02 point
set label "" at    303.1500    ,   0.5031083E-02 point
set label "" at    313.1500    ,   0.3294161E-02 point
set label "" at    298.1500    ,   0.6410888E-02 point ps 2 pt 6

plot [290:320] H(T)
