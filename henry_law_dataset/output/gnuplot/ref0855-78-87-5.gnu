# load "ref0855-78-87-5.gnu"
# chem = "1,2-dichloropropane"

set terminal postscript eps color
set title "ref = 855; chem = 1,2-dichloropropane; casrn = 78-87-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3651576E-02 * exp(  -3827.591    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.4611826E-02 point
set label "" at    303.1500    ,   0.2867456E-02 point
set label "" at    313.1500    ,   0.2005142E-02 point
set label "" at    298.1500    ,   0.3651576E-02 point ps 2 pt 6

plot [290:320] H(T)
