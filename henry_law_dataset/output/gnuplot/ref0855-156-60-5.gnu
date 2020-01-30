# load "ref0855-156-60-5.gnu"
# chem = "(E)-1,2-dichloroethene"

set terminal postscript eps color
set title "ref = 855; chem = (E)-1,2-dichloroethene; casrn = 156-60-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1021056E-02 * exp(  -4043.871    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1296063E-02 point
set label "" at    303.1500    ,   0.8043882E-03 point
set label "" at    313.1500    ,   0.5373001E-03 point
set label "" at    298.1500    ,   0.1021056E-02 point ps 2 pt 6

plot [290:320] H(T)
