# load "ref2996-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2996; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1412651E-02 * exp(  -2224.378    *(1/   298.    -1/T))

set label "" at    318.1500    ,   0.8938860E-03 point
set label "" at    323.1500    ,   0.7938770E-03 point
set label "" at    333.1500    ,   0.6323468E-03 point
set label "" at    343.1500    ,   0.5286697E-03 point
set label "" at    353.1500    ,   0.4474418E-03 point
set label "" at    298.1500    ,   0.1412651E-02 point ps 2 pt 6

plot [310:360] H(T)
