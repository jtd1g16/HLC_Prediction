# load "ref3086-10061-01-5.gnu"
# chem = "cis-1,3-dichloropropene"

set terminal postscript eps color
set title "ref = 3086; chem = cis-1,3-dichloropropene; casrn = 10061-01-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5021688E-02 * exp(  -5784.632    *(1/   298.    -1/T))

set label "" at    275.1500    ,   0.2574174E-01 point
set label "" at    284.1500    ,   0.1272775E-01 point
set label "" at    293.1500    ,   0.7085464E-02 point
set label "" at    298.1500    ,   0.5021688E-02 point ps 2 pt 6

plot [270:300] H(T)
