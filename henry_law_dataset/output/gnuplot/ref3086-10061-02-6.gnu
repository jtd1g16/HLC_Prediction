# load "ref3086-10061-02-6.gnu"
# chem = "trans-1,3-dichloropropene"

set terminal postscript eps color
set title "ref = 3086; chem = trans-1,3-dichloropropene; casrn = 10061-02-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8068497E-02 * exp(  -5709.601    *(1/   298.    -1/T))

set label "" at    275.1500    ,   0.4056870E-01 point
set label "" at    284.1500    ,   0.2012652E-01 point
set label "" at    293.1500    ,   0.1135644E-01 point
set label "" at    298.1500    ,   0.8068497E-02 point ps 2 pt 6

plot [270:300] H(T)
