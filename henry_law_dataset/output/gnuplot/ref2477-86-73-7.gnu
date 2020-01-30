# load "ref2477-86-73-7.gnu"
# chem = "2,3-benzindene"

set terminal postscript eps color
set title "ref = 2477; chem = 2,3-benzindene; casrn = 86-73-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1018147     * exp(  -6190.876    *(1/   298.    -1/T))

set label "" at    277.2500    ,   0.4878049     point
set label "" at    284.1500    ,   0.2824859     point
set label "" at    291.1500    ,   0.1677852     point
set label "" at    298.1500    ,   0.1019368     point
set label "" at    304.1500    ,   0.6756757E-01 point
set label "" at    298.1500    ,   0.1018147     point ps 2 pt 6

plot [270:310] H(T)
