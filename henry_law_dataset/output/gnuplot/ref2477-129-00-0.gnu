# load "ref2477-129-00-0.gnu"
# chem = "pyrene"

set terminal postscript eps color
set title "ref = 2477; chem = pyrene; casrn = 129-00-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5855912     * exp(  -5462.354    *(1/   298.    -1/T))

set label "" at    277.2500    ,    2.325581     point
set label "" at    284.1500    ,    1.449275     point
set label "" at    291.1500    ,   0.9090909     point
set label "" at    298.1500    ,   0.5847953     point
set label "" at    304.1500    ,   0.4081633     point
set label "" at    298.1500    ,   0.5855912     point ps 2 pt 6

plot [270:310] H(T)
