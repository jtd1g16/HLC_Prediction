# load "ref2477-238-84-6.gnu"
# chem = "benzo[a]fluorene"

set terminal postscript eps color
set title "ref = 2477; chem = benzo[a]fluorene; casrn = 238-84-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3702611     * exp(  -4431.500    *(1/   298.    -1/T))

set label "" at    277.2500    ,    1.136364     point
set label "" at    284.1500    ,   0.7692308     point
set label "" at    291.1500    ,   0.5291005     point
set label "" at    298.1500    ,   0.3703704     point
set label "" at    304.1500    ,   0.2762431     point
set label "" at    298.1500    ,   0.3702611     point ps 2 pt 6

plot [270:310] H(T)
