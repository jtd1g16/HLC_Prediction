# load "ref2477-120-12-7.gnu"
# chem = "anthracene"

set terminal postscript eps color
set title "ref = 2477; chem = anthracene; casrn = 120-12-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1771243     * exp(  -5950.248    *(1/   298.    -1/T))

set label "" at    277.2500    ,   0.8000000     point
set label "" at    284.1500    ,   0.4716981     point
set label "" at    291.1500    ,   0.2857143     point
set label "" at    298.1500    ,   0.1773050     point
set label "" at    304.1500    ,   0.1196172     point
set label "" at    298.1500    ,   0.1771243     point ps 2 pt 6

plot [270:310] H(T)
