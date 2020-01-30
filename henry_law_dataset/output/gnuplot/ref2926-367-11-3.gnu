# load "ref2926-367-11-3.gnu"
# chem = "1,2-difluorobenzene"

set terminal postscript eps color
set title "ref = 2926; chem = 1,2-difluorobenzene; casrn = 367-11-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1248095E-02 * exp(  -3468.508    *(1/   298.    -1/T))

set label "" at    298.1900    ,   0.1272228E-02 point
set label "" at    308.1200    ,   0.8322092E-03 point
set label "" at    317.9000    ,   0.5931609E-03 point
set label "" at    322.8900    ,   0.5270658E-03 point
set label "" at    298.1500    ,   0.1248095E-02 point ps 2 pt 6

plot [290:330] H(T)
