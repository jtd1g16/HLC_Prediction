# load "ref2980-110-82-7.gnu"
# chem = "cyclohexane"

set terminal postscript eps color
set title "ref = 2980; chem = cyclohexane; casrn = 110-82-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5427281E-04 * exp(  -3783.410    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.8594171E-04 point
set label "" at    298.1500    ,   0.5294623E-04 point
set label "" at    308.1500    ,   0.3538776E-04 point
set label "" at    315.6500    ,   0.2743546E-04 point
set label "" at    298.1500    ,   0.5427281E-04 point ps 2 pt 6

plot [280:320] H(T)
