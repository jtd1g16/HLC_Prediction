# load "ref2936-6876-23-9.gnu"
# chem = "{trans}-1,2-dimethylcyclohexane"

set terminal postscript eps color
set title "ref = 2936; chem = {trans}-1,2-dimethylcyclohexane; casrn = 6876-23-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1692325E-04 * exp(  -4640.105    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.7539660E-04 point
set label "" at    278.1500    ,   0.5253962E-04 point
set label "" at    283.1500    ,   0.3758987E-04 point
set label "" at    288.1500    ,   0.2782633E-04 point
set label "" at    293.1500    ,   0.2103979E-04 point
set label "" at    298.1500    ,   0.1626595E-04 point
set label "" at    303.1500    ,   0.1288124E-04 point
set label "" at    308.1500    ,   0.1043595E-04 point
set label "" at    313.1500    ,   0.8592230E-05 point
set label "" at    298.1500    ,   0.1692325E-04 point ps 2 pt 6

plot [270:320] H(T)
