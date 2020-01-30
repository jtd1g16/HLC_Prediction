# load "ref0764-7440-63-3.gnu"
# chem = "xenon"

set terminal postscript eps color
set title "ref = 764; chem = xenon; casrn = 7440-63-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4286827E-04 * exp(  -1920.449    *(1/   298.    -1/T))

set label "" at    285.8500    ,   0.5948385E-04 point
set label "" at    288.2500    ,   0.5425981E-04 point
set label "" at    288.6500    ,   0.5316232E-04 point
set label "" at    292.7500    ,   0.4868457E-04 point
set label "" at    303.3500    ,   0.3709510E-04 point
set label "" at    313.1500    ,   0.3042237E-04 point
set label "" at    313.6500    ,   0.2976388E-04 point
set label "" at    321.0500    ,   0.2607632E-04 point
set label "" at    332.1500    ,   0.2230096E-04 point
set label "" at    344.8500    ,   0.1922799E-04 point
set label "" at    298.1500    ,   0.4286827E-04 point ps 2 pt 6

plot [280:350] H(T)
