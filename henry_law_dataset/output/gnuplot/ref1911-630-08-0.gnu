# load "ref1911-630-08-0.gnu"
# chem = "carbon monoxide"

set terminal postscript eps color
set title "ref = 1911; chem = carbon monoxide; casrn = 630-08-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7871586E-05 * exp(  -1401.310    *(1/   298.    -1/T))

set label "" at    274.0300    ,   0.1213511E-04 point
set label "" at    279.2500    ,   0.1076573E-04 point
set label "" at    283.1900    ,   0.9995177E-05 point
set label "" at    288.4000    ,   0.9127754E-05 point
set label "" at    293.0100    ,   0.8458474E-05 point
set label "" at    298.3800    ,   0.7833225E-05 point
set label "" at    303.2000    ,   0.7410521E-05 point
set label "" at    298.1500    ,   0.7871586E-05 point ps 2 pt 6

plot [270:310] H(T)
