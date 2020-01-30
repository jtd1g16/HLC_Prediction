# load "ref1229-104-51-8.gnu"
# chem = "butylbenzene"

set terminal postscript eps color
set title "ref = 1229; chem = butylbenzene; casrn = 104-51-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6211730E-03 * exp(  -5992.511    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.1844716E-02 point
set label "" at    288.1500    ,   0.1207984E-02 point
set label "" at    293.1500    ,   0.8972030E-03 point
set label "" at    298.1500    ,   0.5909720E-03 point
set label "" at    303.1500    ,   0.4611791E-03 point
set label "" at    298.1500    ,   0.6211730E-03 point ps 2 pt 6

plot [280:310] H(T)
