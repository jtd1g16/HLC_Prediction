# load "ref1146-34883-39-1.gnu"
# chem = "2,5-dichlorobiphenyl"

set terminal postscript eps color
set title "ref = 1146; chem = 2,5-dichlorobiphenyl; casrn = 34883-39-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2344949E-01 * exp(  -5746.771    *(1/   298.    -1/T))

set label "" at    283.5500    ,   0.6211180E-01 point
set label "" at    293.1500    ,   0.3378378E-01 point
set label "" at    303.2500    ,   0.1718213E-01 point
set label "" at    308.0500    ,   0.1216545E-01 point
set label "" at    315.2500    ,   0.8130081E-02 point
set label "" at    321.0500    ,   0.6045949E-02 point
set label "" at    298.1500    ,   0.2344949E-01 point ps 2 pt 6

plot [280:330] H(T)
