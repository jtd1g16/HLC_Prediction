# load "ref1940-106-65-0.gnu"
# chem = "dimethyl succinate"

set terminal postscript eps color
set title "ref = 1940; chem = dimethyl succinate; casrn = 106-65-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    29.86934     * exp(  -8476.318    *(1/   298.    -1/T))

set label "" at    283.1500    ,    118.4308     point
set label "" at    284.1500    ,    108.5616     point
set label "" at    286.0500    ,    118.4308     point
set label "" at    287.1500    ,    88.82309     point
set label "" at    288.1500    ,    98.69233     point
set label "" at    289.1500    ,    78.95386     point
set label "" at    290.1500    ,    59.21540     point
set label "" at    292.1500    ,    49.34616     point
set label "" at    292.6500    ,    49.34616     point
set label "" at    295.1500    ,    39.47693     point
set label "" at    298.1500    ,    29.60770     point
set label "" at    298.1500    ,    29.86934     point ps 2 pt 6

plot [280:300] H(T)
