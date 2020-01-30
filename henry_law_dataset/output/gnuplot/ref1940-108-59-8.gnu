# load "ref1940-108-59-8.gnu"
# chem = "dimethyl malonate"

set terminal postscript eps color
set title "ref = 1940; chem = dimethyl malonate; casrn = 108-59-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    37.88532     * exp(  -10958.94    *(1/   298.    -1/T))

set label "" at    283.1000    ,    256.6000     point
set label "" at    285.1000    ,    207.2539     point
set label "" at    286.0000    ,    167.7770     point
set label "" at    287.1000    ,    187.5154     point
set label "" at    288.1000    ,    167.7770     point
set label "" at    289.1000    ,    118.4308     point
set label "" at    290.1000    ,    78.95386     point
set label "" at    291.1000    ,    69.08463     point
set label "" at    292.6000    ,    88.82309     point
set label "" at    293.1000    ,    88.82309     point
set label "" at    295.1000    ,    49.34616     point
set label "" at    298.1000    ,    39.47693     point
set label "" at    298.1500    ,    37.88532     point ps 2 pt 6

plot [280:300] H(T)
