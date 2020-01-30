# load "ref0379-124-38-9.gnu"
# chem = "carbon dioxide"

set terminal postscript eps color
set title "ref = 379; chem = carbon dioxide; casrn = 124-38-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3335816E-03 * exp(  -2605.867    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.7542616E-03 point
set label "" at    274.1500    ,   0.7247604E-03 point
set label "" at    275.1500    ,   0.6974608E-03 point
set label "" at    276.1500    ,   0.6723628E-03 point
set label "" at    277.1500    ,   0.6485857E-03 point
set label "" at    278.1500    ,   0.6270102E-03 point
set label "" at    279.1500    ,   0.6063154E-03 point
set label "" at    280.1500    ,   0.5860608E-03 point
set label "" at    281.1500    ,   0.5644853E-03 point
set label "" at    282.1500    ,   0.5446711E-03 point
set label "" at    283.1500    ,   0.5257375E-03 point
set label "" at    284.1500    ,   0.5081249E-03 point
set label "" at    285.1500    ,   0.4918332E-03 point
set label "" at    286.1500    ,   0.4768624E-03 point
set label "" at    287.1500    ,   0.4623320E-03 point
set label "" at    288.1500    ,   0.4486822E-03 point
set label "" at    289.1500    ,   0.4337114E-03 point
set label "" at    290.1500    ,   0.4209423E-03 point
set label "" at    291.1500    ,   0.4086134E-03 point
set label "" at    292.1500    ,   0.3971652E-03 point
set label "" at    293.1500    ,   0.3865976E-03 point
set label "" at    294.1500    ,   0.3760300E-03 point
set label "" at    295.1500    ,   0.3650221E-03 point
set label "" at    296.1500    ,   0.3540142E-03 point
set label "" at    297.1500    ,   0.3438869E-03 point
set label "" at    298.1500    ,   0.3342000E-03 point
set label "" at    299.1500    ,   0.3249533E-03 point
set label "" at    300.1500    ,   0.3161470E-03 point
set label "" at    301.1500    ,   0.3077810E-03 point
set label "" at    302.1500    ,   0.3002956E-03 point
set label "" at    303.1500    ,   0.2928103E-03 point
set label "" at    298.1500    ,   0.3335816E-03 point ps 2 pt 6

plot [270:310] H(T)
