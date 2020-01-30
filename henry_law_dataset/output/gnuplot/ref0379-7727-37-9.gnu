# load "ref0379-7727-37-9.gnu"
# chem = "nitrogen"

set terminal postscript eps color
set title "ref = 379; chem = nitrogen; casrn = 7727-37-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6281359E-05 * exp(  -1556.177    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1036504E-04 point
set label "" at    274.1500    ,   0.1011406E-04 point
set label "" at    275.1500    ,   0.9867485E-05 point
set label "" at    276.1500    ,   0.9629715E-05 point
set label "" at    277.1500    ,   0.9400750E-05 point
set label "" at    278.1500    ,   0.9184995E-05 point
set label "" at    279.1500    ,   0.8969240E-05 point
set label "" at    280.1500    ,   0.8762292E-05 point
set label "" at    281.1500    ,   0.8564150E-05 point
set label "" at    282.1500    ,   0.8374814E-05 point
set label "" at    283.1500    ,   0.8194284E-05 point
set label "" at    284.1500    ,   0.8026964E-05 point
set label "" at    285.1500    ,   0.7864047E-05 point
set label "" at    286.1500    ,   0.7705533E-05 point
set label "" at    287.1500    ,   0.7560229E-05 point
set label "" at    288.1500    ,   0.7419328E-05 point
set label "" at    289.1500    ,   0.7282829E-05 point
set label "" at    290.1500    ,   0.7155138E-05 point
set label "" at    291.1500    ,   0.7031849E-05 point
set label "" at    292.1500    ,   0.6912964E-05 point
set label "" at    293.1500    ,   0.6802885E-05 point
set label "" at    294.1500    ,   0.6701612E-05 point
set label "" at    295.1500    ,   0.6595936E-05 point
set label "" at    296.1500    ,   0.6494664E-05 point
set label "" at    297.1500    ,   0.6402197E-05 point
set label "" at    298.1500    ,   0.6314134E-05 point
set label "" at    299.1500    ,   0.6221668E-05 point
set label "" at    300.1500    ,   0.6138007E-05 point
set label "" at    301.1500    ,   0.6058751E-05 point
set label "" at    302.1500    ,   0.5979494E-05 point
set label "" at    303.1500    ,   0.5909043E-05 point
set label "" at    298.1500    ,   0.6281359E-05 point ps 2 pt 6

plot [270:310] H(T)
