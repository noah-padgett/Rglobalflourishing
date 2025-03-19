library(survey)
library(svrep)

data(api)

# survey design object
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

# Convert to generalized bootstrap replicate design
boot_dstrat <- dstrat |>
  as_gen_boot_design(
    variance_estimator = "Deville-1",
    replicates = 5000, tau = "auto"
  )

## compare 
svyglm(I(comp.imp=="Yes")~ell+meals+mobility, design=dstrat, family=quasipoisson()) |>
  summary()

svyglm(I(comp.imp=="Yes")~ell+meals+mobility, design=boot_dstrat, family=quasipoisson()) |>
  summary()
        
        