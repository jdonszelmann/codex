effect logic
  ctl or(): bool
  ctl fail(): !

val hLogic = handler
  return(x)  [x]
  ctl or()   resume(False) ++ resume(True)
  ctl fail() []

val hOnce = handler
  ctl 

effect once
  fun once(f: () -> <logic> a): (() -> <logic> a) 