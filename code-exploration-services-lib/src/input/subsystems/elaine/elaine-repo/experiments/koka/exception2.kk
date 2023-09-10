effect abort
  ctl abort(): a

effect exc
  val catch : ((f : () -> <abort|e> a, g : () -> e a) -> e a)

val hAbort = handler
  return(x)   Just(x)
  ctl abort() Nothing

fun main()
  with val catch = fn(f: () -> <abort|e> a, g: () -> e a): e a
    match hAbort(f)
      Just(x) -> x
      Nothing -> g()
  
  val r = catch {
    if False then catch { abort() } { 1 } else abort()
  } {
    catch { abort() } { 2 }
  }
  
  println(r)
