// A val effect acts as fun effect but does not need to be called
// val ask = 5
// is sort of equivalent to
// ctl ask() resume(5)
effect ask
  val ask: int

// Instead of handling the local effect, the local effect gives a function which
// does the local effect
effect local
  val local : ((f : (int) -> <ask|e> int, c : () -> <ask,ask|e> a) -> <ask|e> a)

// Two functions to use in main
fun double(x)
  2 * x

fun two_asks()
  ask + ask

fun main()
  // We have to give an initial value for ask
  with val ask = 5

  // This is the handler for local, which resumes with a function that is roughly
  // the same as the body in the elaboration.
  with val local = fn(f: int -> <ask|e> int, c: () -> <ask,ask|e> a): <ask|e> a
    with val ask = f(ask)
    c()
  println(ask + local(id, { ask }) + local(double, { two_asks() + local(double, { two_asks() } ) }))
