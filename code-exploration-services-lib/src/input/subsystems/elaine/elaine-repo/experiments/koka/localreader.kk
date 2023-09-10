effect ask<a>
  val ask: a

effect reader
  fun local_(f: a -> a, c: () -> <ask<a>,ask<a>|e> b) : (() -> <ask<a>|e> b)

val hLocal = handler
  fun local_(f, c)
    fn()
      with val ask = f(ask)
      c()

fun local(f, c)
  local_(f, c)()

fun double(x)
  2 * x

fun main()
  with val ask = 5
  with hLocal
  println(ask + local(double, { local(double, { local(double, { ask }) }) }))
