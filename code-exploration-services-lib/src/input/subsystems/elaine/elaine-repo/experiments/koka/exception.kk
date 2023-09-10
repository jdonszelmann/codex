effect abort
  ctl abort(): a

effect exc
  fun catch_( f : () -> <abort|e> a, g : () -> e a ) : (() -> e a)


val hAbort = handler
  return(x)   Just(x)
  ctl abort() Nothing

val hExc = handler
  fun catch_(f, g)
    fn()
      match hAbort(f)
        Just(x) -> x
        Nothing -> g()

fun catch(f, g)
  catch_(f, g)()

fun try'(f)
  f

fun main()
  with hExc
  val r = try'
    if True then abort() else 5
  .catch
    7
  println(r)

// fun main()
//   with hExc
//   println(catch(
//     { if False then catch({ abort() }, { 1 }) else abort() },
//     { catch({ abort() }, { 2 }) }
//   ))

// effect state<a>
//   fun get(): a
//   fun put(s : a) : ()

// fun hState(init, action)
//   var s := init
//   handle action
//     return(x)  (x,s)
//     fun get()  s 
//     fun put(x) s := x 

// fun order1(c): div (int, int)
//   with hState(0)
//   with hExc
//   c()

// fun order2(c): div (int, int)
//   with hExc
//   with hState(0)
//   c()

// fun computation(): <exc,state<int>> int
//   put(5)
//   catch({
//     put(6)
//     abort()
//     get()
//   }, {
//     get()
//   })

// fun printState(v: (int, int))
//   println("(" ++ v.fst.show ++ ", " ++ v.snd.show ++ ")")

// fun main()
//   printState(order1(computation))
//   printState(order2(computation))