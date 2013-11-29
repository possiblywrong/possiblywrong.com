// Simple kleisli example

val checkLen: String => Option[String] = x => if (x.length > 5) None else Some(x)

val checkNum: String => Option[Int] = x => try {
   Some(x.toInt)
} catch {
  case _: java.lang.Exception => None
}

val checkMagnitude: Int => Option[Int] = x => if(x > 100) None else Some(x)

val checks = Kleisli(checkLen) >==> checkNum >==> checkMagnitude

// Very simple cokleisli example

// Please don't do this at home, every time you call get something bad happens
val zp = 1.to(100).toList.toZipper.get

// Simple FN to Sum the three adjacent values in a Zipper
val sum_adj3: PartialFunction[Zipper[Int],Int] = {
   case Zipper(Stream.Empty,f,Stream.Empty) => f
   case Zipper(Stream.Empty,f,h #:: _) => f+h
   case Zipper(h #:: _,f,Stream.Empty) => h+f
   case Zipper(h1 #:: _,f,h2 #:: _) => h1+f+h2
}

val sub_right: PartialFunction[Zipper[Int],Int] = {
   case Zipper(_,f,Stream.Empty) => f
   case Zipper(_,f,h #:: _) => f-h
}

val sum_sub = Cokleisli(sum_adj3) =>= Cokleisli(sub_right)

zp.cobind(sum_adj3)
zp.cobind(sub_right)
zp.cobind(sum_adj3).cobind(sub_right)

zp.cobind(sum_sub.run)
zp.cobind(sum_adj3).cobind(sub_right) === zp.cobind(sum_sub.run)

// List rolling average
val toAveFn: Int => PartialFunction[Zipper[Int],Int] = x => {
      case Zipper(Stream.Empty,f,Stream.Empty) => f
      case Zipper(Stream.Empty,f,r) =>
         val rs = r.take(x)
         (f+rs.sum)/(rs.length+1) |> Math.abs
      case Zipper(l,f,Stream.Empty) =>
         val ls = l.take(x)
         (f+ls.sum)/(ls.length+1) |> Math.abs
      case Zipper(l,f,r) =>
         val ls = l.take(x)
         val rs = r.take(x)
         (f+ls.sum+rs.sum)/(rs.length+ls.length+1) |> Math.abs
}
