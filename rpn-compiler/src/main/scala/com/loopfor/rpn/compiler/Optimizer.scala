package com.loopfor.rpn.compiler

import scala.annotation.tailrec

class Optimizer private () {
  def apply(codes: Seq[Code]): Seq[Code] = {
    simulate(codes)

    @tailrec def loop(codes: Seq[Code]): Seq[Code] = {
      val r = collapse(aggregate(codes))
      val q = precompute(r)
      // need better way to detect revisions other than comparing size of both sequences
      if (q.size < r.size) loop(q)
      else q
    }
    loop(codes)
  }

  private val scalarCtors: Map[Class[_ <: ScalarCode], Int => ScalarCode] = Map(
        classOf[AddCode] -> AddCode.apply _,
        classOf[SubtractCode] -> SubtractCode.apply _,
        classOf[MultiplyCode] -> MultiplyCode.apply _,
        classOf[DivideCode] -> DivideCode.apply _,
        classOf[MinCode] -> MinCode.apply _,
        classOf[MaxCode] -> MaxCode.apply _
        )

  private def aggregate(codes: Seq[Code]): Seq[Code] = {
    val (_, _, _, revs) = codes.foldLeft((0, 0, Map.empty[(String, Int), (Int, ScalarCode)], Map.empty[Int, Code])) {
      case ((pos, frame, candidates, revisions), code) =>
        lazy val eligible = candidates filter { case ((_, f), _) => f <= frame }

        def inspect(code: ScalarCode, ctor: Int => ScalarCode) = {
          (eligible get (code.op, frame)) match {
            case Some((p, c)) =>
              val replace = ctor(c.args + code.args - 1)
              (eligible - ((code.op, frame)) + ((replace.op, frame) -> (pos, replace)),
                    revisions + (p -> NopCode) + (pos -> replace))
            case None =>
              (eligible + ((code.op, frame) -> (pos, code)), revisions)
          }
        }

        val (f, c, r) = code match {
          case PushSymbolCode(_) =>
            (1, candidates, revisions)
          case PushCode(_) =>
            (1, candidates, revisions)
          case ModuloCode | PowerCode | RootCode =>
            (-1, eligible, revisions)
          case _code: ScalarCode =>
            val (_cands, _revs) = inspect(_code, scalarCtors(_code.getClass))
            (1 - _code.args, _cands, _revs)
          case _ =>
            (0, candidates, revisions)
        }

        (pos + 1, frame + f, c, r)
    }

    println("---aggregate revisions---")
    revs.toSeq.sortBy { case (k, _) => k } foreach { case (pos, code) => println(s"$pos: $code") }
    revise(codes, revs)
  }

  private def collapse(codes: Seq[Code]): Seq[Code] = {
    val (_, _, revs) = codes.foldLeft((0, Option.empty[ScalarCode], Map.empty[Int, Code])) { case ((pos, prior, revisions), code) =>
      val (_prior, _revisions) = code match {
        case c: ScalarCode =>
          prior match {
            case Some(p) if (c.op == p.op) =>
              val replace = scalarCtors(c.getClass)(c.args + p.args - 1)
              (Some(replace), revisions + (pos - 1 -> NopCode) + (pos -> replace))
            case _ =>
              (Some(c), revisions)
          }
        case _ =>
          (None, revisions)
      }
      (pos + 1, _prior, _revisions)
    }
    println("---collapse revisions---")
    revs.toSeq.sortBy { case (k, _) => k } foreach { case (pos, code) => println(s"$pos: $code") }
    revise(codes, revs)
  }

  private val ops: Map[Class[_ <: Code], (Double, Double) => Double] = Map(
        classOf[AddCode] -> { _ + _ },
        classOf[SubtractCode] -> { _ - _ },
        classOf[MultiplyCode] -> { _ * _ },
        classOf[DivideCode] -> { _ / _ },
        classOf[MinCode] -> { Math.min(_, _) },
        classOf[MaxCode] -> { Math.max(_, _) },
        ModuloCode.getClass -> { _ % _ },
        PowerCode.getClass -> { (base, exp) => Math.pow(base, exp) },
        RootCode.getClass -> { (n, exp) => Math.pow(n, 1 / exp) }
        )

  private val opCtors: Map[Class[_ <: Code], Int => Code] = Map(
        ModuloCode.getClass -> { _: Int => ModuloCode },
        PowerCode.getClass -> { _: Int => PowerCode },
        RootCode.getClass -> { _: Int => RootCode }
        ) ++ scalarCtors

  private def precompute(codes: Seq[Code]): Seq[Code] = {
    trait Value
    case class NumberValue(value: Double, pos: Int) extends Value
    case object ArbitraryValue extends Value

    def analyze(pos: Int, codes: Seq[Code], stack: Seq[Value]): Map[Int, Code] = {

      def inspect(op: Class[_ <: Code], n: Int): (Map[Int, Code], Seq[Value]) = {
        val nums = for (arg @ NumberValue(_, _) <- (stack take n).reverse) yield arg
        val revs = if (nums.size > 1) {
          // apply the operation to each number in a left-to-right manner
          val total = (for (NumberValue(v, _) <- nums) yield v) reduceLeft ops(op)
          // generate nop for all but the last push and modify last push with total
          val revs = nums.init.foldLeft(Map.empty[Int, Code]) { case (r, num) => r + (num.pos -> NopCode) } +
            (nums.last.pos -> PushCode(total))
          if (nums.size == n) {
            // when all arguments are numbers, eliminate the current op with a nop
            // since it is no longer needed
            revs + (pos -> NopCode)
          } else {
            // rewrite the op with a reduced number of arguments
            revs + (pos -> opCtors(op)(n - nums.size + 1))
          }
        } else
          Map.empty[Int, Code]
        (revs, stack drop n)
      }

      codes.headOption match {
        case Some(DeclareSymbolCode(_)) =>
          analyze(pos + 1, codes.tail, stack)
        case Some(PushSymbolCode(_)) =>
          analyze(pos + 1, codes.tail, ArbitraryValue +: stack)
        case Some(PushCode(v)) =>
          analyze(pos + 1, codes.tail, NumberValue(v, pos) +: stack)
        case Some(c: ScalarCode) =>
          val (revs, st) = inspect(c.getClass, c.args)
          if (revs.size == 0) analyze(pos + 1, codes.tail, st)
          else revs
        case Some(ModuloCode) =>
          val (revs, st) = inspect(ModuloCode.getClass, 2)
          if (revs.size == 0) analyze(pos + 1, codes.tail, st)
          else revs
        case Some(PowerCode) =>
          val (revs, st) = inspect(PowerCode.getClass, 2)
          if (revs.size == 0) analyze(pos + 1, codes.tail, st)
          else revs
        case Some(RootCode) =>
          val (revs, st) = inspect(RootCode.getClass, 2)
          if (revs.size == 0) analyze(pos + 1, codes.tail, st)
          else revs
        case None =>
          Map.empty
        case _ =>
          analyze(pos + 1, codes.tail, stack)
      }
    }

    @tailrec def loop(codes: Seq[Code]): Seq[Code] = {
      val revs = analyze(0, codes, Seq.empty)
      if (revs.isEmpty) codes
      else {
        println("---precompute revisions---")
        revs.toSeq.sortBy { case (k, _) => k } foreach { case (pos, code) => println(s"$pos: $code") }
        loop(revise(codes, revs))
      }
    }

    loop(codes)
  }

  private def revise(codes: Seq[Code], revs: Map[Int, Code]): Seq[Code] = {
    if (revs.isEmpty)
      codes
    else {
      val (_, _codes) = codes.foldLeft((0, Seq.empty[Code])) { case ((pos, revised), code) =>
        (pos + 1, (revs get pos) match {
          case Some(NopCode) => revised
          case Some(c) => c +: revised
          case None => code +: revised
        })
      }
      _codes.reverse
    }
  }

  private def simulate(codes: Seq[Code]): Unit = {
    println("---simulation---")
    ((0, 0) /: codes) { case ((p, l), c) =>
      (p + 1, c match {
        case PushSymbolCode(_) =>
          println(s"$p: ${c.repr}")
          l + 1
        case PushCode(_) =>
          println(s"$p: ${c.repr}")
          l + 1
        case AddCode(args) =>
          println(s"$p: ${c.repr} --> $l")
          l - args + 1
        case SubtractCode(args) =>
          println(s"$p: ${c.repr} --> $l")
          l - args + 1
        case MultiplyCode(args) =>
          println(s"$p: ${c.repr} --> $l")
          l - args + 1
        case DivideCode(args) =>
          println(s"$p: ${c.repr} --> $l")
          l - args + 1
        case MinCode(args) =>
          println(s"$p: ${c.repr} --> $l")
          l - args + 1
        case MaxCode(args) =>
          println(s"$p: ${c.repr} --> $l")
          l - args + 1
        case ModuloCode | PowerCode | RootCode =>
          println(s"$p: ${c.repr}")
          l - 1
        case _ =>
          println(s"$p: ${c.repr}")
          l
      })
    }
  }
}

object Optimizer {
  def apply(): Optimizer = new Optimizer
}
