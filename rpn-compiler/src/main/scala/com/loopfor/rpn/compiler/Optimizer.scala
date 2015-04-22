package com.loopfor.rpn.compiler

import scala.math.{max, min, pow}
import scala.annotation.tailrec

class Optimizer private () {
  def apply(codes: Seq[Code]): Seq[Code] = {
    @tailrec def optimize(codes: Seq[Code]): Seq[Code] = {
      val to = transform(codes)
      if (to.size < codes.size) optimize(to) else to
    }
    println("---original code---")
    println(Codes.format(codes))
    optimize(codes)
  }

  private val transform = (flattenDynamicScalars _) andThen (combineDynamicScalars _) andThen (evaluateLiteralExpressions _)

  private val dynamicCtors: Map[Class[_ <: DynamicScalarCode], Int => DynamicScalarCode] = Map(
        classOf[AddCode] -> AddCode.apply _,
        classOf[SubtractCode] -> SubtractCode.apply _,
        classOf[MultiplyCode] -> MultiplyCode.apply _,
        classOf[DivideCode] -> DivideCode.apply _,
        classOf[MinCode] -> MinCode.apply _,
        classOf[MaxCode] -> MaxCode.apply _
        )

  private val scalarCtors: Map[Class[_ <: Code], Int => Code] = Map(
        ModuloCode.getClass -> { _: Int => ModuloCode },
        PowerCode.getClass -> { _: Int => PowerCode }
        ) ++ dynamicCtors

  private val scalarOps: Map[Class[_ <: Code], (Double, Double) => Double] = Map(
        classOf[AddCode] -> { _ + _ },
        classOf[SubtractCode] -> { _ - _ },
        classOf[MultiplyCode] -> { _ * _ },
        classOf[DivideCode] -> { _ / _ },
        classOf[MinCode] -> { min(_, _) },
        classOf[MaxCode] -> { max(_, _) },
        ModuloCode.getClass -> { _ % _ },
        PowerCode.getClass -> { pow(_, _) }
        )

  private def flattenDynamicScalars(codes: Seq[Code]): Seq[Code] = {
    val (_, _, _, revs) = ((0, 0, Map.empty[(String, Int), (Int, DynamicScalarCode)], Map.empty[Int, Code]) /: codes) {
      case ((pos, frame, cands, revs), code) =>
        lazy val eligible = cands filter { case ((_, f), _) => f <= frame }

        def evaluate(code: DynamicScalarCode) = (eligible get (code.op, frame)) match {
          case Some((p, c)) =>
            val rep = dynamicCtors(code.getClass)(c.args + code.args - 1)
            (eligible - ((code.op, frame)) + ((rep.op, frame) -> (pos, rep)),
                  revs + (p -> NopCode) + (pos -> rep))
          case None =>
            (eligible + ((code.op, frame) -> (pos, code)), revs)
        }

        val (f, c, r) = code match {
          case PushSymbolCode(_) => (1, cands, revs)
          case PushCode(_) => (1, cands, revs)
          case c: DynamicScalarCode =>
            val (_cands, _revs) = evaluate(c)
            (1 - c.args, _cands, _revs)
          case c: ScalarCode => (1 - c.args, eligible, revs)
          case _ => (0, cands, revs)
        }
        (pos + 1, frame + f, c, r)
    }
    println("---flatten revisions---")
    printRevisions(revs)
    revise(codes, revs)
  }

  private def combineDynamicScalars(codes: Seq[Code]): Seq[Code] = {
    val (_, _, revs) = ((0, Option.empty[DynamicScalarCode], Map.empty[Int, Code]) /: codes) {
      case ((pos, prior, revs), code) =>
        val (p, r) = code match {
          case c: DynamicScalarCode =>
            prior match {
              case Some(p) if (c.op == p.op) =>
                val rep = dynamicCtors(c.getClass)(c.args + p.args - 1)
                (Some(rep), revs + (pos - 1 -> NopCode) + (pos -> rep))
              case _ => (Some(c), revs)
            }
          case _ => (None, revs)
        }
        (pos + 1, p, r)
    }
    println("---combine revisions---")
    printRevisions(revs)
    revise(codes, revs)
  }

  private def evaluateLiteralExpressions(codes: Seq[Code]): Seq[Code] = {
    trait Value
    case class NumberValue(value: Double, pos: Int) extends Value
    case object ArbitraryValue extends Value

    def analyze(pos: Int, codes: Seq[Code], stack: Seq[Value]): Map[Int, Code] = {
      def inspect(code: ScalarCode): (Map[Int, Code], Seq[Value]) = {
        val nums = for (arg @ NumberValue(_, _) <- (stack take code.args).reverse) yield arg
        val revs = if (nums.size > 1) {
          // apply the operation to each number in a left-to-right manner
          val total = (for (NumberValue(v, _) <- nums) yield v) reduceLeft scalarOps(code.getClass)
          // generate nop for all but the last push and modify last push with total
          val revs = (Map.empty[Int, Code] /: nums.init) { case (r, num) => r + (num.pos -> NopCode) } +
            (nums.last.pos -> PushCode(total))
          if (nums.size == code.args) {
            // when all arguments are numbers, eliminate the current op with a nop
            // since it is no longer needed
            revs + (pos -> NopCode)
          } else {
            // rewrite the op with a reduced number of arguments
            revs + (pos -> scalarCtors(code.getClass)(code.args - nums.size + 1))
          }
        } else
          Map.empty[Int, Code]
        (revs, stack drop code.args)
      }

      codes.headOption match {
        case Some(DeclareSymbolCode(_)) => analyze(pos + 1, codes.tail, stack)
        case Some(PushSymbolCode(_)) => analyze(pos + 1, codes.tail, ArbitraryValue +: stack)
        case Some(PushCode(v)) => analyze(pos + 1, codes.tail, NumberValue(v, pos) +: stack)
        case Some(c: ScalarCode) =>
          val (revs, s) = inspect(c)
          if (revs.size == 0) analyze(pos + 1, codes.tail, s) else revs
        case None => Map.empty
        case _ => analyze(pos + 1, codes.tail, stack)
      }
    }

    @tailrec def evaluate(codes: Seq[Code]): Seq[Code] = {
      val revs = analyze(0, codes, Seq.empty)
      if (revs.isEmpty) codes
      else {
        println("---evaluate revisions---")
        printRevisions(revs)
        evaluate(revise(codes, revs))
      }
    }
    evaluate(codes)
  }

  private def revise(codes: Seq[Code], revs: Map[Int, Code]): Seq[Code] = {
    if (revs.isEmpty)
      codes
    else {
      val (_, cs) = ((0, Seq.empty[Code]) /: codes) { case ((pos, revised), code) =>
        (pos + 1, (revs get pos) match {
          case Some(NopCode) => revised
          case Some(c) => c +: revised
          case None => code +: revised
        })
      }
      cs.reverse
    }
  }

  private def printRevisions(revs: Map[Int, Code]): Unit = {
    revs.toSeq.sortBy { case (p, _) => p } foreach { case (pos, code) => println(s"$pos: $code") }
  }
}

object Optimizer {
  def apply(): Optimizer = new Optimizer
}
