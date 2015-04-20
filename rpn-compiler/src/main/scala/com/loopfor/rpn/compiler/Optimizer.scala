package com.loopfor.rpn.compiler

class Optimizer private () {
  def apply(codes: Seq[Code]): Seq[Code] = {
    simulate(codes)
    collapse(aggregate(codes))
  }

  private val ctors: Map[Class[_ <: ScalarCode], Int => ScalarCode] = Map(
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
            val (_cands, _revs) = inspect(_code, ctors(_code.getClass))
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
              val replace = ctors(c.getClass)(c.args + p.args - 1)
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
