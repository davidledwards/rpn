package com.loopfor.rpn

import scala.annotation.tailrec

object Tools {
  import Tests._

  def main(args: Array[String]): Unit = {
    parserTests(100)
//    generatorTests(100)
  }

  def parserTests(count: Int): Unit = {
    println(s"${tab(1)}val parserTests = Seq[(String, AST)](")
    for (n <- 1 to count) {
      val (expr, _) = Expression.generate()
      println(s"""${tab(2)}("$expr",""")
      (for {
        tokens <- BasicLexer(expr)
        ast <- BasicParser(tokens)
      } yield ast) map { ast =>
        def emit(ast: AST, depth: Int): Unit = {
          def printAST(name: String, l: AST, r: AST): Unit = {
            println(s"${tab(depth)}$name(")
            emit(l, depth + 1)
            println(",")
            emit(r, depth + 1)
            print(s"\n${tab(depth)})")
          }
          ast match {
            case SymbolAST(n) =>
              print(s"""${tab(depth)}SymbolAST("$n")""")
            case NumberAST(v) =>
              print(s"""${tab(depth)}NumberAST($v)""")
            case AddAST(l, r) =>
              printAST("AddAST", l, r)
            case SubtractAST(l, r) =>
              printAST("SubtractAST", l, r)
            case MultiplyAST(l, r) =>
              printAST("MultiplyAST", l, r)
            case DivideAST(l, r) =>
              printAST("DivideAST", l, r)
            case ModuloAST(l, r) =>
              printAST("ModuloAST", l, r)
            case PowerAST(base, exp) =>
              printAST("PowerAST", base, exp)
            case MinAST(l, r) =>
              printAST("MinAST", l, r)
            case MaxAST(l, r) =>
              printAST("MaxAST", l, r)
          }
        }
        emit(ast, 3)
        print(")")
      }
      if (n < count) println(",") else println(")")
    }
  }

  def generatorTests(count: Int): Unit = {
    println(s"${tab(1)}val generatorTests = Seq[(String, Seq[Code])](")
    for (n <- 1 to count) {
      val (expr, _) = Expression.generate()
      println(s"""${tab(2)}("$expr",""")
      println(s"${tab(3)}Seq(")
      (for {
        tokens <- BasicLexer(expr)
        ast <- BasicParser(tokens)
        unopt <- BasicGenerator(ast)
      } yield unopt) map { codes =>
        @tailrec def emit(codes: Seq[Code]): Unit = codes match {
          case Seq(code, rest @ _*) =>
            val s = code match {
              case c: DeclareSymbolCode =>
                s"""DeclareSymbolCode("${c.name}")"""
              case c: PushSymbolCode =>
                s"""PushSymbolCode("${c.name}")"""
              case _ =>
                s"$code"
            }
            print(s"${tab(4)}$s")
            if (rest.isEmpty) print("))") else println(",")
            emit(rest)
          case _ =>
        }
        emit(codes)
      }
      if (n < count) println(",") else println(")")
    }
  }

  private def tab(n: Int) = "  " * n
}
