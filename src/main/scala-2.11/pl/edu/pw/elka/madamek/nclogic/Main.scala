package pl.edu.pw.elka.madamek.nclogic

import pl.edu.pw.elka.madamek.nclogic.parser.{Parser, Tokenizer}
import pl.edu.pw.elka.madamek.nclogic.solver.DnfConverter

object Main extends App {


  val line = "(a & N(!a)) | (!a & N(a))"

  val expr = for {
    tokens <- Tokenizer.tokenize(line)
    expr <- Parser.parse(tokens)
  } yield expr


  println(DnfConverter.convertExpr(expr.get.simplify))
  val dnf = DnfConverter.convert(expr.get.simplify)


  var current = HistoryGraphFactory.create(dnf).head.successors
  for (i <-0 until 10) {
    println(current.head)
    current = current.head.successors
  }
  //
  //  val x = GraphGenerator.nextStep(Set(Neg(N(Var("a"))), N(N(Var("a")))))
  //  println(x)


  //Solver.isTautology(expr.get)
  // val cnf = Cnf(expr.get)
  //  Parser.parse(line) match {
  //    case Failure(e) => println("parse error")
  //    case Success(expr) => println(Solver.isTautology(expr))
  //  }
}

