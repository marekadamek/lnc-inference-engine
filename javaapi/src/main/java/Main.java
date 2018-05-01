import nclogic.java.model.HistoryGraph;
import nclogic.java.model.expr.And;
import nclogic.java.model.expr.Change;
import nclogic.java.model.expr.Expr;
import nclogic.java.model.expr.Var;
import nclogic.java.parser.Parser;

import java.util.List;

public class Main {

    public static void main(String[] args) {
        Parser parser = new Parser();
        Expr expr = new Change(new Change(new And(new Var("a"), new Var("b"))));
        HistoryGraph graph = new HistoryGraph(expr);

        Expr from = parser.parse("a & b");
        Expr to = parser.parse("!a & !b");

        List<Expr> path = graph.findPath(from, to);

    }
}
