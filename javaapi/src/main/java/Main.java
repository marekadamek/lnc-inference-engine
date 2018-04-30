import nclogic.java.model.expr.And;
import nclogic.java.model.expr.Change;
import nclogic.java.model.expr.Expr;
import nclogic.java.model.expr.Var;
import nclogic.java.parser.Parser;
import nclogic.model.HistoryGraph;
import nclogic.model.converters.CnfConverter;
import nclogic.sat.Sat;
import scala.collection.immutable.List;

public class Main {

    public static void main(String[] args) {
        Parser parser = new Parser();
        Expr expr = new Change(new Change(new And(new Var("a"), new Var("b"))));
        nclogic.model.expr.Expr sat = (nclogic.model.expr.Expr) Sat.solve(CnfConverter.convert(expr.getScalaExpr()));
        HistoryGraph graph = new HistoryGraph(sat);

        Expr from = parser.parse("a & b");
        Expr to = parser.parse("!a & !b");

        List<nclogic.model.expr.Expr> path = graph.findPath(from.getScalaExpr(), to.getScalaExpr());

    }
}
