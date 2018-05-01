package nclogic.java;

import nclogic.java.model.HistoryGraph;
import nclogic.java.model.expr.And;
import nclogic.java.model.expr.Change;
import nclogic.java.model.expr.Expr;
import nclogic.java.model.expr.Var;
import nclogic.java.parser.Parser;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;

public class Test1 {

    @Test
    public void test() {
        Parser parser = new Parser();
        Expr expr = new Change(new And(new Var("a"), new Var("b")));
        HistoryGraph graph = new HistoryGraph(expr);

        Expr from = parser.parse("a & b");
        Expr to = parser.parse("!a & !b");

        List<Expr> path = graph.findPath(from, to);
        assertEquals(path.get(0), from);
        assertEquals(path.get(path.size() - 1), to);
    }
}
