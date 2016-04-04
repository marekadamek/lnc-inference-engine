import nclogic.LncInferenceEngine;
import nclogic.model.CnfConverter;
import nclogic.model.DnfConverter;
import nclogic.model.Graph;
import nclogic.model.Types;
import nclogic.parser.Parser;
import scala.collection.immutable.Set;

public class Main {

    public static void main(String[] args) {
        String formula =
                "(a => N(b & !a)) " +
                "& (b => N(c & !b)) " +
                "& (c => N(d & !c)) " +
                "& ((a & !b & !c & !d) | (!a & b & !c & !d) | (!a & !b & c & !d) | (!a & !b & !c & d))";
        Types.Expr expr = Parser.parse(formula).get();
        Graph<Set<Types.Expr>> historyGraph = LncInferenceEngine.getHistoryGraph(expr);
        Set<Types.Expr> clause = DnfConverter.convert(Parser.parse("a & !b & !c & !d").get()).head();
        Set<? extends Set<Types.Expr>> successors = historyGraph.getSuccessors(clause);
        System.out.println(historyGraph);
        System.out.println(successors);
    }
}
