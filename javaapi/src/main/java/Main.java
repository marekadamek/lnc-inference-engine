import nclogic.LncInferenceEngine;
import nclogic.model.Graph;
import nclogic.model.Types;
import nclogic.parser.Parser;
import scala.collection.immutable.Set;

public class Main {

    public static void main(String[] args) {
        String formula = "(a => N(b & !a)) & (b => N(c & !b)) & (c => N(d & !c)) & ((a & !b & !c & !d) | (!a & b & !c & !d) | (!a & !b & c & !d) | (!a & !b & !c & d))";
        Types.Expr expr = Parser.parse(formula).get();
        Graph<Set<Types.Expr>> historyGraph = LncInferenceEngine.getHistoryGraph(expr);
        Set<Types.Expr> state = ((Types.And) Parser.parse("a & !b & !c & !d").get().simplify()).es();
        Set<? extends Set<Types.Expr>> successors = historyGraph.getSuccessors(state);
        System.out.println(historyGraph);
        System.out.println(successors);
        //System.out.println(LncInferenceEngine.getNegativeValuations(expr));
    }
}
