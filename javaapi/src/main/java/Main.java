import nclogic.LncInferenceEngine;
import nclogic.model.Types;
import nclogic.parser.Parser;

public class Main {

    public static void main(String[] args) {
        Types.Expr expr = Parser.parse("N(a | b) <=> N(a) | N(b)").get();
        boolean tautology = LncInferenceEngine.isTautology(expr);
        System.out.println(tautology);
    }
}
