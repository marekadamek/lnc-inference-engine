package nclogic.java.parser;

import nclogic.java.model.expr.Expr;
import nclogic.java.model.expr.ToJavaExprConverter;
import scala.util.Try;

public class Parser {

    public Expr parse(String input) {
        Try<nclogic.model.expr.Expr> result = nclogic.parser.Parser.parse(input);

        if (result.isFailure()) {
            throw new RuntimeException("parse exception");
        }

        return ToJavaExprConverter.convert(result.get());
    }
}
