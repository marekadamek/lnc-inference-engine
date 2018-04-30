package nclogic.java.parser;

import nclogic.java.model.expr.Expr;
import nclogic.java.model.expr.ToJavaConverter;
import scala.util.Try;

public class Parser {

    ToJavaConverter converter = new ToJavaConverter();

    public Expr parse(String input) {
        Try<nclogic.model.expr.Expr> result = nclogic.parser.Parser.parse(input);

        if (result.isFailure()) {
            throw new RuntimeException("parse exception");
        }

        return converter.convert(result.get());

    }
}
