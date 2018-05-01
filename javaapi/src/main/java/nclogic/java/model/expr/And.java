package nclogic.java.model.expr;


import scala.collection.JavaConverters;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class And extends Expr {

    private final nclogic.model.expr.And and;
    private final Set<Expr> es;

    public And(Set<Expr> es) {
        this.es = es;
        Set<nclogic.model.expr.Expr> converted = es.stream().map(Expr::getScalaExpr).collect(Collectors.toSet());
        scala.collection.immutable.Set<nclogic.model.expr.Expr> exprSet = JavaConverters.asScalaSetConverter(converted).asScala().toSet();
        and = new nclogic.model.expr.And(exprSet);
    }

    public And(Expr... es) {
        this(new HashSet<>(Arrays.asList(es)));
    }

    public And(nclogic.model.expr.And and) {
        this.and = and;
        es = ToJavaExprConverter.convertSet(and.es());
    }

    public Set<Expr> getEs() {
        return es;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return and;
    }
}
