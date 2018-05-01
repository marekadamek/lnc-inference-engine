package nclogic.java.model.expr;


import scala.collection.JavaConverters;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class Or extends Expr {

    private final nclogic.model.expr.Or or;
    private final Set<Expr> es;

    public Or(Set<Expr> es) {
        this.es = es;
        Set<nclogic.model.expr.Expr> converted = es.stream().map(Expr::getScalaExpr).collect(Collectors.toSet());
        scala.collection.immutable.Set<nclogic.model.expr.Expr> exprSet = JavaConverters.asScalaSetConverter(converted).asScala().toSet();
        or = new nclogic.model.expr.Or(exprSet);
    }

    public Or(Expr... es) {
        this(new HashSet<>(Arrays.asList(es)));
    }


    public Or(nclogic.model.expr.Or or) {
        this.or = or;
        es = ToJavaExprConverter.convertSet(or.es());
    }

    public Set<Expr> getEs() {
        return es;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return or;
    }
}
