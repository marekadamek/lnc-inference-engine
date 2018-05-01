package nclogic.java.model.expr;


import scala.collection.JavaConverters;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class ToJavaExprConverter {
    private ToJavaExprConverter() {

    }

    public static Expr convert(nclogic.model.expr.Expr e) {
        // And
        if (e instanceof nclogic.model.expr.And) {
            return new And((nclogic.model.expr.And) e);
        }

        // Change
        if (e instanceof nclogic.model.expr.Change) {
            return new Change((nclogic.model.expr.Change) e);
        }

        // Eq
        if (e instanceof nclogic.model.expr.Eq) {
            return new Eq((nclogic.model.expr.Eq) e);
        }

        // False
        if (e.equals(nclogic.model.expr.False.finallyExpr())) {
            return False.getIns();
        }

        // Finally
        if (e instanceof nclogic.model.expr.Finally) {
            return new Finally((nclogic.model.expr.Finally) e);
        }

        // Globally
        if (e instanceof nclogic.model.expr.Globally) {
            return new Globally((nclogic.model.expr.Globally) e);
        }

        // Impl
        if (e instanceof nclogic.model.expr.Impl) {
            return new Impl((nclogic.model.expr.Impl) e);
        }

        // Neg
        if (e instanceof nclogic.model.expr.Neg) {
            return new Neg((nclogic.model.expr.Neg) e);
        }

        // Next
        if (e instanceof nclogic.model.expr.Next) {
            return new Next((nclogic.model.expr.Next) e);
        }

        // Or
        if (e instanceof nclogic.model.expr.Or) {
            return new Or((nclogic.model.expr.Or) e);
        }

        // True
        if (e.equals(nclogic.model.expr.True.finallyExpr())) {
            return True.getIns();
        }

        // Var
        if (e instanceof nclogic.model.expr.Var) {
            return new Var(((nclogic.model.expr.Var) e).name());
        }

        // Xor
        if (e instanceof nclogic.model.expr.Xor) {
            return new Xor((nclogic.model.expr.Xor) e);
        }

        throw new IllegalArgumentException("Unsupported Expr type: " + e.getClass().getName());
    }

    public static Set<Expr> convertSet(scala.collection.Set<nclogic.model.expr.Expr> scalaSet) {
        Set<nclogic.model.expr.Expr> exprs = JavaConverters.asJavaSetConverter(scalaSet).asJava();
        return exprs.stream().map(ToJavaExprConverter::convert).collect(Collectors.toSet());
    }

    public static List<Expr> convertList(scala.collection.immutable.List<nclogic.model.expr.Expr> scalaList) {
        List<nclogic.model.expr.Expr> exprs = JavaConverters.asJavaListConverter(scalaList).asJava();
        return exprs.stream().map(ToJavaExprConverter::convert).collect(Collectors.toList());
    }
}
