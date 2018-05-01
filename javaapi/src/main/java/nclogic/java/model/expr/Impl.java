package nclogic.java.model.expr;

public class Impl extends Expr {

    private final nclogic.model.expr.Impl impl;
    private final Expr e1;
    private final Expr e2;

    public Impl(Expr e1, Expr e2) {
        this.e1 = e1;
        this.e2 = e2;
        impl = new nclogic.model.expr.Impl(e1.getScalaExpr(), e2.getScalaExpr());
    }

    public Impl(nclogic.model.expr.Impl impl) {
        this.impl = impl;
        this.e1 = ToJavaExprConverter.convert(impl.e1());
        this.e2 = ToJavaExprConverter.convert(impl.e2());
    }

    public Expr getE1() {
        return e1;
    }

    public Expr getE2() {
        return e2;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return impl;
    }
}
