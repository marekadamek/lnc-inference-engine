package nclogic.java.model.expr;

public class Eq extends Expr {

    private final nclogic.model.expr.Eq eq;
    private final Expr e1;
    private final Expr e2;

    public Eq(Expr e1, Expr e2) {
        this.e1 = e1;
        this.e2 = e2;
        eq = new nclogic.model.expr.Eq(e1.getScalaExpr(), e2.getScalaExpr());
    }

    public Eq(nclogic.model.expr.Eq eq) {
        this.eq = eq;
        this.e1 = ToJavaExprConverter.convert(eq.e1());
        this.e2 = ToJavaExprConverter.convert(eq.e2());
    }

    public Expr getE1() {
        return e1;
    }

    public Expr getE2() {
        return e2;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return eq;
    }
}
