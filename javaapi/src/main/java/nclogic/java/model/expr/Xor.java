package nclogic.java.model.expr;

public class Xor extends Expr {

    private final nclogic.model.expr.Xor xor;
    private final Expr e1;
    private final Expr e2;

    public Xor(Expr e1, Expr e2) {
        this.e1 = e1;
        this.e2 = e2;
        xor = new nclogic.model.expr.Xor(e1.getScalaExpr(), e2.getScalaExpr());
    }

    public Xor(nclogic.model.expr.Xor xor) {
        this.xor = xor;
        this.e1 = ToJavaExprConverter.convert(xor.e1());
        this.e2 = ToJavaExprConverter.convert(xor.e2());
    }

    public Expr getE1() {
        return e1;
    }

    public Expr getE2() {
        return e2;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return xor;
    }
}
