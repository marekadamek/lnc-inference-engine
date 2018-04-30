package nclogic.java.model.expr;

public class Neg implements Expr {

    private final nclogic.model.expr.Neg neg;
    private final Expr e;

    public Neg(Expr e) {
        this.e = e;
        neg = new nclogic.model.expr.Neg(e.getScalaExpr());
    }

    public Expr getE() {
        return e;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return neg;
    }
}