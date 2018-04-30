package nclogic.java.model.expr;

public class Next implements Expr {

    private final nclogic.model.expr.Next next;
    private final Expr e;

    public Next(Expr e) {
        this.e = e;
        next = new nclogic.model.expr.Next(e.getScalaExpr());
    }

    public Expr getE() {
        return e;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return next;
    }
}
