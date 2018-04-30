package nclogic.java.model.expr;

public class Globally implements Expr {

    private final nclogic.model.expr.Globally globally;

    private final Expr e;

    public Globally(Expr e) {
        this.e = e;
        globally = new nclogic.model.expr.Globally(e.getScalaExpr());
    }

    public Expr getE() {
        return e;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return globally ;
    }
}
