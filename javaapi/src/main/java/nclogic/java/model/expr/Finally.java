package nclogic.java.model.expr;

public class Finally implements Expr {

    private final nclogic.model.expr.Finally aFinally;

    private final Expr e;

    public Finally(Expr e) {
        this.e = e;
        aFinally = new nclogic.model.expr.Finally(e.getScalaExpr());
    }

    public Expr getE() {
        return e;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return aFinally ;
    }
}
