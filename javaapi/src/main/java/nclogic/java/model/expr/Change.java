package nclogic.java.model.expr;

public class Change implements Expr {

    private final nclogic.model.expr.Change change;
    private final Expr e;

    public Change(Expr e) {
        this.e = e;
        change = new nclogic.model.expr.Change(e.getScalaExpr());
    }

    public Expr getE() {
        return e;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return change;
    }
}
