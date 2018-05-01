package nclogic.java.model.expr;

public class Finally extends Expr {

    private final nclogic.model.expr.Finally aFinally;

    private final Expr e;

    public Finally(Expr e) {
        this.e = e;
        aFinally = new nclogic.model.expr.Finally(e.getScalaExpr());
    }

    public Finally(nclogic.model.expr.Finally aFinally) {
        this.aFinally = aFinally;
        this.e = ToJavaExprConverter.convert(aFinally.e());
    }

    public Expr getE() {
        return e;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return aFinally ;
    }
}
