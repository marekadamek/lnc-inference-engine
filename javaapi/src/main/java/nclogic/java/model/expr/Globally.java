package nclogic.java.model.expr;

public class Globally extends Expr {

    private final nclogic.model.expr.Globally globally;

    private final Expr e;

    public Globally(Expr e) {
        this.e = e;
        globally = new nclogic.model.expr.Globally(e.getScalaExpr());
    }

    public Globally(nclogic.model.expr.Globally globally) {
        this.globally = globally;
        this.e = ToJavaExprConverter.convert(globally.e());
    }

    public Expr getE() {
        return e;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return globally ;
    }
}
