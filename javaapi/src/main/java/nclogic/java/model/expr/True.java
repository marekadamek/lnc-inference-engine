package nclogic.java.model.expr;

public class True implements Expr {

    private static final True ins = new True();

    private True() {
    }

    public static True getIns() {
        return ins;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return nclogic.model.expr.True.finallyExpr();
    }
}
