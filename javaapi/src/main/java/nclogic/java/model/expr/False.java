package nclogic.java.model.expr;

public class False extends Expr {

    private static final False ins = new False();

    private False() {
    }

    public static False getIns() {
        return ins;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return nclogic.model.expr.False.finallyExpr();
    }
}
