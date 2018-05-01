package nclogic.java.model.expr;

public class Var extends Expr {

    private final nclogic.model.expr.Var neg;
    private final String name;

    public Var(String name) {
        this.name = name;
        neg = new nclogic.model.expr.Var(name);
    }

    public String getName() {
        return name;
    }

    @Override
    public nclogic.model.expr.Expr getScalaExpr() {
        return neg;
    }
}
