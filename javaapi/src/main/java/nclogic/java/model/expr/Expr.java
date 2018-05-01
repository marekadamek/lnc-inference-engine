package nclogic.java.model.expr;

public abstract class Expr {

    public abstract nclogic.model.expr.Expr getScalaExpr();

    @Override
    public boolean equals(Object o) {
        if (o instanceof Expr) {
            return this.getScalaExpr().equals(((Expr) o).getScalaExpr());
        }
        return false;
    }

    @Override
    public int hashCode() {
        return getScalaExpr().hashCode();
    }

    public Expr simplify() {
        nclogic.model.expr.Expr scalaExpr = getScalaExpr();
        return ToJavaExprConverter.convert(scalaExpr.simplify(scalaExpr.level()));
    }
}
