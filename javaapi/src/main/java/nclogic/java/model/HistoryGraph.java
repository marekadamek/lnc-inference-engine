package nclogic.java.model;

import nclogic.java.model.expr.Expr;
import nclogic.java.model.expr.ToJavaExprConverter;

import java.util.List;
import java.util.Set;

public class HistoryGraph {

    private final nclogic.model.HistoryGraph scalaGraph;

    public HistoryGraph(Expr formula) {
        scalaGraph = nclogic.model.HistoryGraph.fromBaseExpr(formula.getScalaExpr());
    }

    public Set<Expr> getAllNodes() {
        return ToJavaExprConverter.convertSet(scalaGraph.getAllNodes());
    }

    public Set<Expr> getMatchingNodes(Expr e) {
        return ToJavaExprConverter.convertSet(scalaGraph.getMatchingNodes(e.getScalaExpr()));
    }

    public Set<Expr> getMatchingFromNodes(Expr e) {
        return ToJavaExprConverter.convertSet(scalaGraph.getMatchingFromNodes(e.getScalaExpr()));
    }

    public Set<Expr> getSuccessors(Expr e) {
        return ToJavaExprConverter.convertSet(scalaGraph.getSuccessors(e.getScalaExpr()));
    }

    public List<Expr> findPath(Expr from, Expr to) {
        return ToJavaExprConverter.convertList(scalaGraph.findPath(from.getScalaExpr(), to.getScalaExpr()));
    }
}
